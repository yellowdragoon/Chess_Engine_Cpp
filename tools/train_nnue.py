"""Train a small NNUE evaluation network and export it for the engine.

Architecture ("perspective" network):

    768 sparse inputs --(feature transformer, shared)--> HL accumulator per side
    [accumulator(side to move), accumulator(other side)] -> clipped ReLU -> 1 output

Loss: the network's output, squashed with sigmoid(eval / SCALE), is fitted to a
blend of the search score (also squashed) and the game result:

    target = LAMBDA * sigmoid(score / SCALE) + (1 - LAMBDA) * result

Export: int16 weights quantized with QA = 255 (feature transformer) and
QB = 512 (output layer; the output weights are small, so they need the extra precision), the same scheme the engine uses for inference.

    uv run train_nnue.py ../data/hce.npz --out ../nets/net.bin
"""

import argparse
import struct
import time

import numpy as np
import torch
import torch.nn as nn

INPUTS = 768
PAD = 768
SCALE = 400.0
QA, QB = 255, 512
FT_CLIP = 127 / QA * 2  # keeps 32 summed features inside int16 after quantization
# SCReLU: the engine sums v * v * w in int32 blocks of 8, which needs |w| <= 2047
SCRELU_OUT_MAX = 2047


class NNUE(nn.Module):
    def __init__(self, hidden: int, activation: str = "crelu"):
        super().__init__()
        self.activation = activation
        self.ft = nn.EmbeddingBag(INPUTS + 1, hidden, mode="sum", padding_idx=PAD)
        self.ft_bias = nn.Parameter(torch.zeros(hidden))
        self.out = nn.Linear(2 * hidden, 1)
        nn.init.normal_(self.ft.weight, std=0.05)
        with torch.no_grad():
            self.ft.weight[PAD].zero_()

    def forward(self, stm_feats, nstm_feats):
        stm = self.ft(stm_feats) + self.ft_bias
        nstm = self.ft(nstm_feats) + self.ft_bias
        hidden = torch.clamp(torch.cat([stm, nstm], dim=1), 0.0, 1.0)
        if self.activation == "screlu":
            hidden = hidden * hidden
        return self.out(hidden).squeeze(1)  # eval in units of SCALE centipawns

    def clip(self):
        with torch.no_grad():
            self.ft.weight.clamp_(-FT_CLIP, FT_CLIP)
            self.ft.weight[PAD].zero_()
            if self.activation == "screlu":
                self.out.weight.clamp_(-SCRELU_OUT_MAX / QB, SCRELU_OUT_MAX / QB)


def load(paths, val_fraction, seed):
    """Loads one or more .npz files. Features stay int16 (indices < 769) to keep
    memory low; batches are widened to int64 when they're used."""
    parts = {key: [] for key in ("stm_feats", "nstm_feats", "score", "result")}
    for path in paths:
        data = np.load(path)
        stm = data["stm"].astype(bool)
        white, black = data["white"], data["black"]
        # side-to-move perspective first
        parts["stm_feats"].append(np.where(stm[:, None], black, white))
        parts["nstm_feats"].append(np.where(stm[:, None], white, black))
        del white, black
        sign = np.where(stm, -1.0, 1.0).astype(np.float32)
        parts["score"].append(data["score"].astype(np.float32) * sign)
        parts["result"].append(np.where(stm, 1.0 - data["result"], data["result"]).astype(np.float32))
    arrays = [np.concatenate(parts[key]) for key in ("stm_feats", "nstm_feats", "score", "result")]
    del parts

    n = len(arrays[2])
    is_val = np.zeros(n, dtype=bool)
    is_val[np.random.default_rng(seed).choice(n, int(n * val_fraction), replace=False)] = True
    val = [torch.from_numpy(x[is_val]) for x in arrays]
    train = [torch.from_numpy(x[~is_val]) for x in arrays]
    return train, val


def batch_loss(model, batch, lam):
    stm_feats, nstm_feats, score, result = batch
    pred = torch.sigmoid(model(stm_feats.long(), nstm_feats.long()))
    target = lam * torch.sigmoid(score / SCALE) + (1 - lam) * result
    return torch.mean((pred - target) ** 2)


def export(model, path):
    """Little-endian: ft weights int16[768][HL], ft bias int16[HL],
    output weights int16[2*HL], output bias int32."""
    ft_w = model.ft.weight.detach()[:INPUTS].numpy()
    ft_b = model.ft_bias.detach().numpy()
    out_w = model.out.weight.detach().numpy().reshape(-1)
    out_b = model.out.bias.detach().numpy()[0]
    # the network predicts eval / SCALE; fold SCALE into the output layer
    q_ft_w = np.round(ft_w * QA).astype(np.int16)
    q_ft_b = np.round(ft_b * QA).astype(np.int16)
    q_out_w = np.clip(np.round(out_w * QB), -32768, 32767).astype(np.int16)
    q_out_b = np.int32(round(out_b * QA * QB))
    if model.activation == "screlu" and np.abs(q_out_w.astype(np.int32)).max() > SCRELU_OUT_MAX:
        raise ValueError(f"SCReLU output weights must stay within ±{SCRELU_OUT_MAX} for the engine's int32 sums")
    with open(path, "wb") as f:
        f.write(q_ft_w.tobytes())
        f.write(q_ft_b.tobytes())
        f.write(q_out_w.tobytes())
        f.write(struct.pack("<i", int(q_out_b)))
    print(f"exported {path}: hidden {ft_w.shape[1]}, "
          f"max |ft| {np.abs(q_ft_w).max()}, max |out| {np.abs(q_out_w).max()}")


def quantized_eval(path, hidden, white_feats, black_feats, stm, activation="crelu"):
    """Reference integer inference, identical to the engine's. Returns cp."""
    raw = open(path, "rb").read()
    o = 0
    ft_w = np.frombuffer(raw, np.int16, INPUTS * hidden, o).reshape(INPUTS, hidden).astype(np.int32); o += INPUTS * hidden * 2
    ft_b = np.frombuffer(raw, np.int16, hidden, o).astype(np.int32); o += hidden * 2
    out_w = np.frombuffer(raw, np.int16, 2 * hidden, o).astype(np.int32); o += 4 * hidden
    out_b = struct.unpack_from("<i", raw, o)[0]
    acc_w = ft_b + ft_w[white_feats].sum(0)
    acc_b = ft_b + ft_w[black_feats].sum(0)
    us, them = (acc_w, acc_b) if stm == 0 else (acc_b, acc_w)
    hidden_act = np.clip(np.concatenate([us, them]), 0, QA).astype(np.int64)
    if activation == "screlu":
        # (v * v) * w is QA * QA * QB; divide by QA (truncating, like C++) to get back to QA * QB
        total = int((hidden_act * hidden_act * out_w).sum())
        out = (total // QA if total >= 0 else -((-total) // QA)) + out_b
    else:
        out = int((hidden_act * out_w).sum()) + out_b
    # integer division truncating toward zero, like C++
    q = out * int(SCALE)
    return q // (QA * QB) if q >= 0 else -((-q) // (QA * QB))


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("data", nargs="+", help="one or more .npz files from nnue_data.py")
    ap.add_argument("--out", default="net.bin")
    ap.add_argument("--hidden", type=int, default=256)
    ap.add_argument("--activation", choices=["crelu", "screlu"], default="crelu",
                    help="clipped ReLU, or squared clipped ReLU (the engine needs NNUE_SCRELU=1)")
    ap.add_argument("--epochs", type=int, default=20)
    ap.add_argument("--batch", type=int, default=16384)
    ap.add_argument("--lr", type=float, default=1e-3)
    ap.add_argument("--lam", type=float, default=0.75, help="weight of search score vs game result")
    ap.add_argument("--val", type=float, default=0.02)
    ap.add_argument("--seed", type=int, default=0)
    ap.add_argument("--threads", type=int, default=0)
    args = ap.parse_args()

    torch.manual_seed(args.seed)
    if args.threads:
        torch.set_num_threads(args.threads)
    train, val = load(args.data, args.val, args.seed)
    n = len(train[0])
    print(f"train {n}  val {len(val[0])}  hidden {args.hidden}  activation {args.activation}")

    model = NNUE(args.hidden, args.activation)
    opt = torch.optim.Adam(model.parameters(), lr=args.lr)
    sched = torch.optim.lr_scheduler.CosineAnnealingLR(opt, T_max=args.epochs)

    best_val = float("inf")
    for epoch in range(args.epochs):
        start = time.time()
        model.train()
        perm = torch.randperm(n)
        total = 0.0
        for i in range(0, n, args.batch):
            idx = perm[i : i + args.batch]
            loss = batch_loss(model, [t[idx] for t in train], args.lam)
            opt.zero_grad()
            loss.backward()
            opt.step()
            model.clip()
            total += loss.item() * len(idx)
        sched.step()
        model.eval()
        with torch.no_grad():
            val_loss = sum(batch_loss(model, [t[i : i + 65536] for t in val], args.lam).item()
                           * len(val[0][i : i + 65536]) for i in range(0, len(val[0]), 65536)) / max(len(val[0]), 1)
        print(f"epoch {epoch + 1:3d}  train {total / n:.6f}  val {val_loss:.6f}  "
              f"lr {sched.get_last_lr()[0]:.2e}  {time.time() - start:.0f}s", flush=True)
        if val_loss < best_val:
            best_val = val_loss
            export(model, args.out)


if __name__ == "__main__":
    main()
