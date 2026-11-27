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


class NNUE(nn.Module):
    def __init__(self, hidden: int):
        super().__init__()
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
        return self.out(hidden).squeeze(1)  # eval in units of SCALE centipawns

    def clip(self):
        with torch.no_grad():
            self.ft.weight.clamp_(-FT_CLIP, FT_CLIP)
            self.ft.weight[PAD].zero_()


def load(path, val_fraction, seed):
    data = np.load(path)
    stm = data["stm"].astype(bool)
    white = data["white"].astype(np.int64)
    black = data["black"].astype(np.int64)
    # side-to-move perspective first
    stm_feats = np.where(stm[:, None], black, white)
    nstm_feats = np.where(stm[:, None], white, black)
    sign = np.where(stm, -1.0, 1.0).astype(np.float32)
    score = data["score"].astype(np.float32) * sign
    result = np.where(stm, 1.0 - data["result"], data["result"]).astype(np.float32)

    n = len(score)
    order = np.random.default_rng(seed).permutation(n)
    n_val = int(n * val_fraction)
    tensors = [torch.from_numpy(x[order]) for x in (stm_feats, nstm_feats, score, result)]
    val = [t[:n_val] for t in tensors]
    train = [t[n_val:] for t in tensors]
    return train, val


def batch_loss(model, batch, lam):
    stm_feats, nstm_feats, score, result = batch
    pred = torch.sigmoid(model(stm_feats, nstm_feats))
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
    with open(path, "wb") as f:
        f.write(q_ft_w.tobytes())
        f.write(q_ft_b.tobytes())
        f.write(q_out_w.tobytes())
        f.write(struct.pack("<i", int(q_out_b)))
    print(f"exported {path}: hidden {ft_w.shape[1]}, "
          f"max |ft| {np.abs(q_ft_w).max()}, max |out| {np.abs(q_out_w).max()}")


def quantized_eval(path, hidden, white_feats, black_feats, stm):
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
    hidden_act = np.clip(np.concatenate([us, them]), 0, QA)
    out = int((hidden_act * out_w).sum()) + out_b
    # integer division truncating toward zero, like C++
    q = out * int(SCALE)
    return q // (QA * QB) if q >= 0 else -((-q) // (QA * QB))


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("data", help=".npz from nnue_data.py")
    ap.add_argument("--out", default="net.bin")
    ap.add_argument("--hidden", type=int, default=256)
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
    print(f"train {n}  val {len(val[0])}  hidden {args.hidden}")

    model = NNUE(args.hidden)
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
