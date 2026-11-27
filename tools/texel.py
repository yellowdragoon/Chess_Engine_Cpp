"""Texel tuning: fit the hand-crafted evaluation's weights to game results.

The eval (board.cpp, evaluate()) is a sum of per-piece, per-square weights,
kept as a middlegame and an endgame table and blended by game phase:

    mg = sum over white pieces of mg[piece][sq] - sum over black pieces of mg[piece][mirror(sq)]
    eg = (the same with the eg table)
    eval = (mg * phase + eg * (24 - phase)) / 24          (centipawns, White's view)

That is linear in the weights, so tuning is logistic regression: predict the
game result with sigmoid(K * eval / 400) (base 10, like Elo) and minimise the
mean squared error against the actual results (1, 0.5, 0) of self-play games.

    uv run texel.py ../data/hce/*.txt --out ../hce_params.h            # one table (mg == eg)
    uv run texel.py ../data/hce/*.txt --tapered --out ../hce_params.h  # separate mg and eg

Weights start from the current hce_params.h, so a run continues from the
hand-written values. Use --check with an engine binary to confirm the engine's
eval matches this script's integer eval exactly.
"""

import argparse
import os
import re
import subprocess
import time

import numpy as np
import torch

PIECES = "PNBRQK"
PHASE = {"N": 1, "B": 1, "R": 2, "Q": 4}
PAD = 384  # index of an always-zero weight, used to pad piece lists to 32
MAX_PIECES = 32


# --- squares ---------------------------------------------------------------
# Python uses the standard order (a1 = 0, b1 = 1, ..., h8 = 63). The engine
# uses file-major order (8 * file + rank). Black pieces use the vertically
# mirrored square (standard: sq ^ 56).

def engine_to_std(e: int) -> int:
    return ((e & 7) << 3) | (e >> 3)


# --- data ------------------------------------------------------------------

def fen_features(fen: str):
    """(indices, signs, phase) for one position. Index = piece * 64 + square
    from White's view; sign is +1 for a white piece and -1 for a black one."""
    board = fen.split(" ", 1)[0]
    idx, sgn = [], []
    phase = 0
    rank, file = 7, 0
    for ch in board:
        if ch == "/":
            rank -= 1
            file = 0
        elif ch.isdigit():
            file += int(ch)
        else:
            sq = rank * 8 + file
            upper = ch.upper()
            if ch.isupper():
                idx.append(PIECES.index(upper) * 64 + sq)
                sgn.append(1)
            else:
                idx.append(PIECES.index(upper) * 64 + (sq ^ 56))
                sgn.append(-1)
            phase += PHASE.get(upper, 0)
            file += 1
    return idx, sgn, min(phase, 24)


def load_text(paths, max_positions):
    """Parses datagen lines ("fen | white cp | white result") into arrays."""
    idx = np.full((max_positions, MAX_PIECES), PAD, dtype=np.int16)
    sgn = np.zeros((max_positions, MAX_PIECES), dtype=np.int8)
    phase = np.zeros(max_positions, dtype=np.int8)
    result = np.zeros(max_positions, dtype=np.float32)
    n = 0
    for path in paths:
        with open(path) as f:
            for line in f:
                parts = line.split("|")
                if len(parts) != 3:
                    continue  # a partial line from a file still being written
                try:
                    i, s, ph = fen_features(parts[0].strip())
                    r = float(parts[2])
                except (ValueError, IndexError):
                    continue
                if len(i) > MAX_PIECES:
                    continue
                idx[n, : len(i)] = i
                sgn[n, : len(s)] = s
                phase[n] = ph
                result[n] = r
                n += 1
                if n == max_positions:
                    return idx, sgn, phase, result
    return idx[:n], sgn[:n], phase[:n], result[:n]


def load(paths, max_positions, cache):
    if cache and os.path.exists(cache):
        d = np.load(cache)
        return d["idx"], d["sgn"], d["phase"], d["result"]
    t0 = time.time()
    arrays = load_text(paths, max_positions)
    print(f"parsed {len(arrays[3]):,} positions in {time.time() - t0:.0f}s")
    if cache:
        np.savez(cache, idx=arrays[0], sgn=arrays[1], phase=arrays[2], result=arrays[3])
    return arrays


# --- weights <-> hce_params.h ----------------------------------------------

def read_header(path):
    """Returns (mg, eg) as float arrays of 384 in standard square order."""
    text = open(path).read()
    tables = []
    for name in ("hce_mg", "hce_eg"):
        body = re.search(name + r"\[6\]\[64\]\s*=\s*\{(.*?)\};", text, re.S).group(1)
        body = re.sub(r"//[^\n]*", "", body)
        vals = [int(v) for v in re.findall(r"-?\d+", body)]
        assert len(vals) == 384, f"{name}: expected 384 values, got {len(vals)}"
        w = np.zeros(384, dtype=np.float32)
        for p in range(6):
            for e in range(64):
                w[p * 64 + engine_to_std(e)] = vals[p * 64 + e]
        tables.append(w)
    return tables


def write_header(path, mg, eg, note):
    def table(name, w):
        out = [f"static const int {name}[6][64] = {{"]
        for p in range(6):
            out.append(f"  {{ // {PIECES[p]}")
            for f in range(8):
                row = [int(w[p * 64 + engine_to_std(8 * f + r)]) for r in range(8)]
                out.append("    " + ", ".join(f"{v:5d}" for v in row) + ",")
            out.append("  },")
        out.append("};")
        return "\n".join(out)

    header = f"""// Hand-crafted evaluation weights, in centipawns from White's point of view.
// Generated by tools/texel.py; edit by retuning rather than by hand.
// {note}
//
// hce_mg[piece][square] and hce_eg[piece][square]: piece is P N B R Q K, square
// is the engine's file-major index (8 * file + rank), so each line of 8 is one
// file from rank 1 to rank 8. Material is folded into every square's value.
// mg is used in the middlegame and eg in the endgame; evaluate() blends them by
// game phase. When mg == eg the eval does not depend on phase.

#pragma once

{table("hce_mg", mg)}

{table("hce_eg", eg)}
"""
    with open(path, "w") as f:
        f.write(header)


# --- the model -------------------------------------------------------------

def evaluate(mg, eg, idx, sgn, phase):
    """White's-view eval in centipawns (float), for a batch."""
    zero = torch.zeros(1)
    mg_sum = (torch.cat([mg, zero])[idx] * sgn).sum(1)
    eg_sum = (torch.cat([eg, zero])[idx] * sgn).sum(1)
    return (mg_sum * phase + eg_sum * (24 - phase)) / 24


def predict(e, k):
    return torch.sigmoid(k * e * (np.log(10) / 400))  # = 1 / (1 + 10^(-k e / 400))


def int_eval(mg, eg, idx, sgn, phase):
    """The engine's integer eval: C++ division truncates toward zero."""
    w_mg = np.append(mg, 0).astype(np.int64)
    w_eg = np.append(eg, 0).astype(np.int64)
    m = (w_mg[idx] * sgn).sum(1)
    e = (w_eg[idx] * sgn).sum(1)
    num = m * phase + e * (24 - phase)
    return np.sign(num) * (np.abs(num) // 24)


def mean_error(mg, eg, data, k, batch=1 << 17):
    idx, sgn, phase, result = data
    total = 0.0
    with torch.no_grad():
        for i in range(0, len(result), batch):
            e = evaluate(mg, eg, idx[i : i + batch], sgn[i : i + batch], phase[i : i + batch])
            total += ((predict(e, k) - result[i : i + batch]) ** 2).sum().item()
    return total / len(result)


def fit_k(mg, eg, data):
    """Golden-section search for the K that best fits the starting weights."""
    lo, hi = 0.1, 4.0
    g = (np.sqrt(5) - 1) / 2
    for _ in range(30):
        a, b = hi - g * (hi - lo), lo + g * (hi - lo)
        if mean_error(mg, eg, data, a) < mean_error(mg, eg, data, b):
            hi = b
        else:
            lo = a
    return (lo + hi) / 2


def check_engine(engine, paths, mg, eg, count):
    """Compares the engine's `eval` with int_eval on `count` positions."""
    fens = []
    for path in paths:
        with open(path) as f:
            for line in f:
                if line.count("|") == 2:
                    fens.append(line.split("|")[0].strip())
                if len(fens) >= count:
                    break
        if len(fens) >= count:
            break
    commands = "".join(f"position fen {fen}\neval\n" for fen in fens) + "quit\n"
    out = subprocess.run([engine], input=commands, capture_output=True, text=True).stdout
    got = [int(line) for line in out.split() if re.fullmatch(r"-?\d+", line)]
    mismatches = 0
    for fen, value in zip(fens, got):
        i, s, ph = fen_features(fen)
        i = np.array([i]); s = np.array([s]); ph = np.array([ph])
        expected = int(int_eval(mg, eg, i, s, ph)[0])
        if fen.split()[1] == "b":
            expected = -expected  # the engine reports the side to move's view
        if expected != value:
            mismatches += 1
            if mismatches <= 5:
                print(f"  mismatch: engine {value}, python {expected}: {fen}")
    print(f"engine check: {len(got) - mismatches}/{len(got)} positions match")
    return mismatches == 0 and len(got) == len(fens)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("data", nargs="+", help="datagen text files")
    ap.add_argument("--start", default="../hce_params.h", help="weights to start from")
    ap.add_argument("--out", help="where to write the tuned hce_params.h")
    ap.add_argument("--tapered", action="store_true", help="tune separate middlegame and endgame tables")
    ap.add_argument("--max-positions", type=int, default=20_000_000)
    ap.add_argument("--cache", help="npz file to cache the parsed data in")
    ap.add_argument("--epochs", type=int, default=6)
    ap.add_argument("--batch", type=int, default=16384)
    ap.add_argument("--lr", type=float, default=2.0, help="Adam step size, in centipawns")
    ap.add_argument("--val", type=float, default=0.05, help="fraction held out for validation")
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--check", metavar="ENGINE", help="compare an engine's eval with this script's")
    args = ap.parse_args()
    torch.manual_seed(args.seed)

    mg0, eg0 = read_header(args.start)
    if args.check:
        ok = check_engine(args.check, args.data, mg0, eg0, 200)
        raise SystemExit(0 if ok else 1)

    idx, sgn, phase, result = load(args.data, args.max_positions, args.cache)
    n = len(result)
    perm = np.random.default_rng(args.seed).permutation(n)
    n_val = int(n * args.val)
    def take(rows):
        return (torch.from_numpy(idx[rows].astype(np.int64)), torch.from_numpy(sgn[rows].astype(np.float32)),
                torch.from_numpy(phase[rows].astype(np.float32)), torch.from_numpy(result[rows]))
    val = take(np.sort(perm[:n_val]))
    train = take(perm[n_val:])
    print(f"{n - n_val:,} training and {n_val:,} validation positions")

    mg = torch.nn.Parameter(torch.from_numpy(mg0.copy()))
    eg = torch.nn.Parameter(torch.from_numpy(eg0.copy())) if args.tapered else mg
    k = fit_k(mg.detach(), eg.detach(), val)
    start_error = mean_error(mg.detach(), eg.detach(), val, k)
    print(f"K = {k:.3f}, starting validation error {start_error:.6f}")

    params = [mg, eg] if args.tapered else [mg]
    opt = torch.optim.Adam(params, lr=args.lr)
    steps = args.epochs * ((len(train[3]) + args.batch - 1) // args.batch)
    sched = torch.optim.lr_scheduler.CosineAnnealingLR(opt, steps)
    for epoch in range(args.epochs):
        t0 = time.time()
        order = torch.randperm(len(train[3]))
        for i in range(0, len(order), args.batch):
            rows = order[i : i + args.batch]
            e = evaluate(mg, eg, train[0][rows], train[1][rows], train[2][rows])
            loss = ((predict(e, k) - train[3][rows]) ** 2).mean()
            opt.zero_grad()
            loss.backward()
            opt.step()
            sched.step()
        err = mean_error(mg.detach(), eg.detach(), val, k)
        print(f"epoch {epoch + 1}: validation error {err:.6f}  ({time.time() - t0:.0f}s)")

    mg_int = np.round(mg.detach().numpy())
    eg_int = np.round(eg.detach().numpy())
    # error of the rounded integer eval, exactly as the engine computes it
    v_idx, v_sgn, v_phase, v_res = (x.numpy() for x in val)
    ie = int_eval(mg_int, eg_int, v_idx, v_sgn.astype(np.int64), v_phase.astype(np.int64))
    int_err = float(((1 / (1 + 10 ** (-k * ie / 400)) - v_res) ** 2).mean())
    print(f"integer weights: validation error {int_err:.6f} (started at {start_error:.6f})")

    for p, name in enumerate(PIECES):
        print(f"  {name}: average mg {mg_int[p * 64:(p + 1) * 64].mean():7.1f}  eg {eg_int[p * 64:(p + 1) * 64].mean():7.1f}")
    if args.out:
        kind = "tapered (separate mg and eg)" if args.tapered else "one table (mg == eg)"
        note = (f"Texel-tuned, {kind}, on {n:,} positions; K = {k:.3f}, "
                f"validation error {start_error:.6f} -> {int_err:.6f}.")
        write_header(args.out, mg_int, eg_int, note)
        print(f"wrote {args.out}")


if __name__ == "__main__":
    main()
