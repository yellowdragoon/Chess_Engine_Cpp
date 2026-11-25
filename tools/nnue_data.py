"""Turn datagen text files into compact numpy arrays for training.

Input lines (from `engine datagen`):  fen | white cp score | white result

Features follow the usual "768" layout, one set per perspective:

    index = side * 384 + piece_type * 64 + square

where `side` is 0 for the perspective's own pieces and 1 for the opponent's,
`piece_type` is 0..5 for P N B R Q K, and `square` is standard a1=0, b1=1, ...
h8=63, flipped vertically (square ^ 56) for Black's perspective. So both
perspectives see "their" pieces at the bottom of the board.
"""

import glob
import os
import sys

import numpy as np

PIECES = {c: i for i, c in enumerate("PNBRQK")}
MAX_PIECES = 32
PAD = 768  # padding index, maps to a zero embedding row


def fen_features(fen: str):
    """Returns (white-perspective features, black-perspective features, stm)."""
    board, stm = fen.split(" ")[:2]
    white_feats, black_feats = [], []
    rank = 7
    file = 0
    for ch in board:
        if ch == "/":
            rank -= 1
            file = 0
        elif ch.isdigit():
            file += int(ch)
        else:
            square = rank * 8 + file
            piece_type = PIECES[ch.upper()]
            is_white = ch.isupper()
            white_feats.append((0 if is_white else 1) * 384 + piece_type * 64 + square)
            black_feats.append((1 if is_white else 0) * 384 + piece_type * 64 + (square ^ 56))
            file += 1
    return white_feats, black_feats, 0 if stm == "w" else 1


def convert(paths, out_prefix):
    lines = []
    for path in paths:
        with open(path) as f:
            lines.extend(f)
    n = len(lines)
    white = np.full((n, MAX_PIECES), PAD, dtype=np.int16)
    black = np.full((n, MAX_PIECES), PAD, dtype=np.int16)
    stm = np.zeros(n, dtype=np.int8)
    score = np.zeros(n, dtype=np.int16)
    result = np.zeros(n, dtype=np.float32)
    kept = 0
    for line in lines:
        parts = line.split("|")
        if len(parts) != 3:
            continue
        w, b, side = fen_features(parts[0].strip())
        white[kept, : len(w)] = w
        black[kept, : len(b)] = b
        stm[kept] = side
        score[kept] = int(parts[1])
        result[kept] = float(parts[2])
        kept += 1
    np.savez(out_prefix, white=white[:kept], black=black[:kept], stm=stm[:kept],
             score=score[:kept], result=result[:kept])
    print(f"{kept} positions -> {out_prefix}.npz")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit("usage: uv run nnue_data.py '<glob of datagen files>' <out prefix>")
    files = sorted(glob.glob(sys.argv[1]))
    if not files:
        sys.exit(f"no files match {sys.argv[1]}")
    os.makedirs(os.path.dirname(sys.argv[2]) or ".", exist_ok=True)
    convert(files, sys.argv[2])
