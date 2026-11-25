# Tools

Python tooling for the engine, managed with [uv](https://docs.astral.sh/uv/).
Run everything from this directory; `uv run` creates the environment on first use.

## Match runner

Plays two UCI engines against each other. Each random 8-ply opening is played
twice with colours swapped. Prints Elo with a 95% interval after each game.

```sh
# from the repo root: build the two versions to compare
make EXE=bin/new
git stash && make EXE=bin/old && git stash pop

cd tools
uv run match.py ../bin/new ../bin/old --games 400 --tc 10+0.1 --concurrency 8

# stop as soon as the result is significant (SPRT, alpha = beta = 0.05)
uv run match.py ../bin/new ../bin/old --sprt 0 10 --games 20000 --tc 10+0.1
```

`--sprt 0 10` tests "the new version is at least 10 Elo stronger" against
"it is no stronger". The LLR uses the trinomial normal approximation, which is
close to what fishtest and OpenBench report for the same games.

## NNUE training

```sh
# 1. self-play data with the current engine (from the repo root)
./tools/datagen.sh ./engine data/hce 8 30000 5000 1000

# 2. pack it for training (from tools/)
uv run nnue_data.py '../data/hce/*.txt' ../data/hce

# 3. train and export the quantized network
uv run train_nnue.py ../data/hce.npz --out ../nets/net.bin --epochs 20
```

The network is `768 -> 256` per side, concatenated (side to move first),
clipped ReLU, then `512 -> 1`. Inputs are piece-square features from each
side's own point of view (see `nnue_data.py`). The loss fits
`sigmoid(eval / 400)` to `0.75 * sigmoid(score / 400) + 0.25 * result`.
Export quantizes the first layer by 255 and the output layer by 512;
`train_nnue.quantized_eval` is a reference implementation of the engine's
integer inference.
