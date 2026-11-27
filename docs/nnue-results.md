# NNUE results

A running log of networks and how they did against the hand-crafted
evaluation (HCE). Every match uses the same search (PR #4), the match runner
in `tools/match.py`, 8+0.08 per game, and paired random 8-ply openings, on an
M1 Pro with 2 games in parallel.

| Net | Data | Epochs | Val loss | vs HCE | Games |
|-----|------|--------|----------|--------|-------|
| v0  | 1.75M HCE self-play positions | 15 | 0.00729 | **+143 ± 69** (+63 =13 −24) | 100 |
| v1  | 6.66M HCE self-play positions | 20 | 0.00717 | **+191 ± 50** (+134 =32 −34) | 200 |

`nets/net.bin` is v1.

## Setup

- Network: `768 → 256` per side, `[stm, nstm] → CReLU → 1`, int16, QA 255 / QB 512
- Data: `engine datagen`, 5k soft nodes per move, quiet positions only, 8–9
  random opening plies
- Loss: `0.75 · sigmoid(score/400) + 0.25 · result`, MSE in sigmoid space,
  Adam 1e-3 with cosine decay, batch 16384
- Speed: about 23% fewer nodes per second than the HCE build (see `engine bench`)

## Reproduce

```sh
make EVAL=hce EXE=bin/hce
make EXE=bin/nnue                      # embeds nets/net.bin
cd tools && uv run match.py ../bin/nnue ../bin/hce --games 200 --tc 8+0.08 --concurrency 2 --seed 2
```

## Next

- Bootstrap: generate data with v1 and retrain (round 2)
- Train on all 19M HCE positions
- Larger hidden layer, SCReLU, output buckets
