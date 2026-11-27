# NNUE results

A running log of networks and how they did against the hand-crafted
evaluation (HCE). Every match uses the same search (PR #4), the match runner
in `tools/match.py`, 8+0.08 per game, and paired random 8-ply openings, on an
M1 Pro with 2 games in parallel.

| Net | Data | Epochs | Val loss | vs HCE | Games |
|-----|------|--------|----------|--------|-------|
| v0  | 1.75M HCE self-play positions | 15 | 0.00729 | **+143 ± 69** (+63 =13 −24) | 100 |
| v1  | 6.66M HCE self-play positions | 20 | 0.00717 | **+191 ± 50** (+134 =32 −34) | 200 |

Head-to-head between networks (validation losses aren't comparable across
rows because each run draws its own validation set):

| Match | Result | Games |
|-------|--------|-------|
| v2 (all 19.1M HCE positions) vs v1 | **+113 ± 38** (+174 =46 −80) | 300 |
| v4 (19.1M HCE + 7.65M v1 self-play) vs v2 | **+132 ± 38** (+176 =57 −67) | 300 |
| v3 (7.65M v1 self-play only) vs v1 | **+103 ± 37** (+164 =58 −78) | 300 |

v3 and v1 were trained on about the same amount of data; v3's came from
v1's own games instead of the HCE's. That alone is worth about 100 Elo,
which is the case for bootstrapping.

## Search speed (PR #11)

Incremental Zobrist keys, quiescence skipping quiet moves without copying
the board, and a cap on history scores (which could outgrow the capture
and killer bonuses): `bench 9` 2448 ms -> 1754 ms on the same node count
(±0.03%), and **+64 ± 30** (+185 =103 −112, 400 games) against the
previous build with the same network.

`nets/net.bin` is v4. More data has been the biggest lever so far: going
from 6.7M to 19M positions was worth about 110 Elo, and adding the v1
self-play positions about 130 more.

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

- Round 3: self-play data from v4 (running)
- Larger hidden layer, SCReLU, output buckets
