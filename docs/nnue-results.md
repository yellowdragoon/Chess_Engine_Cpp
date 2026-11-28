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

## Round 3: v4 self-play data, CReLU vs SCReLU

8.4M more positions from v4's own games (34.5M in total, same 2% validation
split for every net below). Matches are against v4 on the current search:

| Net | Data | Activation | Val loss | vs v4 | Games |
|-----|------|------------|----------|-------|-------|
| v5s | 26.8M (as v4) | SCReLU | 0.007405 (v4: 0.007536) | **−74 ± 34** (+103 =67 −176), SPRT H0 | 346 |
| v5s, both at depth 8 | | | | **−22 ± 30** (+143 =89 −168) | 400 |
| v6s | 34.5M | SCReLU | 0.007588 | **+21 ± 14** (+760 =376 −653), SPRT H1 | 1789 |
| **v6** | 34.5M | CReLU | 0.007709 | **+61 ± 28** (+266 =86 −175), SPRT H1 | 527 |
| **v6**, clean rerun | | | | **+67 ± 31** (+193 =90 −117) | 400 |

`nets/net.bin` is now v6 (CReLU). Three lessons:

- **More data, from the latest net, is still the lever.** Adding v4's games
  was worth about +65, on top of +110 (6.7M → 19M HCE positions) and +130
  (v1's games) before it.
- **SCReLU fits better but plays worse here.** Its validation loss is lower
  every time, yet it loses at a fixed depth too, so this is not just speed.
  It is also about 35% slower (2.09M vs 3.21M nps on `bench 9`): v * v * w
  needs 32-bit products where CReLU's fit in 16 bits. Engines that gain from
  SCReLU usually pair it with a smaller QB (64) and clipped output weights so
  the product stays in 16 bits; worth trying with a bigger hidden layer.
  `make ACTIVATION=screlu NET=...` still builds it.
- **Validation loss is not Elo.** Every activation decision needs a match.

The SPRT runs above (not the reruns) shared the machine with another match,
and 10–18% of their games ended on time forfeits; the clean rerun had none
and agrees with them.

## Setup

- Network: `768 → 256` per side, `[stm, nstm] → CReLU → 1`, int16, QA 255 / QB 512
  (`ACTIVATION=screlu` builds the SCReLU variant)
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

- Round 4: self-play data from v6
- Larger hidden layer (and retry SCReLU with QB 64 there), output buckets
