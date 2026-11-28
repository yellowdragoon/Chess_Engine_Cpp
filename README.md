# A chess engine written from scratch in C++

A UCI chess engine built up one technique at a time, from bitboard move
generation and plain alpha-beta to a modern search and an NNUE evaluation
trained on its own games. Every milestone can be rebuilt, played against in the browser and
measured against the version before it.

## Quick start

```sh
make              # builds ./engine with the NNUE eval (clang++ or g++, C++17; needs xxd)
make EVAL=hce     # the hand-written (Texel-tuned) evaluation instead
make test         # perft: move generation against published node counts
make play         # opens a board in your browser to play against it
```

`make play` and the other tools use Python via [uv](https://docs.astral.sh/uv/),
which sets up its own environment on first run.

The engine speaks [UCI](https://www.chessprogramming.org/UCI), so it also works
in any chess GUI (En Croissant, Cute Chess, Arena, ...): add `./engine` as a
UCI engine.

## What's inside

All of the engine is in `board.cpp`.

- **Board and move generation:** bitboards with magic-bitboard sliding
  attacks, copy-make, full legality including castling, en passant and
  promotions, checked by perft.
- **Search:** negamax alpha-beta with iterative deepening and time
  management, a transposition table, principal variation search, null-move
  pruning, late move reductions, quiescence search, and move ordering by
  hash move, MVV-LVA, killer moves and history.
- **Draws:** Zobrist hashing for repetition detection, plus the fifty-move
  rule.
- **Evaluation (default): NNUE.** A small neural network, 768 piece-square
  inputs seen from each side → 256 hidden units per side → 1 output, trained
  on the engine's own games (`nets/net.bin`, embedded in the binary at build
  time). The first layer is updated incrementally as moves are made and taken
  back, so it is cheap enough to call millions of times per search.
- **Evaluation (`EVAL=hce`):** material and piece-square tables, with separate
  middlegame and endgame values blended by game phase, Texel-tuned on about
  19M self-play positions (`hce_params.h`). It is kept as a baseline to
  compare against.

Besides the usual UCI commands (`uci`, `isready`, `ucinewgame`,
`position`, `go`, `setoption name Hash`, `quit`), the engine understands:

| Command | What it does |
|---|---|
| `d` | print the board, side to move, en passant square and castling rights |
| `eval` | print the static evaluation in centipawns, from the side to move's view (NNUE or HCE, whichever was built) |
| `./engine perft <depth> [fen]` | count leaf nodes, for testing move generation |
| `./engine datagen <file> <games> <nodes> <seed>` | write self-play training data |

## Version history

Every milestone is listed in [`versions.txt`](versions.txt) and tagged
`engine/<name>` on GitHub. `make versions` builds them all into `bin/`, and
`make play` lets you pick any of them as your opponent. `make ladder` plays
each version against the one before it. Its latest results (200 games per
pair at 8+0.08, from [`docs/engine-history.md`](docs/engine-history.md)):

| Version | What it added | Elo vs previous | Estimated rating |
|---|---|---|---|
| `v0-original` | Bitboards, negamax alpha-beta, quiescence, MVV-LVA ordering, material + piece-square tables | baseline | ~1540 ± 200 |
| `v1-uci` | Makefile, perft tests, UCI mode, capture-generation bug fix | +0 ± 40 | ~1540 ± 190 |
| `v2-zobrist` | Zobrist hashing, repetition and fifty-move draws | +47 ± 41 | ~1590 ± 190 |
| `v3-search` | Iterative deepening, time control, transposition table, PVS, null move, LMR, killers and history | +636 ± 161 | ~2220 ± 100 |
| `v4-texel` | Texel-tuned tapered evaluation | +210 ± 53 | ~2430 ± 90 |
| `v5-nnue` | NNUE evaluation (768 → 256×2 → 1), first trained net | +44 ± 44 | ~2480 ± 70 |
| `v6-nnue-v4` | Net v4: more data plus games from the first net | +177 ± 48 | ~2660 ± 50 |
| `v7-fast-search` | Incremental Zobrist keys, cheaper quiescence, bounded history | +80 ± 43 | **2735 ± 33** (measured) |

The estimated rating is on Stockfish's `UCI_Elo` scale. Only `v7` was
measured directly: `tools/rate.py` played it against Stockfish 19 at reduced
strength (300 games at levels 2600 and 2800, 8+0.08). Each older version's
rating is that figure minus the ladder steps above it, so the error bars add up
on the way down. Self-play gaps tend to overstate the difference against other
opponents, so the oldest versions are probably somewhat stronger than shown.
Treat all of these as rough guides, not CCRL ratings.

`v0`–`v2` predate time control, so they search to a fixed depth of 4, and the
`v3` step includes the switch to playing on the clock.

## Tools

All in [`tools/`](tools), documented in [`tools/README.md`](tools/README.md).
Run them from that directory with `uv run`.

| Tool | Make target | What it does |
|---|---|---|
| `play.py` | `make play` | Browser GUI: play any version, drag or click to move, eval bar, start from a FEN |
| `match.py` | | Engine-vs-engine matches with an Elo estimate and SPRT early stopping |
| `versions.py` | `make versions` | Builds every version in `versions.txt` from its commit |
| `ladder.py` | `make ladder` | Measures each version against the previous one |
| `texel.py` | | Tunes the hand-written evaluation's weights on self-play data |
| `nnue_data.py`, `train_nnue.py` | | Pack self-play data and train, then quantize, the NNUE network |
| `datagen.sh` | | Generates self-play positions in parallel (training data for both) |

`make test-play` runs the tools' test suite: the engine's UCI move handling,
version builds, the play server, and the browser UI in headless Chromium.

## Layout

```
board.cpp, board.h, magic.h   the engine
nets/net.bin                  the NNUE network (trained by tools/train_nnue.py)
hce_params.h                  hand-written eval weights (generated by tools/texel.py)
versions.txt                  the engine's milestones
tests/perft.sh                perft regression test (make test)
tools/                        Python tools and their tests
docs/engine-history.md        Elo of each version vs the previous
docs/nnue-results.md          how each network was trained and how it did
```
