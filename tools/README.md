# Tools

Python tooling for the engine, managed with [uv](https://docs.astral.sh/uv/).
Run everything from this directory; `uv run` creates the environment on first use.

## Engine versions

Every milestone of the engine is listed in [`versions.txt`](../versions.txt):
a name, the commit to build, and what it added. Binaries are not committed
(they are built with `-march=native` for your machine); build them instead:

```sh
make versions                    # from the repo root: builds bin/v0-original, bin/v1-uci, ...
cd tools && uv run versions.py list
```

`versions.py` builds each commit from `git archive`, so your branch and working
tree are never touched. The three versions from before time control
(`v0`–`v2`) only understand `go depth`, so the GUI and the match runner search
them to the fixed depth given in `versions.txt`. They also need small patches
to run at all, in `tools/versions/` (each patch explains itself).

`make ladder` plays each version against the previous one and writes
[`docs/engine-history.md`](../docs/engine-history.md): how much Elo each
technique was worth. It only plays pairs it has not measured yet.

**When a PR changes the engine's strength,** add a line to `versions.txt`
(the merge commit can be filled in after merging), then run `make ladder`.
To give each version a git tag (`engine/<name>`), run `uv run versions.py tag`
and push the tags with `git push origin 'refs/tags/engine/*'`.

## Play against the engine

```sh
make play            # from the repo root: builds ./engine, then opens a board in the browser
```

The page lets you pick the opponent from the current build (`engine`), the
versions in `versions.txt` (with a description of each), and every other
binary in `bin/`, so saved versions like `bin/nnue-v4` or
`bin/hce-texel-B` are one click away. To save the current build as a version
you can play later, run `make EXE=bin/<name>`. To choose the list yourself:

```sh
cd tools
uv run play.py ../bin/nnue-v4 ../bin/hce-texel-B   # just these
uv run play.py ../bin --port 8001                  # every engine in a folder
```

Drag a piece or click it, then click a square. You can choose your colour and
the engine's time per move, take back, and flip the board. The eval bar and the
engine's line show its last score (White's view) and depth. To start from a
position, paste a FEN into the new-game panel. Piece art is
Colin M.L. Burnett's set (GPLv2+), loaded from lichess's repository at a pinned
commit; offline, the page falls back to Unicode pieces.

### Tests

```sh
make test-play     # from the repo root
```

Three layers, all against the real engine binary:

- `tests/test_engine_uci.py`: the engine applies castling, en passant and
  every promotion exactly as python-chess does (compares its `d` output after
  special moves and along seeded random games).
- `tests/test_play_server.py`: `play.py`'s rules, takeback, game endings,
  FEN starts, long games, switching engines, broken or crashing engines, and
  the HTTP API.
- `tests/test_play_ui.py`: headless Chromium with real mouse clicks and drags:
  moving, castling, en passant, promotion picker, playing Black, flip,
  takeback, results, engine choice, phone layout and the offline fallback.
  Piece images are stubbed, so the tests need no network.

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

## Texel tuning

`texel.py` fits the hand-crafted evaluation's weights (`hce_params.h`) to the
results of self-play games. The eval is a sum of per-piece, per-square weights,
so this is logistic regression: predict each game's result with
`1 / (1 + 10^(-K * eval / 400))`, then minimise the squared error with Adam.

```sh
# self-play positions from the current engine (see "engine datagen")
cd tools
uv run texel.py ../data/hce/*.txt --cache ../data/texel.npz --out ../hce_params.h

# separate middlegame and endgame tables, blended by game phase
uv run texel.py ../data/hce/*.txt --cache ../data/texel.npz --tapered --out ../hce_params.h

# confirm the engine's eval matches the tuner's integer eval
cd .. && make && cd tools && uv run texel.py ../data/hce/1001.txt --check ../engine
```

It starts from the weights already in `hce_params.h`, fits K once, holds out
5% of the positions for validation, and prints each piece's average value
after tuning. Lower validation error doesn't guarantee more Elo: test the new
build against the old one with `match.py --sprt`.
