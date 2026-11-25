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
