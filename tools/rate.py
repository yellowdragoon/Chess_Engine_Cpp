"""Estimate an engine's rating by playing Stockfish at reduced strength.

Stockfish's UCI_LimitStrength / UCI_Elo options make it play at a chosen
rating, calibrated against the CCRL rating list. Playing it at a few levels
and converting each match score to an Elo difference gives one rating
estimate per level; the estimates are then combined, weighting each by how
precise it is.

    uv run rate.py ../bin/nnue --levels 1600 1900 2200 --games 100 --tc 8+0.08

Read the result as approximate: Stockfish calibrated UCI_Elo at a slower
time control than fast test matches, and a weakened Stockfish loses by
making deliberate mistakes, unlike a genuinely weaker engine. Levels where
one side scores (nearly) everything carry little information, so aim for
levels around where the engine scores 20-80%.
"""

import argparse
import math
import re
import shutil
import subprocess
import sys
from pathlib import Path

import chess.engine

from match import summarize

FINAL = re.compile(r"final: \+(\d+) =(\d+) -(\d+)")


def check_levels(stockfish: str, levels: list[int]) -> None:
    """Fails early if the engine has no UCI_Elo option or a level is out of its range."""
    engine = chess.engine.SimpleEngine.popen_uci(stockfish)
    try:
        option = engine.options.get("UCI_Elo")
        if option is None or "UCI_LimitStrength" not in engine.options:
            sys.exit(f"{stockfish} has no UCI_LimitStrength/UCI_Elo options; is it Stockfish?")
        bad = [lvl for lvl in levels if not option.min <= lvl <= option.max]
        if bad:
            sys.exit(f"levels {bad} are outside UCI_Elo's range {option.min}-{option.max}")
    finally:
        engine.quit()


def play_level(args, level: int) -> tuple[int, int, int]:
    """Runs match.py against Stockfish at one level; returns (wins, draws, losses)."""
    cmd = [sys.executable, str(Path(__file__).with_name("match.py")), args.engine, args.stockfish,
           "--games", str(args.games), "--tc", args.tc, "--concurrency", str(args.concurrency),
           "--seed", str(args.seed),
           "--option-b", "UCI_LimitStrength=true", "--option-b", f"UCI_Elo={level}",
           "--option-b", "Threads=1"]
    out = subprocess.run(cmd, capture_output=True, text=True)
    match = FINAL.search(out.stdout)
    if out.returncode != 0 or match is None:
        sys.exit(f"match at {level} failed:\n{out.stdout[-2000:]}\n{out.stderr[-2000:]}")
    return tuple(int(x) for x in match.groups())


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("engine", help="engine to rate")
    ap.add_argument("--stockfish", default=shutil.which("stockfish") or "stockfish",
                    help="path to a Stockfish binary (default: stockfish on PATH)")
    ap.add_argument("--levels", type=int, nargs="+", default=[1600, 1900, 2200],
                    help="UCI_Elo levels to play")
    ap.add_argument("--games", type=int, default=100, help="games per level")
    ap.add_argument("--tc", default="8+0.08")
    ap.add_argument("--concurrency", type=int, default=4)
    ap.add_argument("--seed", type=int, default=1)
    args = ap.parse_args()

    check_levels(args.stockfish, args.levels)

    weighted, weights = 0.0, 0.0
    print(f"{'level':>6}  {'result':>16}  {'score':>6}  {'estimate':>15}")
    for level in args.levels:
        w, d, l = play_level(args, level)
        n = w + d + l
        score = (w + 0.5 * d) / n
        if score in (0.0, 1.0):
            # elo_from_score clamps these to about +-2400, which would only mislead
            print(f"{level:>6}  {f'+{w} ={d} -{l}':>16}  {score:>6.1%}  {'out of range':>15}")
            continue
        diff, err = summarize(w, d, l)
        estimate = level + diff
        print(f"{level:>6}  {f'+{w} ={d} -{l}':>16}  {score:>6.1%}  {estimate:>7.0f} ± {err:<5.0f}")
        sigma = err / 1.96
        weighted += estimate / sigma ** 2
        weights += 1 / sigma ** 2

    if weights == 0:
        sys.exit("no level gave a usable score; pick levels closer to the engine's strength")
    rating = weighted / weights
    err = 1.96 / math.sqrt(weights)
    print(f"\nestimated rating: {rating:.0f} ± {err:.0f} (95%, Stockfish UCI_Elo scale, approximate)")


if __name__ == "__main__":
    main()
