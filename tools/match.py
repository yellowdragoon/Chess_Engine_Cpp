"""Play a match between two UCI engines and report Elo, optionally as an SPRT.

Each opening is played twice with colours swapped, so a lopsided opening
cancels out. Openings are random 8-ply sequences from a fixed seed, so two
runs with the same arguments use the same openings.

    uv run match.py ../engine-new ../engine-old --games 400 --tc 10+0.1
    uv run match.py ../engine-new ../engine-old --sprt 0 10 --tc 10+0.1
"""

import argparse
import math
import random
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

import chess
import chess.engine


def random_opening(rng: random.Random, plies: int) -> list[chess.Move]:
    """A random legal move sequence that doesn't end the game."""
    while True:
        board = chess.Board()
        moves = []
        for _ in range(plies):
            legal = list(board.legal_moves)
            if not legal:
                break
            move = rng.choice(legal)
            board.push(move)
            moves.append(move)
        if len(moves) == plies and not board.is_game_over():
            return moves


def parse_tc(tc: str) -> tuple[float, float]:
    base, _, inc = tc.partition("+")
    return float(base), float(inc or 0)


def play_game(engines, white_idx, opening, tc, hash_mb):
    """Returns (result from engine 0's perspective: 1, 0.5 or 0, reason)."""
    base, inc = parse_tc(tc)
    board = chess.Board()
    for move in opening:
        board.push(move)
    clocks = [base, base]  # indexed by engine
    game_id = object()  # a new object per game makes python-chess send ucinewgame
    for engine in engines:
        if "Hash" in engine.options:
            engine.configure({"Hash": hash_mb})

    for ply in range(600):
        if board.is_game_over(claim_draw=True):
            break
        side_engine = white_idx if board.turn == chess.WHITE else 1 - white_idx
        w_engine, b_engine = white_idx, 1 - white_idx
        limit = chess.engine.Limit(
            white_clock=clocks[w_engine], black_clock=clocks[b_engine],
            white_inc=inc, black_inc=inc)
        start = time.monotonic()
        result = engines[side_engine].play(board, limit, game=game_id)
        clocks[side_engine] -= time.monotonic() - start
        if clocks[side_engine] < 0:
            return (0.0 if side_engine == 0 else 1.0), "time forfeit"
        clocks[side_engine] += inc
        if result.move is None or result.move not in board.legal_moves:
            return (0.0 if side_engine == 0 else 1.0), f"illegal move {result.move}"
        board.push(result.move)
    else:
        return 0.5, "move limit"

    outcome = board.outcome(claim_draw=True)
    if outcome.winner is None:
        return 0.5, outcome.termination.name.lower()
    winner_engine = white_idx if outcome.winner == chess.WHITE else 1 - white_idx
    return (1.0 if winner_engine == 0 else 0.0), outcome.termination.name.lower()


def parse_options(pairs: list[str]) -> dict[str, str]:
    """["UCI_Elo=1800", ...] -> {"UCI_Elo": "1800"}; python-chess converts the types."""
    options = {}
    for pair in pairs:
        name, sep, value = pair.partition("=")
        if not sep:
            raise SystemExit(f"bad option {pair!r}, expected NAME=VALUE")
        options[name] = value
    return options


def elo_from_score(score: float) -> float:
    score = min(max(score, 1e-6), 1 - 1e-6)
    return -400 * math.log10(1 / score - 1)


def score_from_elo(elo: float) -> float:
    return 1 / (1 + 10 ** (-elo / 400))


def summarize(w, d, l):
    n = w + d + l
    if n == 0:
        return 0.0, 0.0
    score = (w + 0.5 * d) / n
    var = (w * (1 - score) ** 2 + d * (0.5 - score) ** 2 + l * score ** 2) / n
    stderr = math.sqrt(var / n) if var > 0 else 0.0
    elo = elo_from_score(score)
    lo, hi = elo_from_score(score - 1.96 * stderr), elo_from_score(score + 1.96 * stderr)
    return elo, (hi - lo) / 2


def llr(w, d, l, elo0, elo1):
    """Log-likelihood ratio of H1 (elo1) vs H0 (elo0), trinomial normal approximation."""
    n = w + d + l
    if n == 0 or w == 0 or l == 0:
        return 0.0
    score = (w + 0.5 * d) / n
    var = (w * (1 - score) ** 2 + d * (0.5 - score) ** 2 + l * score ** 2) / n
    s0, s1 = score_from_elo(elo0), score_from_elo(elo1)
    return (s1 - s0) * (2 * score - s0 - s1) * n / (2 * var)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("engine_a", help="engine under test")
    ap.add_argument("engine_b", help="baseline engine")
    ap.add_argument("--games", type=int, default=200, help="max games (rounded up to pairs)")
    ap.add_argument("--tc", default="10+0.1", help="seconds+increment per game, e.g. 10+0.1")
    ap.add_argument("--concurrency", type=int, default=4)
    ap.add_argument("--hash", type=int, default=16, help="hash MB per engine")
    ap.add_argument("--option-a", action="append", default=[], metavar="NAME=VALUE",
                    help="UCI option for engine_a, repeatable (e.g. Threads=1)")
    ap.add_argument("--option-b", action="append", default=[], metavar="NAME=VALUE",
                    help="UCI option for engine_b, repeatable (e.g. UCI_LimitStrength=true --option-b UCI_Elo=1800)")
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--plies", type=int, default=8, help="random opening length")
    ap.add_argument("--sprt", nargs=2, type=float, metavar=("ELO0", "ELO1"),
                    help="stop early once the SPRT accepts H0 or H1 (alpha = beta = 0.05)")
    args = ap.parse_args()

    rng = random.Random(args.seed)
    pairs = (args.games + 1) // 2
    openings = [random_opening(rng, args.plies) for _ in range(pairs)]
    jobs = [(opening, white) for opening in openings for white in (0, 1)]

    lock = threading.Lock()
    stats = {"w": 0, "d": 0, "l": 0, "reasons": {}}
    stop = threading.Event()
    lower, upper = math.log(0.05 / 0.95), math.log(0.95 / 0.05)
    local = threading.local()
    all_engines = []

    def get_engines():
        if not hasattr(local, "engines"):
            local.engines = [chess.engine.SimpleEngine.popen_uci(args.engine_a),
                             chess.engine.SimpleEngine.popen_uci(args.engine_b)]
            for engine, options in zip(local.engines, (args.option_a, args.option_b)):
                if options:
                    engine.configure(parse_options(options))
            with lock:
                all_engines.extend(local.engines)
        return local.engines

    def run(job):
        if stop.is_set():
            return None
        opening, white = job
        return play_game(get_engines(), white, opening, args.tc, args.hash)

    print(f"{args.engine_a} vs {args.engine_b}, tc {args.tc}, {len(jobs)} games max")
    try:
        with ThreadPoolExecutor(args.concurrency) as pool:
            futures = [pool.submit(run, job) for job in jobs]
            for future in as_completed(futures):
                # Once the SPRT has stopped, ignore queued (cancelled) games and the
                # games that were still running, so the verdict matches the counts.
                if future.cancelled() or stop.is_set():
                    continue
                res = future.result()
                if res is None:
                    continue
                result, reason = res
                with lock:
                    key = {1.0: "w", 0.5: "d", 0.0: "l"}[result]
                    stats[key] += 1
                    stats["reasons"][reason] = stats["reasons"].get(reason, 0) + 1
                    w, d, l = stats["w"], stats["d"], stats["l"]
                    elo, err = summarize(w, d, l)
                    line = f"games {w + d + l:4d}  +{w} ={d} -{l}  elo {elo:+.1f} ± {err:.1f}"
                    if args.sprt:
                        ratio = llr(w, d, l, *args.sprt)
                        line += f"  llr {ratio:+.2f} ({lower:.2f}, {upper:.2f})"
                        if ratio >= upper or ratio <= lower:
                            stop.set()
                    print(line, flush=True)
                if stop.is_set():
                    for f in futures:
                        f.cancel()
    finally:  # never leave engine processes running, even after an error
        for engine in all_engines:
            try:
                engine.quit()
            except Exception:
                pass

    w, d, l = stats["w"], stats["d"], stats["l"]
    elo, err = summarize(w, d, l)
    print(f"\nfinal: +{w} ={d} -{l}  elo {elo:+.1f} ± {err:.1f} (95%)")
    print("terminations:", dict(sorted(stats["reasons"].items(), key=lambda kv: -kv[1])))
    if args.sprt:
        ratio = llr(w, d, l, *args.sprt)
        verdict = "H1 accepted" if ratio >= upper else "H0 accepted" if ratio <= lower else "inconclusive"
        print(f"sprt [{args.sprt[0]:g}, {args.sprt[1]:g}]: llr {ratio:+.2f}, {verdict}")


if __name__ == "__main__":
    main()
