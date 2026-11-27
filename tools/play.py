"""Play against the engine in a browser.

    uv run play.py                      # current build plus every engine in ../bin
    uv run play.py ../bin/nnue-v4 ../bin/hce-texel-B   # just these
    uv run play.py ../bin --port 8001   # every engine in a folder

A small HTTP server (standard library only, plus python-chess) that keeps one
game, checks your moves, asks the engine for its reply over UCI and serves
play.html. Pick the opponent from the page; only one game at a time, and it
is meant for local use.
"""

import argparse
import json
import os
import threading
import webbrowser
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import chess
import chess.engine

PAGE = Path(__file__).with_name("play.html")

# The engine reads each UCI line into a fixed buffer, so a long game's
# "position ... moves" line can overflow it. Send at most this many recent
# plies after a FEN; a repetition cannot reach further back than the
# fifty-move rule's 100 plies anyway.
MAX_SENT_PLIES = 100


def find_engines(paths: list[str]) -> dict[str, Path]:
    """Map a display name to each engine binary. Folders are expanded."""
    engines: dict[str, Path] = {}
    for p in map(Path, paths):
        if p.is_dir():
            files = sorted(f for f in p.iterdir() if f.is_file() and os.access(f, os.X_OK))
        elif p.is_file():
            files = [p]
        else:
            continue
        for f in files:
            name = f.name
            if name in engines and engines[name].resolve() != f.resolve():
                name = f"{f.parent.name}/{f.name}"
            engines.setdefault(name, f)
    return engines


class Game:
    def __init__(self, engines: dict[str, Path], hash_mb: int):
        self.engines = engines
        self.hash_mb = hash_mb
        self.engine = None
        self.engine_name = None
        self.error = None
        self.lock = threading.Lock()
        self.new(next(iter(engines)), chess.WHITE, 1.0)

    def load(self, name: str):
        """Start the named engine, stopping the previous one."""
        if name == self.engine_name and self.engine is not None:
            return
        if name not in self.engines:
            raise ValueError(f"unknown engine {name!r}")
        self.close()
        self.engine = chess.engine.SimpleEngine.popen_uci(str(self.engines[name]))
        try:
            self.engine.configure({"Hash": self.hash_mb})
        except chess.engine.EngineError:
            pass  # engine has no Hash option
        self.engine_name = name

    def close(self):
        if self.engine is not None:
            try:
                self.engine.quit()
            except chess.engine.EngineTerminatedError:
                pass
        self.engine = None
        self.engine_name = None

    def new(self, name: str, human: chess.Color, movetime: float, fen: str | None = None):
        self.error = None
        try:
            board = chess.Board(fen) if fen else chess.Board()
            if not board.is_valid():
                raise ValueError(board.status().name)
        except ValueError as e:
            self.error = f"bad FEN: {e}"
            board = chess.Board()
        try:
            self.load(name)
        except Exception as e:  # bad binary, crashed on startup, ...
            self.close()
            self.error = f"could not start {name}: {e}"
        self.board = board
        self.human = human
        self.movetime = movetime
        self.info = {}

    def board_for_engine(self) -> chess.Board:
        """The current position with only the last MAX_SENT_PLIES moves."""
        moves = self.board.move_stack[-MAX_SENT_PLIES:]
        root = self.board.copy()
        for _ in moves:
            root.pop()
        b = chess.Board(root.fen())
        for m in moves:
            b.push(m)
        return b

    def engine_move(self):
        if (self.engine is None or self.board.is_game_over(claim_draw=True)
                or self.board.turn == self.human):
            return
        try:
            result = self.engine.play(
                self.board_for_engine(),
                chess.engine.Limit(time=self.movetime),
                info=chess.engine.INFO_SCORE,
            )
        except (chess.engine.EngineError, chess.engine.EngineTerminatedError) as e:
            self.error = f"{self.engine_name} failed: {e}"
            self.close()
            return
        self.info = result.info
        self.board.push(result.move)

    def human_move(self, uci: str) -> str | None:
        if self.board.turn != self.human:
            return "not your turn"
        try:
            move = chess.Move.from_uci(uci)
        except ValueError:
            return "bad move"
        if move not in self.board.legal_moves:
            return "illegal move"
        self.board.push(move)
        return None

    def undo(self):
        """Take back the human's last move and the engine's reply to it.

        Does nothing if the human has not moved yet (e.g. playing Black and
        only the engine's first move is on the board)."""
        b = self.board
        root_turn = b.root().turn
        human_plies = [i for i in range(len(b.move_stack))
                       if (root_turn if i % 2 == 0 else not root_turn) == self.human]
        if not human_plies:
            return
        for _ in range(len(b.move_stack) - human_plies[-1]):
            b.pop()
        self.info = {}

    def state(self) -> dict:
        b = self.board
        # SAN history, replayed from the start
        replay, san = b.root(), []
        root = b.root()
        for m in b.move_stack:
            san.append(replay.san(m))
            replay.push(m)

        score = cp = mate = None
        if "score" in self.info:
            s = self.info["score"].white()
            if s.is_mate():
                mate = s.mate()
                score = f"#{mate}"
            else:
                cp = s.score()
                score = f"{cp / 100:+.2f}"

        status = None
        outcome = b.outcome(claim_draw=True)
        if outcome:
            reason = outcome.termination.name.replace("_", " ").lower()
            if outcome.winner is None:
                status = f"Draw by {reason}"
            else:
                who = "You win" if outcome.winner == self.human else f"{self.engine_name} wins"
                status = f"{who} by {reason}"

        return {
            "fen": b.fen(),
            "turn": "w" if b.turn else "b",
            "human": "w" if self.human else "b",
            "legal": [m.uci() for m in b.legal_moves],
            "last": b.move_stack[-1].uci() if b.move_stack else None,
            "check": b.is_check(),
            "san": san,
            "first_move": root.fullmove_number,
            "first_black": root.turn == chess.BLACK,
            "score": score,
            "cp": cp,
            "mate": mate,
            "depth": self.info.get("depth"),
            "status": status,
            "error": self.error,
            "engine": self.engine_name,
            "engines": list(self.engines),
            "movetime": self.movetime,
        }


def make_handler(game: Game):
    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *args):
            pass

        def send(self, code: int, body: bytes, ctype: str):
            self.send_response(code)
            self.send_header("Content-Type", ctype)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def send_json(self, obj, code=200):
            self.send(code, json.dumps(obj).encode(), "application/json")

        def do_GET(self):
            if self.path in ("/", "/index.html"):
                self.send(200, PAGE.read_bytes(), "text/html; charset=utf-8")
            elif self.path == "/api/state":
                with game.lock:
                    self.send_json(game.state())
            else:
                self.send(404, b"not found", "text/plain")

        def do_POST(self):
            length = int(self.headers.get("Content-Length") or 0)
            try:
                req = json.loads(self.rfile.read(length) or b"{}")
            except json.JSONDecodeError:
                return self.send_json({"error": "bad json"}, 400)

            with game.lock:
                if self.path == "/api/new":
                    human = chess.BLACK if req.get("color") == "b" else chess.WHITE
                    movetime = min(max(float(req.get("movetime", 1.0)), 0.05), 60.0)
                    name = req.get("engine") or game.engine_name or next(iter(game.engines))
                    game.new(name, human, movetime, req.get("fen") or None)
                elif self.path == "/api/move":
                    err = game.human_move(str(req.get("move", "")))
                    if err:
                        return self.send_json({**game.state(), "error": err}, 400)
                elif self.path == "/api/undo":
                    game.undo()
                elif self.path == "/api/go":
                    game.engine_move()
                else:
                    return self.send(404, b"not found", "text/plain")
                self.send_json(game.state())

    return Handler


def main():
    p = argparse.ArgumentParser(description="Play against UCI engines in the browser.")
    p.add_argument("engines", nargs="*", default=["../engine", "../bin"],
                   help="engine binaries or folders of them (default: ../engine and ../bin)")
    p.add_argument("--port", type=int, default=8000)
    p.add_argument("--hash", type=int, default=64, help="hash table size in MB")
    p.add_argument("--no-browser", action="store_true", help="don't open a browser tab")
    args = p.parse_args()

    engines = find_engines(args.engines)
    if not engines:
        p.error(f"no engine binaries found in {' '.join(args.engines)}; run `make` first")

    game = Game(engines, args.hash)
    server = ThreadingHTTPServer(("127.0.0.1", args.port), make_handler(game))
    url = f"http://localhost:{args.port}"
    print(f"{len(engines)} engine(s): {', '.join(engines)}")
    print(f"Open {url}  (Ctrl-C to stop)")
    if not args.no_browser:
        webbrowser.open(url)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        game.close()


if __name__ == "__main__":
    main()
