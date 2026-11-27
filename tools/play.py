"""Play against the engine in a browser.

    uv run play.py ../engine            # then open http://localhost:8000

A small HTTP server (standard library only, plus python-chess) that keeps one
game, checks your moves, asks the engine for its reply over UCI and serves
play.html. Only one game at a time; it is meant for local use.
"""

import argparse
import json
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


class Game:
    def __init__(self, engine_path: str, hash_mb: int):
        self.engine = chess.engine.SimpleEngine.popen_uci(engine_path)
        try:
            self.engine.configure({"Hash": hash_mb})
        except chess.engine.EngineError:
            pass  # engine has no Hash option
        self.name = self.engine.id.get("name", Path(engine_path).name)
        self.lock = threading.Lock()
        self.new(chess.WHITE, 1.0)

    def new(self, human: chess.Color, movetime: float):
        self.board = chess.Board()
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
        if self.board.is_game_over(claim_draw=True) or self.board.turn == self.human:
            return
        result = self.engine.play(
            self.board_for_engine(),
            chess.engine.Limit(time=self.movetime),
            info=chess.engine.INFO_SCORE,
        )
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
        """Take back to the human's previous turn."""
        if self.board.move_stack:
            self.board.pop()
        while self.board.move_stack and self.board.turn != self.human:
            self.board.pop()
        self.info = {}

    def state(self) -> dict:
        b = self.board
        # SAN history, replayed from the start
        replay, san = chess.Board(), []
        for m in b.move_stack:
            san.append(replay.san(m))
            replay.push(m)

        score = None
        if "score" in self.info:
            s = self.info["score"].white()
            score = f"#{s.mate()}" if s.is_mate() else f"{s.score() / 100:+.2f}"

        status = None
        outcome = b.outcome(claim_draw=True)
        if outcome:
            reason = outcome.termination.name.replace("_", " ").lower()
            if outcome.winner is None:
                status = f"Draw ({reason})"
            else:
                who = "You win" if outcome.winner == self.human else f"{self.name} wins"
                status = f"{who} ({reason})"

        return {
            "fen": b.fen(),
            "turn": "w" if b.turn else "b",
            "human": "w" if self.human else "b",
            "legal": [m.uci() for m in b.legal_moves],
            "last": b.move_stack[-1].uci() if b.move_stack else None,
            "check": b.is_check(),
            "san": san,
            "score": score,
            "depth": self.info.get("depth"),
            "status": status,
            "engine": self.name,
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
                    game.new(human, movetime)
                elif self.path == "/api/move":
                    err = game.human_move(str(req.get("move", "")))
                    if err:
                        return self.send_json({"error": err, **game.state()}, 400)
                elif self.path == "/api/undo":
                    game.undo()
                elif self.path == "/api/go":
                    game.engine_move()
                else:
                    return self.send(404, b"not found", "text/plain")
                self.send_json(game.state())

    return Handler


def main():
    p = argparse.ArgumentParser(description="Play against a UCI engine in the browser.")
    p.add_argument("engine", nargs="?", default="../engine", help="path to the engine binary")
    p.add_argument("--port", type=int, default=8000)
    p.add_argument("--hash", type=int, default=64, help="hash table size in MB")
    p.add_argument("--no-browser", action="store_true", help="don't open a browser tab")
    args = p.parse_args()

    game = Game(args.engine, args.hash)
    server = ThreadingHTTPServer(("127.0.0.1", args.port), make_handler(game))
    url = f"http://localhost:{args.port}"
    print(f"Playing {game.name} at {url}  (Ctrl-C to stop)")
    if not args.no_browser:
        webbrowser.open(url)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        game.engine.quit()


if __name__ == "__main__":
    main()
