"""The engine must apply every move the GUI sends exactly as python-chess does.

The GUI checks legality with python-chess and sends `position ... moves ...`.
If the engine applied a move differently (castling rook not moved, en passant
pawn not removed, wrong promotion piece), it would search a different position
from the one on screen. These tests compare the engine's `d` output with
python-chess after special moves and along many random games.
"""

import random
import subprocess

import chess
import pytest


class UciEngine:
    def __init__(self, path):
        self.p = subprocess.Popen([str(path)], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                  text=True, bufsize=1)

    def send(self, line: str):
        self.p.stdin.write(line + "\n")
        self.p.stdin.flush()

    def display(self, position_cmd: str) -> dict:
        """Send a position, then parse the engine's `d` output."""
        self.send(position_cmd)
        self.send("d")
        rows, info = {}, {}
        while True:
            line = self.p.stdout.readline()
            assert line, "engine exited"
            if line[:1] in "12345678" and line[1:3] == " |":
                cells = [c.strip() for c in line.split("|")[1:9]]
                rows[int(line[0])] = cells
            elif ":" in line:
                key, _, value = line.partition(":")
                info[key.strip()] = value.strip()
                if key.strip() == "Castling rights":
                    break
        placement = {}
        for rank, cells in rows.items():
            for f, c in enumerate(cells):
                if c:
                    placement[chess.square(f, rank - 1)] = c
        return {
            "placement": placement,
            "turn": info["Side to move"],
            "ep": info["En passant square"],
            "castling": info["Castling rights"],
        }

    def close(self):
        self.send("quit")
        self.p.wait(timeout=5)


def expected(board: chess.Board) -> dict:
    castling = "".join(
        sym if board.has_castling_rights(color) and test(color) else "-"
        for sym, color, test in [
            ("K", chess.WHITE, board.has_kingside_castling_rights),
            ("Q", chess.WHITE, board.has_queenside_castling_rights),
            ("k", chess.BLACK, board.has_kingside_castling_rights),
            ("q", chess.BLACK, board.has_queenside_castling_rights),
        ]
    )
    return {
        "placement": {sq: p.symbol() for sq, p in board.piece_map().items()},
        "turn": "white" if board.turn else "black",
        "ep": chess.square_name(board.ep_square) if board.ep_square is not None else "none",
        "castling": castling,
    }


@pytest.fixture(scope="module")
def uci(engine_path):
    e = UciEngine(engine_path)
    yield e
    e.close()


SPECIAL_MOVES = [
    ("white O-O", "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1", ["e1g1"]),
    ("white O-O-O", "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1", ["e1c1"]),
    ("black O-O", "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQkq - 0 1", ["e8g8"]),
    ("black O-O-O", "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQkq - 0 1", ["e8c8"]),
    ("rook move drops one right", "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", ["h1h2"]),
    ("rook captured drops right", "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", ["a1a8"]),
    ("king move drops both", "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1", ["e8d8"]),
    ("white en passant", "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1", ["e5d6"]),
    ("black en passant", "4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 1", ["d4e3"]),
    ("ep square after double push", "4k3/3p4/8/4P3/8/8/8/4K3 b - - 0 1", ["d7d5"]),
    ("ep then reply", "4k3/3p4/8/4P3/8/8/8/4K3 b - - 0 1", ["d7d5", "e5d6"]),
    ("promote queen", "8/4P1k1/8/8/8/8/8/K7 w - - 0 1", ["e7e8q"]),
    ("promote knight", "8/4P1k1/8/8/8/8/8/K7 w - - 0 1", ["e7e8n"]),
    ("promote rook", "8/4P1k1/8/8/8/8/8/K7 w - - 0 1", ["e7e8r"]),
    ("promote bishop", "8/4P1k1/8/8/8/8/8/K7 w - - 0 1", ["e7e8b"]),
    ("capture-promote", "3r2k1/4P3/8/8/8/8/8/K7 w - - 0 1", ["e7d8q"]),
    ("black promote", "k7/8/8/8/8/8/3p4/2R3K1 b - - 0 1", ["d2c1n"]),
    ("from startpos", None, ["e2e4", "d7d5", "e4d5", "c7c5", "d5c6"]),
]


@pytest.mark.parametrize("name,fen,moves", SPECIAL_MOVES, ids=[m[0] for m in SPECIAL_MOVES])
def test_special_moves(uci, name, fen, moves):
    board = chess.Board(fen) if fen else chess.Board()
    for m in moves:
        board.push_uci(m)
    root = f"fen {fen}" if fen else "startpos"
    assert uci.display(f"position {root} moves {' '.join(moves)}") == expected(board)


@pytest.mark.parametrize("seed", range(12))
def test_random_games_match_python_chess(uci, seed):
    """Random games are full of odd castling, ep and promotion situations."""
    rng = random.Random(seed)
    board = chess.Board()
    moves = []
    # stay well under the engine's 2000-byte UCI line (about 390 plies)
    while not board.is_game_over() and len(moves) < 160:
        move = rng.choice(list(board.legal_moves))
        board.push(move)
        moves.append(move.uci())
        got = uci.display(f"position startpos moves {' '.join(moves)}")
        assert got == expected(board), f"after {' '.join(moves)}"
