"""play.py without a browser: game rules, endings, engine handling and the HTTP API."""

import json
import random
import urllib.error
import urllib.request

import chess
import pytest

import play
from conftest import BROKEN_ENGINE, CRASHING_ENGINE, serve, write_script


@pytest.fixture
def game(engine_path):
    g = play.Game({"engine": engine_path}, hash_mb=16)
    g.new("engine", chess.WHITE, 0.05)
    yield g
    g.close()


# ---- engine discovery ----

def test_find_engines_expands_folders_and_skips_non_executables(tmp_path, engine_path):
    (tmp_path / "a").symlink_to(engine_path)
    (tmp_path / "b").symlink_to(engine_path)
    (tmp_path / "notes.txt").write_text("not an engine")
    found = play.find_engines([str(engine_path), str(tmp_path), str(tmp_path / "missing")])
    assert list(found) == ["engine", "a", "b"]


def test_find_engines_disambiguates_same_name(tmp_path, engine_path):
    for d in ("x", "y"):
        (tmp_path / d).mkdir()
        write_script(tmp_path / d / "eng", BROKEN_ENGINE)
    found = play.find_engines([str(tmp_path / "x"), str(tmp_path / "y")])
    assert list(found) == ["eng", "y/eng"]


# ---- moves ----

def test_rejects_illegal_wrong_side_and_garbage(game):
    assert game.human_move("e2e5") == "illegal move"
    assert game.human_move("zz") == "bad move"
    assert game.human_move("e2e4") is None
    assert game.human_move("d2d4") == "not your turn"


def test_engine_replies_with_a_legal_move(game):
    game.human_move("e2e4")
    game.engine_move()
    s = game.state()
    assert len(s["san"]) == 2 and s["turn"] == "w"
    assert s["depth"] and (s["cp"] is not None or s["mate"] is not None)


@pytest.mark.parametrize("fen,move,san,check_sq,piece", [
    ("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", "e1g1", "O-O", chess.F1, "R"),
    ("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", "e1c1", "O-O-O", chess.D1, "R"),
    ("4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1", "e5d6", "exd6", chess.D5, None),
    ("8/4P1k1/8/8/8/8/8/K7 w - - 0 1", "e7e8n", "e8=N+", chess.E8, "N"),
    ("3r2k1/4P3/8/8/8/8/8/K7 w - - 0 1", "e7d8q", "exd8=Q+", chess.D8, "Q"),
])
def test_special_moves(game, fen, move, san, check_sq, piece):
    game.new("engine", chess.WHITE, 0.05, fen)
    assert game.human_move(move) is None
    assert game.state()["san"] == [san]
    p = game.board.piece_at(check_sq)
    assert (p.symbol() if p else None) == piece


def test_promotion_requires_a_piece(game):
    game.new("engine", chess.WHITE, 0.05, "8/4P1k1/8/8/8/8/8/K7 w - - 0 1")
    assert game.human_move("e7e8") == "illegal move"


def test_undo_returns_to_humans_turn(game):
    game.human_move("e2e4"); game.engine_move()
    game.human_move("d2d4"); game.engine_move()
    game.undo()
    assert len(game.board.move_stack) == 2 and game.board.turn == chess.WHITE
    game.undo(); game.undo()
    assert game.board.move_stack == [] and game.state()["san"] == []


def test_undo_as_black_keeps_engines_first_move(engine_path):
    g = play.Game({"engine": engine_path}, 16)
    g.new("engine", chess.BLACK, 0.05)
    g.engine_move()
    g.undo()  # nothing of ours to take back
    assert len(g.board.move_stack) == 1 and g.board.turn == chess.BLACK
    g.human_move("e7e5"); g.engine_move()
    g.undo()
    assert len(g.board.move_stack) == 1 and g.board.turn == chess.BLACK
    g.close()


def test_undo_from_fen_with_black_to_move(game):
    game.new("engine", chess.WHITE, 0.05, "4k3/8/8/8/8/8/4P3/4K3 b - - 0 1")
    game.engine_move()
    game.human_move("e2e4")
    game.undo()
    assert len(game.board.move_stack) == 1 and game.board.turn == chess.WHITE


# ---- FEN starts ----

def test_fen_start_and_move_numbering(game):
    game.new("engine", chess.BLACK, 0.05, "4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 12")
    game.human_move("d4e3")
    s = game.state()
    assert s["san"] == ["dxe3"] and s["first_move"] == 12 and s["first_black"] is True


@pytest.mark.parametrize("fen", ["not a fen", "8/8/8/8/8/8/8/8 w - - 0 1",
                                 "k7/8/8/8/8/8/3p4/2R1K3 b - - 0 1"])
def test_bad_fen_falls_back_to_start_with_error(game, fen):
    game.new("engine", chess.WHITE, 0.05, fen)
    s = game.state()
    assert s["error"].startswith("bad FEN") and s["fen"] == chess.STARTING_FEN


# ---- game endings ----

@pytest.mark.parametrize("fen,move,status", [
    ("6k1/5ppp/8/8/8/8/8/R5K1 w - - 0 1", "a1a8", "You win by checkmate"),
    ("7k/8/8/6Q1/8/8/8/K7 w - - 0 1", "g5g6", "Draw by stalemate"),
    ("7k/8/8/8/8/8/1r6/K7 w - - 0 1", "a1b2", "Draw by insufficient material"),
    ("7k/8/8/8/8/8/8/K6r w - - 99 80", "a1a2", "Draw by fifty moves"),
])
def test_endings_after_human_move(game, fen, move, status):
    game.new("engine", chess.WHITE, 0.05, fen)
    assert game.human_move(move) is None
    assert game.state()["status"] == status
    game.engine_move()  # the engine must not move once the game is over
    assert len(game.board.move_stack) == 1


def test_threefold_repetition_ends_game(game):
    for m in ["g1f3", "g8f6", "f3g1", "f6g8"] * 2:
        game.board.push_uci(m)
    assert game.state()["status"] == "Draw by threefold repetition"


def test_engine_checkmates_human(game):
    game.new("engine", chess.WHITE, 0.2, "r5k1/8/8/8/8/8/5PPP/6K1 w - - 0 1")
    game.human_move("g1h1")
    game.engine_move()
    assert game.state()["status"] == "engine wins by checkmate"


def test_no_engine_move_after_game_over(game):
    game.new("engine", chess.BLACK, 0.05, "6k1/5ppp/8/8/8/8/8/R5K1 w - - 0 1")
    game.board.push_uci("a1a8")
    game.engine_move()
    assert len(game.board.move_stack) == 1


# ---- long games and the engine's UCI line limit ----

def test_long_game_sends_at_most_100_plies_and_same_position(game):
    rng = random.Random(3)
    while True:
        b = chess.Board()
        while not b.is_game_over() and len(b.move_stack) < 300:
            b.push(rng.choice(list(b.legal_moves)))
        if len(b.move_stack) == 300:
            break
    game.board, game.human = b, not b.turn
    sent = game.board_for_engine()
    assert len(sent.move_stack) == 100
    assert sent.fen() == b.fen()
    # repetition history inside the last 100 plies is preserved
    assert sent.is_repetition(2) == b.is_repetition(2)
    game.engine_move()
    assert len(game.board.move_stack) == 301 and game.error is None


# ---- engine switching and failures ----

def test_switch_engines_restarts_process(tmp_path, engine_path):
    (tmp_path / "alpha").symlink_to(engine_path)
    (tmp_path / "beta").symlink_to(engine_path)
    g = play.Game(play.find_engines([str(tmp_path)]), 16)
    first = g.engine
    g.new("beta", chess.WHITE, 0.05)
    assert g.engine_name == "beta" and g.engine is not first
    g.new("beta", chess.WHITE, 0.05)
    assert g.state()["engines"] == ["alpha", "beta"]
    g.close()


def test_broken_engine_reports_error_and_recovers(tmp_path, engine_path):
    engines = {"good": engine_path, "broken": write_script(tmp_path / "broken", BROKEN_ENGINE)}
    g = play.Game(engines, 16)
    g.new("broken", chess.WHITE, 0.05)
    s = g.state()
    assert s["error"].startswith("could not start broken") and s["engine"] is None
    assert g.human_move("e2e4") is None
    g.engine_move()  # no engine: no crash, no move
    assert len(g.board.move_stack) == 1
    g.new("good", chess.WHITE, 0.05)
    assert g.state()["error"] is None and g.engine_name == "good"
    g.close()


def test_engine_crash_mid_game_is_reported(tmp_path):
    g = play.Game({"crasher": write_script(tmp_path / "crasher", CRASHING_ENGINE)}, 16)
    g.human_move("e2e4")
    g.engine_move()
    s = g.state()
    assert "crasher failed" in s["error"] and s["engine"] is None
    g.close()


def test_unknown_engine_name(game):
    game.new("nope", chess.WHITE, 0.05)
    assert "unknown engine" in game.state()["error"]


# ---- HTTP API ----

def call(base, path, body=None):
    data = None if body is None else (body if isinstance(body, bytes) else json.dumps(body).encode())
    req = urllib.request.Request(base + path, data=data, headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req) as r:
            return r.status, r.read()
    except urllib.error.HTTPError as e:
        return e.code, e.read()


def test_http_api(engine_path):
    with serve({"engine": engine_path}) as (base, game):
        code, body = call(base, "/")
        assert code == 200 and b"<title>Play the engine</title>" in body

        code, body = call(base, "/api/new", {"color": "w", "movetime": 999})
        s = json.loads(body)
        assert code == 200 and s["movetime"] == 60.0 and s["human"] == "w"
        assert call(base, "/api/new", {"movetime": 0})[1] and game.movetime == 0.05

        code, body = call(base, "/api/move", {"move": "e2e5"})
        assert code == 400 and json.loads(body)["error"] == "illegal move"

        code, body = call(base, "/api/move", {"move": "e2e4"})
        assert code == 200 and json.loads(body)["turn"] == "b"
        s = json.loads(call(base, "/api/go", {})[1])
        assert len(s["san"]) == 2

        s = json.loads(call(base, "/api/undo", {})[1])
        assert s["san"] == []

        assert json.loads(call(base, "/api/state")[1])["fen"] == chess.STARTING_FEN
        assert call(base, "/api/move", b"{not json")[0] == 400
        assert call(base, "/api/nope", {})[0] == 404
        assert call(base, "/nope")[0] == 404

        s = json.loads(call(base, "/api/new", {"color": "b", "fen": "4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 1"})[1])
        assert s["human"] == "b" and s["turn"] == "b" and "d4e3" in s["legal"]
