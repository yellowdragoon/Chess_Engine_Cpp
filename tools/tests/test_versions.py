"""Engine version history: the manifest, patches, builds, fixed-depth play and the ladder."""

import subprocess
from pathlib import Path

import chess
import chess.engine
import pytest

import ladder
import match
import play
import versions
from versions import ROOT, Version, build, load_manifest


@pytest.fixture(scope="module")
def manifest():
    return load_manifest()


# ---- manifest ----

def test_manifest_is_valid(manifest):
    assert len(manifest) >= 5
    for v in manifest:
        # every commit exists in this repository
        subprocess.run(["git", "-C", str(ROOT), "cat-file", "-e", f"{v.commit}^{{commit}}"], check=True)
        assert v.description and v.tag == f"engine/{v.name}"
        assert v.depth is None or 1 <= v.depth <= 10
    # fixed-depth versions all come before the first version with time control
    first_clock = next(i for i, v in enumerate(manifest) if v.depth is None)
    assert all(v.depth is None for v in manifest[first_clock:])


@pytest.mark.parametrize("text,error", [
    ("v0 abc123\n", "expected"),
    ("v0 abc123 fast description\n", "limit must be"),
    ("v0 abc123 - one\nv0 def456 - two\n", "duplicate"),
])
def test_manifest_rejects_bad_lines(tmp_path, text, error):
    f = tmp_path / "versions.txt"
    f.write_text("# comment\n\n" + text)
    with pytest.raises(ValueError, match=error):
        load_manifest(f)


def test_manifest_parses_limits(tmp_path):
    f = tmp_path / "versions.txt"
    f.write_text("a 1111111 depth=3 first one\nb 2222222 -  second, with   spaces\n")
    a, b = load_manifest(f)
    assert (a.depth, b.depth) == (3, None)
    assert b.description == "second, with   spaces"


def test_every_patch_belongs_to_a_version_and_applies_cleanly(manifest, tmp_path):
    names = {v.name: v for v in manifest}
    patches = sorted(versions.PATCHES.glob("*.patch"))
    assert patches, "expected patches for the oldest versions"
    for p in patches:
        assert p.stem in names, f"{p.name} has no version in versions.txt"
        src = tmp_path / p.stem
        src.mkdir()
        archive = subprocess.run(["git", "-C", str(ROOT), "archive", names[p.stem].commit],
                                 check=True, capture_output=True).stdout
        subprocess.run(["tar", "-x", "-C", str(src)], input=archive, check=True)
        subprocess.run(["patch", "-p1", "--dry-run", "-s", "-i", str(p)], cwd=src, check=True)


# ---- builds (one without a Makefile, one with) ----

@pytest.mark.parametrize("name", ["v0-original", "v4-texel"])
def test_build_version_speaks_uci(manifest, tmp_path, name):
    v = next(v for v in manifest if v.name == name)
    exe = build(v, out_dir=tmp_path)
    assert exe == tmp_path / name and exe.exists()
    mtime = exe.stat().st_mtime
    assert build(v, out_dir=tmp_path) == exe and exe.stat().st_mtime == mtime  # kept, not rebuilt
    with chess.engine.SimpleEngine.popen_uci(str(exe)) as e:
        board = chess.Board("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
        limit = chess.engine.Limit(depth=v.depth) if v.depth else chess.engine.Limit(time=0.1)
        # the old versions finish their fixed depth quickly even here (the
        # quiescence patch); without it this position took minutes
        result = e.play(board, limit)
        assert result.move in board.legal_moves


def test_build_does_not_touch_the_working_tree(manifest, tmp_path):
    before = subprocess.run(["git", "-C", str(ROOT), "status", "--porcelain"], capture_output=True, text=True).stdout
    build(manifest[1], out_dir=tmp_path)
    after = subprocess.run(["git", "-C", str(ROOT), "status", "--porcelain"], capture_output=True, text=True).stdout
    assert before == after


# ---- GUI integration ----

def test_play_orders_engines_and_uses_fixed_depth(engine_path):
    engines = {n: engine_path for n in ["zeta", "v1", "engine", "v0", "alpha"]}
    ordered = play.order_engines(engines, ["v0", "v1"])
    assert list(ordered) == ["engine", "v0", "v1", "alpha", "zeta"]

    vs = {"v0": Version("v0", "x", 3, "old one"), "v1": Version("v1", "y", None, "new one")}
    g = play.Game({"v0": engine_path, "v1": engine_path}, 16, vs)
    assert g.limit() == chess.engine.Limit(depth=3)
    g.new("v1", chess.WHITE, 0.25)
    assert g.limit() == chess.engine.Limit(time=0.25)
    s = g.state()
    assert s["versions"]["v0"] == {"description": "old one", "depth": 3}
    g.new("v0", chess.WHITE, 0.25)
    g.human_move("e2e4"); g.engine_move()
    assert g.state()["depth"] == 3
    g.close()


# ---- match runner ----

def test_match_fixed_depth_engines_never_lose_on_time(engine_path):
    engines = [chess.engine.SimpleEngine.popen_uci(str(engine_path)) for _ in range(2)]
    try:
        # a 1 ms clock: without --depth-a/-b one side would forfeit at once
        score, reason = match.play_game(engines, 0, [], "0.001+0", 16, depths=(2, 2))
        assert reason != "time forfeit"
        score, reason = match.play_game(engines, 0, [], "0.001+0", 16, depths=(None, 2))
        assert reason == "time forfeit" and score == 0.0
    finally:
        for e in engines:
            e.quit()


# ---- ladder ----

def test_ladder_parses_match_output():
    m = ladder.FINAL.search("final: +142 =24 -34  elo +209.9 ± 52.9 (95%)")
    assert m.groups() == ("142", "24", "34", "+209.9", "52.9")
    assert ladder.FINAL.search("final: +0 =0 -10  elo -inf ± inf (95%)")


def test_ladder_table(tmp_path, monkeypatch):
    monkeypatch.setattr(ladder, "DOCS", tmp_path)
    monkeypatch.setattr(ladder, "TABLE", tmp_path / "engine-history.md")
    vs = [Version("a", "1", 4, "first"), Version("b", "2", 4, "second"), Version("c", "3", None, "third")]
    results = [
        {"new": "b", "old": "a", "w": 10, "d": 5, "l": 5, "elo": 88.0, "err": 30.0, "tc": "8+0.08",
         "limits": {"a": "depth 4", "b": "depth 4"}},
    ]
    ladder.write_table(vs, results)
    md = (tmp_path / "engine-history.md").read_text()
    assert "| `a` | first | baseline |" in md
    assert "| `b` | second | **+88** ± 30 | +10 =5 −5 | +88 |" in md
    assert "| `c` | third | not measured |" in md
    assert "`a` at depth 4" in md and "8+0.08" in md


def test_committed_history_matches_manifest(manifest):
    """docs/engine-history.md must have a row for every version."""
    md = (ROOT / "docs" / "engine-history.md").read_text()
    for v in manifest:
        assert f"| `{v.name}` |" in md
