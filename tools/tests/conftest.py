"""Shared fixtures for the play GUI tests.

The tests drive the real engine binary (../../engine by default, or $ENGINE),
so build it first: `make test-play` from the repo root does both.
"""

import os
import stat
import sys
import threading
from contextlib import contextmanager
from pathlib import Path

import pytest

TOOLS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(TOOLS))

import play  # noqa: E402

ENGINE = Path(os.environ.get("ENGINE", TOOLS.parent / "engine")).resolve()


@pytest.fixture(scope="session")
def engine_path() -> Path:
    if not ENGINE.is_file():
        pytest.exit(f"engine binary not found at {ENGINE}; run `make` first", returncode=2)
    return ENGINE


@contextmanager
def serve(engines: dict[str, Path], versions=None):
    """Run play.py's server on a free port in a background thread."""
    game = play.Game(engines, hash_mb=16, versions=versions)
    server = play.ThreadingHTTPServer(("127.0.0.1", 0), play.make_handler(game))
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_address[1]}", game
    finally:
        server.shutdown()
        server.server_close()
        game.close()


def write_script(path: Path, body: str) -> Path:
    """Write an executable Python script (used for fake, broken engines)."""
    path.write_text(f"#!{sys.executable}\n{body}")
    path.chmod(path.stat().st_mode | stat.S_IXUSR)
    return path


# A UCI engine that handshakes normally, then dies as soon as it is asked to
# search: lets the tests check that a crash mid-game is reported, not fatal.
CRASHING_ENGINE = """
import sys
for line in sys.stdin:
    cmd = line.strip()
    if cmd == "uci":
        print("id name crasher"); print("uciok"); sys.stdout.flush()
    elif cmd == "isready":
        print("readyok"); sys.stdout.flush()
    elif cmd.startswith("go") or cmd == "quit":
        sys.exit(1)
"""

# Not an engine at all: exits immediately, so the UCI handshake fails.
BROKEN_ENGINE = "import sys\nsys.exit(1)\n"
