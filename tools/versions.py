"""Build, list and tag the engine versions in ../versions.txt.

    uv run versions.py build            # build any missing versions into ../bin
    uv run versions.py build --force    # rebuild all of them
    uv run versions.py list
    uv run versions.py tag              # create local git tags engine/<name>

Each version is built from its commit with `git archive`, so your working tree
and branch are never touched. If tools/versions/<name>.patch exists it is
applied first (only the oldest versions need one; each patch says why).
Binaries are not committed: they are built with -march=native for this machine
and can always be rebuilt from source.
"""

import argparse
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
MANIFEST = ROOT / "versions.txt"
PATCHES = ROOT / "tools" / "versions"
BIN = ROOT / "bin"

# Used for commits from before the Makefile existed; same flags as the Makefile.
CXX_FALLBACK = ["c++", "-O3", "-DNDEBUG", "-std=c++17", "-march=native", "-w", "board.cpp", "-o", "engine"]


@dataclass
class Version:
    name: str
    commit: str
    depth: int | None  # fixed search depth for engines without time control
    description: str

    @property
    def tag(self) -> str:
        return f"engine/{self.name}"


def load_manifest(path: Path = MANIFEST) -> list[Version]:
    versions = []
    for n, line in enumerate(path.read_text().splitlines(), 1):
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        parts = line.split(None, 3)
        if len(parts) < 4:
            raise ValueError(f"{path.name}:{n}: expected 'name commit limit description'")
        name, commit, limit, desc = parts
        if limit == "-":
            depth = None
        elif limit.startswith("depth="):
            depth = int(limit.removeprefix("depth="))
        else:
            raise ValueError(f"{path.name}:{n}: limit must be '-' or 'depth=N', got {limit!r}")
        versions.append(Version(name, commit, depth, desc))
    names = [v.name for v in versions]
    if len(set(names)) != len(names):
        raise ValueError(f"{path.name}: duplicate version names")
    return versions


def git(*args: str, **kw) -> subprocess.CompletedProcess:
    return subprocess.run(["git", "-C", str(ROOT), *args], check=True, **kw)


def build(v: Version, out_dir: Path = BIN, force: bool = False) -> Path:
    out = out_dir / v.name
    if out.exists() and not force:
        return out
    out_dir.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=f"engine-{v.name}-") as tmp:
        tmp = Path(tmp)
        archive = subprocess.run(["git", "-C", str(ROOT), "archive", v.commit],
                                 check=True, capture_output=True).stdout
        subprocess.run(["tar", "-x", "-C", str(tmp)], input=archive, check=True)
        patch = PATCHES / f"{v.name}.patch"
        if patch.exists():
            subprocess.run(["patch", "-p1", "-s", "-i", str(patch)], cwd=tmp, check=True)
        if (tmp / "Makefile").exists():
            subprocess.run(["make", "-s", "EXE=engine"], cwd=tmp, check=True)
        else:
            subprocess.run(CXX_FALLBACK, cwd=tmp, check=True)
        shutil.copy2(tmp / "engine", out)
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    b = sub.add_parser("build", help="build versions into bin/")
    b.add_argument("names", nargs="*", help="only these versions (default: all)")
    b.add_argument("--force", action="store_true", help="rebuild even if the binary exists")
    sub.add_parser("list", help="show the versions")
    sub.add_parser("tag", help="create annotated tags engine/<name> (local only; push them yourself)")
    args = ap.parse_args()

    versions = load_manifest()
    if args.cmd == "list":
        for v in versions:
            limit = f"depth {v.depth}" if v.depth else "clock"
            built = "built" if (BIN / v.name).exists() else "-"
            print(f"{v.name:14} {v.commit:9} {limit:8} {built:6} {v.description}")
    elif args.cmd == "build":
        wanted = set(args.names) or {v.name for v in versions}
        unknown = wanted - {v.name for v in versions}
        if unknown:
            sys.exit(f"unknown version(s): {', '.join(sorted(unknown))}")
        for v in versions:
            if v.name in wanted:
                existed = (BIN / v.name).exists()
                out = build(v, force=args.force)
                print(f"{'kept ' if existed and not args.force else 'built'} {out.relative_to(ROOT)}")
    elif args.cmd == "tag":
        existing = set(git("tag", "--list", "engine/*", capture_output=True, text=True).stdout.split())
        for v in versions:
            if v.tag in existing:
                print(f"exists  {v.tag}")
                continue
            git("tag", "-a", v.tag, v.commit, "-m", f"{v.name}: {v.description}")
            print(f"tagged  {v.tag} -> {v.commit}")
        print("\nPush with: git push origin 'refs/tags/engine/*'")


if __name__ == "__main__":
    main()
