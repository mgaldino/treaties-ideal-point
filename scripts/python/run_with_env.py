#!/usr/bin/env python3
"""Load repo-root .env and run a command.

Usage:
  python3 scripts/python/run_with_env.py -- <command> [args...]
"""
from __future__ import annotations

import os
import pathlib
import subprocess
import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent.parent


def _load_env(env_path: pathlib.Path) -> None:
    if not env_path.exists():
        return
    for line in env_path.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        key = key.strip()
        value = value.strip().strip('"').strip("'")
        if key and key not in os.environ:
            os.environ[key] = value


def main() -> int:
    if "--" not in sys.argv:
        print("Expected -- followed by command to run.", file=sys.stderr)
        return 2
    idx = sys.argv.index("--")
    cmd = sys.argv[idx + 1 :]
    if not cmd:
        print("No command provided after --.", file=sys.stderr)
        return 2

    _load_env(REPO_ROOT / ".env")
    try:
        return subprocess.call(cmd)
    except FileNotFoundError:
        print(f"Command not found: {cmd[0]}", file=sys.stderr)
        return 127


if __name__ == "__main__":
    raise SystemExit(main())
