#!/usr/bin/env python3
"""Download WITS product concordance tables (public)."""
from __future__ import annotations

import argparse
import json
import os
import pathlib
import sys
import time
import urllib.request

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
sys.path.append(str(SCRIPT_DIR))
from _common import DEFAULT_UA, ensure_dir, today_str, write_access_log  # noqa: E402

BASE_URL = "https://wits.worldbank.org/data/public/concordance"

DEFAULT_TABLES = [
    "Concordance_H4_to_H0.zip",
]


def download(url: str, dest: pathlib.Path, timeout: int = 120) -> None:
    req = urllib.request.Request(url, headers={"User-Agent": DEFAULT_UA})
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        dest.write_bytes(resp.read())


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--outdir", default="data/raw/concordance/wits")
    parser.add_argument("--tables", default=",")
    parser.add_argument("--sleep", type=float, default=0.5)
    parser.add_argument("--timeout", type=int, default=120)
    args = parser.parse_args()

    tables = DEFAULT_TABLES if args.tables == "," else [t.strip() for t in args.tables.split(",") if t.strip()]
    outdir = ensure_dir(args.outdir)

    log = {
        "source": "WITS Product Concordance",
        "base_url": BASE_URL,
        "access_date": today_str(),
        "requested_tables": tables,
        "downloaded": [],
        "errors": [],
    }

    for table in tables:
        url = f"{BASE_URL}/{table}"
        dest = outdir / table
        try:
            download(url, dest, timeout=args.timeout)
            log["downloaded"].append(str(dest))
            time.sleep(args.sleep)
        except Exception as exc:  # noqa: BLE001
            log["errors"].append(f"{url}: {exc}")

    write_access_log(outdir / "access_log.json", log)
    if log["errors"]:
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
