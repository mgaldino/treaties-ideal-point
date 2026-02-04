from __future__ import annotations

import argparse
import pathlib

from _common import download_from_landing, print_summary, write_access_log

SOURCE_NAME = "DESTA v2.3"
LANDING_URL = "https://www.designoftradeagreements.org/downloads/"

DATA_EXTS = [".zip", ".csv", ".xlsx", ".xls", ".dta", ".sav", ".rds", ".rda"]


def main() -> int:
    parser = argparse.ArgumentParser(description="Download DESTA v2.3 data files.")
    parser.add_argument("--out", default="data/raw/desta", help="Output directory")
    parser.add_argument("--dry-run", action="store_true", help="List candidate links only")
    args = parser.parse_args()

    out_dir = pathlib.Path(args.out)
    payload = download_from_landing(
        source_name=SOURCE_NAME,
        landing_url=LANDING_URL,
        out_dir=out_dir,
        exts=DATA_EXTS,
        dry_run=args.dry_run,
    )

    log_path = out_dir / "access_log.json"
    write_access_log(log_path, payload)
    print_summary(payload)

    return 0 if not payload.get("errors") else 1


if __name__ == "__main__":
    raise SystemExit(main())
