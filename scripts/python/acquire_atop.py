from __future__ import annotations

import argparse
import pathlib

from _common import download_from_landing, print_summary, write_access_log

SOURCE_NAME = "ATOP 5.1"
LANDING_URLS = [
    "https://www.atopdata.org/data.html",
    "http://www.atopdata.org/data.html",
]

DATA_EXTS = [".zip", ".csv", ".xlsx", ".xls", ".dta", ".sav", ".rds", ".rda", ".pdf"]


def main() -> int:
    parser = argparse.ArgumentParser(description="Download ATOP 5.1 data files.")
    parser.add_argument("--out", default="data/raw/atop", help="Output directory")
    parser.add_argument("--dry-run", action="store_true", help="List candidate links only")
    args = parser.parse_args()

    out_dir = pathlib.Path(args.out)
    payload = None

    for url in LANDING_URLS:
        payload = download_from_landing(
            source_name=SOURCE_NAME,
            landing_url=url,
            out_dir=out_dir,
            exts=DATA_EXTS,
            dry_run=args.dry_run,
        )
        if not payload.get("errors"):
            break

    if payload is None:
        return 1

    if payload.get("errors") and "No downloadable files" in " ".join(payload.get("errors", [])):
        payload["notes"].append(
            "No direct download links detected. If needed, manually download from the ATOP data page and place files in this folder."
        )

    log_path = out_dir / "access_log.json"
    write_access_log(log_path, payload)
    print_summary(payload)

    return 0 if not payload.get("errors") else 1


if __name__ == "__main__":
    raise SystemExit(main())
