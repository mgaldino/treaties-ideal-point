from __future__ import annotations

import argparse
import pathlib

from _common import download_from_landing, download_file, print_summary, write_access_log

SOURCE_NAME = "WTO RTA Database"
LANDING_URL = "https://data.wto.org/en/dataset/ext_rta"
PORTAL_URL = "https://rtais.wto.org/"
DIRECT_EXPORT_URL = "https://rtais.wto.org/UI/ExportAllRTAList.aspx"

DATA_EXTS = [".zip", ".csv", ".xlsx", ".xls"]


def main() -> int:
    parser = argparse.ArgumentParser(description="Download WTO RTA data files.")
    parser.add_argument("--out", default="data/raw/wto_rta", help="Output directory")
    parser.add_argument("--dry-run", action="store_true", help="List candidate links only")
    args = parser.parse_args()

    out_dir = pathlib.Path(args.out)
    payload = download_from_landing(
        source_name=SOURCE_NAME,
        landing_url=LANDING_URL,
        out_dir=out_dir,
        exts=DATA_EXTS,
        dry_run=args.dry_run,
        include_links=[PORTAL_URL, DIRECT_EXPORT_URL],
    )

    if payload.get("errors"):
        try:
            out_dir.mkdir(parents=True, exist_ok=True)
            dest = out_dir / "WTO_RTA_AllRTAs.xlsx"
            download_file(DIRECT_EXPORT_URL, dest)
            payload["downloaded_files"].append(str(dest))
            payload["notes"].append("Downloaded via direct RTAIS export endpoint.")
            payload["errors"] = []
        except Exception as exc:  # noqa: BLE001
            payload["errors"].append(f"Direct export download failed: {exc}")
            payload["notes"].append(
                "If no direct download links are detected, use the WTO Data Portal dataset page to export the data (Excel/CSV) and place the file in this folder."
            )

    log_path = out_dir / "access_log.json"
    write_access_log(log_path, payload)
    print_summary(payload)

    return 0 if not payload.get("errors") else 1


if __name__ == "__main__":
    raise SystemExit(main())
