#!/usr/bin/env python3
"""Batch acquisition for WITS/TRAINS tariffs (raw only).

Uses credentials from repo-root .env (WITS_USER/WITS_PASS). Designed for
sequential, resumable pulls with basic diagnostics and a manifest CSV.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import json
import os
import pathlib
import re
import socket
import subprocess
import sys
import time

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent.parent
sys.path.append(str(SCRIPT_DIR))
from _common import ensure_dir  # noqa: E402

G20_REPORTERS = [
    "ARG",
    "AUS",
    "BRA",
    "CAN",
    "CHN",
    "FRA",
    "DEU",
    "IND",
    "IDN",
    "ITA",
    "JPN",
    "KOR",
    "MEX",
    "RUS",
    "SAU",
    "ZAF",
    "TUR",
    "GBR",
    "USA",
]

TRANSIENT_PATTERNS = [
    "timed out",
    "timeout",
    "temporary failure",
    "connection reset",
    "connection aborted",
    "remote end closed",
    "http error 502",
    "http error 503",
    "http error 504",
    "http error 429",
    "http error 403",
    "name or service not known",
    "nodename nor servname provided",
    "gaierror",
    "dns",
]


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


def _has_credentials() -> bool:
    return bool(os.environ.get("WITS_USER") and os.environ.get("WITS_PASS"))


def parse_reporters(arg: str | None) -> list[str]:
    if not arg:
        return G20_REPORTERS[:]
    cleaned = arg.replace(";", ",")
    parts = re.split(r"[,\s]+", cleaned.strip())
    reporters = [part.strip().upper() for part in parts if part.strip()]
    return reporters


def should_skip(out_dir: pathlib.Path) -> bool:
    if not out_dir.exists():
        return False
    if any(out_dir.glob("tariffs_partner_*.json")):
        return True
    return False


def load_access_log(path: pathlib.Path) -> dict | None:
    if not path.exists():
        return None
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:  # noqa: BLE001
        return None


def classify_status(exit_code: int, errors: list[str], skipped: bool) -> str:
    if skipped:
        return "skipped"
    if not errors and exit_code == 0:
        return "success"
    text = " ".join(errors).lower()
    if "data not found" in text or "no partner codes found" in text:
        return "no_data"
    if "http error 403" in text or "forbidden" in text:
        return "auth_error"
    if exit_code == 2:
        return "partial_error"
    return "failed"


def is_transient_error(errors: list[str]) -> bool:
    text = " ".join(errors).lower()
    return any(pat in text for pat in TRANSIENT_PATTERNS)


def diagnose_network(host: str = "wits.worldbank.org") -> dict:
    diag = {
        "dns_resolves": False,
        "dns_ip": "",
        "dns_error": "",
    }
    try:
        ip = socket.gethostbyname(host)
        diag["dns_resolves"] = True
        diag["dns_ip"] = ip
    except Exception as exc:  # noqa: BLE001
        diag["dns_error"] = str(exc)
    return diag


def build_command(args: argparse.Namespace, reporter: str, year: int) -> list[str]:
    cmd = [
        sys.executable,
        str(SCRIPT_DIR / "acquire_wits_tariffs.py"),
        "--reporter-iso3",
        reporter,
        "--year",
        str(year),
        "--outdir",
        args.outdir,
        "--sleep",
        str(args.sleep),
        "--timeout",
        str(args.timeout),
    ]
    if args.partner_list:
        cmd.extend(["--partner-list", args.partner_list])
    if args.max_partners and args.max_partners > 0:
        cmd.extend(["--max-partners", str(args.max_partners)])
    return cmd


def write_manifest_row(path: pathlib.Path, row: dict, fieldnames: list[str]) -> None:
    ensure_dir(path.parent)
    exists = path.exists()
    with open(path, "a", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        if not exists:
            writer.writeheader()
        writer.writerow(row)


def log_line(path: pathlib.Path, message: str) -> None:
    ensure_dir(path.parent)
    with open(path, "a", encoding="utf-8") as f:
        f.write(message.rstrip() + "\n")


def run_one(
    reporter: str,
    year: int,
    args: argparse.Namespace,
    log_path: pathlib.Path,
    retry_sleep: float,
) -> tuple[int, list[str], dict | None, dict | None, int]:
    attempts = 0
    diagnostics = None
    access_log = None
    errors: list[str] = []
    exit_code = 1

    while attempts < 2:
        attempts += 1
        cmd = build_command(args, reporter, year)
        log_line(log_path, f"[{dt.datetime.utcnow().isoformat()}Z] RUN {reporter} {year}")
        result = subprocess.run(cmd, capture_output=True, text=True)
        exit_code = result.returncode

        out_dir = pathlib.Path(args.outdir) / reporter / str(year)
        access_log = load_access_log(out_dir / "access_log.json")
        errors = []
        if access_log and access_log.get("errors"):
            errors = [str(err) for err in access_log.get("errors", [])]

        if exit_code == 0 and not errors:
            break

        if not is_transient_error(errors):
            break

        diagnostics = diagnose_network()
        log_line(log_path, f"[{dt.datetime.utcnow().isoformat()}Z] DIAG {diagnostics}")
        if not diagnostics.get("dns_resolves"):
            break

        log_line(log_path, f"[{dt.datetime.utcnow().isoformat()}Z] RETRY after {retry_sleep}s")
        time.sleep(retry_sleep)

    return exit_code, errors, access_log, diagnostics, attempts - 1


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--reporters", default="")
    parser.add_argument("--start-year", type=int, default=1990)
    parser.add_argument("--end-year", type=int, default=2024)
    parser.add_argument("--outdir", default="data/raw/tariffs/wits_trn")
    parser.add_argument("--sleep", type=float, default=0.5)
    parser.add_argument("--timeout", type=int, default=180)
    parser.add_argument("--partner-list", default="")
    parser.add_argument("--max-partners", type=int, default=0)
    parser.add_argument("--skip-existing", action="store_true", default=True)
    parser.add_argument("--no-skip-existing", action="store_false", dest="skip_existing")
    parser.add_argument("--manifest", default="")
    parser.add_argument("--retry-sleep", type=float, default=30.0)
    args = parser.parse_args()

    _load_env(REPO_ROOT / ".env")
    if not _has_credentials():
        print("Missing WITS credentials in environment (.env).", file=sys.stderr)
        return 2

    reporters = parse_reporters(args.reporters)
    years = list(range(args.start_year, args.end_year + 1))

    timestamp = dt.datetime.utcnow().strftime("%Y%m%dT%H%M%SZ")
    log_path = REPO_ROOT / "logs" / f"wits_batch_{timestamp}.log"
    if args.manifest:
        manifest_path = pathlib.Path(args.manifest)
    else:
        manifest_path = REPO_ROOT / "data" / "processed" / f"wits_tariffs_manifest_{timestamp}.csv"

    fieldnames = [
        "timestamp_utc",
        "reporter_iso3",
        "year",
        "status",
        "exit_code",
        "retry_count",
        "skipped",
        "partner_count",
        "error_count",
        "errors",
        "dns_resolves",
        "dns_ip",
        "dns_error",
        "access_log",
        "out_dir",
    ]

    any_failures = False

    for reporter in reporters:
        for year in years:
            out_dir = pathlib.Path(args.outdir) / reporter / str(year)
            skipped = False
            if args.skip_existing and should_skip(out_dir):
                skipped = True
                row = {
                    "timestamp_utc": dt.datetime.utcnow().isoformat() + "Z",
                    "reporter_iso3": reporter,
                    "year": year,
                    "status": "skipped",
                    "exit_code": 0,
                    "retry_count": 0,
                    "skipped": True,
                    "partner_count": "",
                    "error_count": 0,
                    "errors": "",
                    "dns_resolves": "",
                    "dns_ip": "",
                    "dns_error": "",
                    "access_log": str(out_dir / "access_log.json"),
                    "out_dir": str(out_dir),
                }
                write_manifest_row(manifest_path, row, fieldnames)
                log_line(log_path, f"[{dt.datetime.utcnow().isoformat()}Z] SKIP {reporter} {year}")
                continue

            exit_code, errors, access_log, diagnostics, retry_count = run_one(
                reporter,
                year,
                args,
                log_path,
                args.retry_sleep,
            )

            partner_count = ""
            if access_log and access_log.get("partner_count") is not None:
                partner_count = access_log.get("partner_count")

            status = classify_status(exit_code, errors, skipped)
            if status not in ("success", "skipped", "no_data"):
                any_failures = True

            row = {
                "timestamp_utc": dt.datetime.utcnow().isoformat() + "Z",
                "reporter_iso3": reporter,
                "year": year,
                "status": status,
                "exit_code": exit_code,
                "retry_count": retry_count,
                "skipped": skipped,
                "partner_count": partner_count,
                "error_count": len(errors),
                "errors": " | ".join(errors),
                "dns_resolves": diagnostics.get("dns_resolves") if diagnostics else "",
                "dns_ip": diagnostics.get("dns_ip") if diagnostics else "",
                "dns_error": diagnostics.get("dns_error") if diagnostics else "",
                "access_log": str(out_dir / "access_log.json"),
                "out_dir": str(out_dir),
            }
            write_manifest_row(manifest_path, row, fieldnames)

    if any_failures:
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
