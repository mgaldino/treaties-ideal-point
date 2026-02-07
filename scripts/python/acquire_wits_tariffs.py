#!/usr/bin/env python3
"""Acquire WITS/TRAINS tariff data via API (pilot-friendly).

This script downloads raw API responses and preserves them in-repo.
It supports optional basic authentication via env vars WITS_USER/WITS_PASS.

Example:
  python3 scripts/python/acquire_wits_tariffs.py --reporter-iso3 BRA --year 1990
  python3 scripts/python/run_with_env.py -- python3 scripts/python/acquire_wits_tariffs.py --reporter-iso3 BRA --year 1990
"""
from __future__ import annotations

import argparse
import base64
import json
import os
import pathlib
import sys
import time
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
sys.path.append(str(SCRIPT_DIR))
from _common import DEFAULT_UA, ensure_dir, today_str, write_access_log  # noqa: E402


BASE_URL = "https://wits.worldbank.org"
API_URL = f"{BASE_URL}/API/V1"


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


def _auth_header() -> dict:
    user = os.environ.get("WITS_USER")
    password = os.environ.get("WITS_PASS")
    if user and password:
        creds = f"{user}:{password}".encode("utf-8")
        token = base64.b64encode(creds).decode("ascii")
        return {"Authorization": f"Basic {token}"}
    return {}


def _fetch_bytes(auth_header: dict, url: str, timeout: int = 120) -> bytes:
    headers = {"User-Agent": DEFAULT_UA, **auth_header}
    req = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read()


def _strip_ns(tag: str) -> str:
    return tag.split("}", 1)[-1].lower()


def _parse_country_metadata(xml_bytes: bytes) -> list[dict]:
    root = ET.fromstring(xml_bytes)
    records = []
    for elem in root.iter():
        if _strip_ns(elem.tag) != "country":
            continue
        record = {k.lower(): v for k, v in elem.attrib.items()}
        for child in list(elem):
            key = _strip_ns(child.tag)
            val = (child.text or "").strip()
            if key:
                record[key.lower()] = val
        if "countrycode" in record and ("iso3code" in record or "iso3" in record):
            records.append(record)
    return records


def _find_country_code(records: list[dict], iso3: str) -> str:
    iso3 = iso3.upper()
    for rec in records:
        iso3_code = rec.get("iso3code") or rec.get("iso3", "")
        if iso3_code.upper() == iso3:
            return rec.get("countrycode", "")
    return ""


def _parse_partner_codes(xml_bytes: bytes) -> list[str]:
    root = ET.fromstring(xml_bytes)
    codes: set[str] = set()
    for elem in root.iter():
        for child in list(elem):
            key = _strip_ns(child.tag)
            val = (child.text or "").strip()
            if not val:
                continue
            if key == "partnerlist":
                parts = [p for p in val.split(";") if p]
                codes.update(parts)
            if key == "partnercode":
                codes.add(val)
            elif key == "partner" and val.isdigit():
                codes.add(val)
    return sorted(codes)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--reporter-iso3", required=True)
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--outdir", default="data/raw/tariffs/wits_trn")
    parser.add_argument("--partner-list", default="")
    parser.add_argument("--max-partners", type=int, default=0)
    parser.add_argument("--sleep", type=float, default=0.5)
    parser.add_argument("--timeout", type=int, default=180)
    args = parser.parse_args()

    _load_env(SCRIPT_DIR.parent.parent / ".env")
    auth_header = _auth_header()

    reporter_iso3 = args.reporter_iso3.upper()
    year = args.year
    out_root = ensure_dir(args.outdir) / reporter_iso3 / str(year)
    ensure_dir(out_root)

    access_log = {
        "source": "WITS/TRAINS API",
        "base_url": API_URL,
        "reporter_iso3": reporter_iso3,
        "year": year,
        "access_date": today_str(),
        "requests": [],
        "errors": [],
    }

    # Metadata: countries list (ISO3 -> WITS countrycode)
    countries_url = f"{API_URL}/wits/datasource/trn/country/all"
    access_log["countries_url"] = countries_url
    try:
        xml_bytes = _fetch_bytes(auth_header, countries_url, timeout=args.timeout)
        (out_root / "countries_metadata.xml").write_bytes(xml_bytes)
        country_records = _parse_country_metadata(xml_bytes)
        reporter_code = _find_country_code(country_records, reporter_iso3)
        if not reporter_code:
            raise RuntimeError(f"Reporter ISO3 not found in metadata: {reporter_iso3}")
        access_log["reporter_code"] = reporter_code
    except Exception as exc:  # noqa: BLE001
        access_log["errors"].append(f"Failed to fetch country metadata: {exc}")
        write_access_log(out_root / "access_log.json", access_log)
        return 1

    # Partner list (from args or data availability)
    partners: list[str] = []
    if args.partner_list:
        partners = [p.strip() for p in args.partner_list.split(",") if p.strip()]
    else:
        availability_url = (
            f"{API_URL}/wits/datasource/trn/dataavailability/country/{reporter_code}/year/{year}"
        )
        access_log["data_availability_url"] = availability_url
        try:
            xml_bytes = _fetch_bytes(auth_header, availability_url, timeout=args.timeout)
            (out_root / "data_availability.xml").write_bytes(xml_bytes)
            partners = _parse_partner_codes(xml_bytes)
        except Exception as exc:  # noqa: BLE001
            access_log["errors"].append(f"Failed to fetch data availability: {exc}")
            write_access_log(out_root / "access_log.json", access_log)
            return 1

    if not partners:
        access_log["errors"].append("No partner codes found.")
        write_access_log(out_root / "access_log.json", access_log)
        return 1

    if args.max_partners and args.max_partners > 0:
        partners = partners[: args.max_partners]
        access_log["max_partners"] = args.max_partners

    access_log["partner_count"] = len(partners)

    # Data requests: one partner at a time (all products)
    access_log["datatype"] = "reported"
    access_log["product"] = "ALL"
    for partner in partners:
        data_url = (
            f"{API_URL}/SDMX/V21/datasource/TRN/reporter/{reporter_code}/"
            f"partner/{partner}/product/ALL/year/{year}/datatype/reported?format=JSON"
        )
        payload = {"url": data_url, "partner": partner, "bytes": 0, "status": "ok"}
        try:
            data_bytes = _fetch_bytes(auth_header, data_url, timeout=args.timeout)
            dest = out_root / f"tariffs_partner_{partner}.json"
            dest.write_bytes(data_bytes)
            payload["bytes"] = dest.stat().st_size
        except Exception as exc:  # noqa: BLE001
            payload["status"] = "error"
            payload["error"] = str(exc)
            access_log["errors"].append(f"Partner {partner}: {exc}")
        access_log["requests"].append(payload)
        time.sleep(args.sleep)

    write_access_log(out_root / "access_log.json", access_log)
    return 0 if not access_log["errors"] else 2


if __name__ == "__main__":
    raise SystemExit(main())
