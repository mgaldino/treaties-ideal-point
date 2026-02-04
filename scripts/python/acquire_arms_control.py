#!/usr/bin/env python3
"""Acquire arms control / disarmament treaty ratification data.

Scrapes participant tables from treaties.un.org Chapter XXVI (Disarmament)
for multilateral arms control treaties.

Note: NPT (1968) and BWC (1972) are NOT deposited with the UN Secretary-General,
so they are not in Chapter XXVI. This script covers treaties available from
treaties.un.org. NPT and BWC data could be added later from IAEA / BWC ISU.

Outputs:
  data/raw/arms_control/arms_control_ratifications.csv
  data/raw/arms_control/access_log.json
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import json
import os
import re
import sys
import time
from html.parser import HTMLParser

DEFAULT_UA = "IdealPointDataBot/0.1 (contact: local)"

BASE_URL = "https://treaties.un.org"
VIEW_DETAILS = (
    BASE_URL
    + "/Pages/ViewDetails.aspx?src=TREATY&mtdsg_no={mtdsg}&chapter={chapter}&clang=_en"
)

# Arms control treaties from Chapter XXVI + protocols
TREATIES = [
    # ENMOD
    {"mtdsg": "XXVI-1", "chapter": 26, "treaty_id": "enmod",
     "treaty_name": "Convention on the prohibition of military or any other hostile use of environmental modification techniques",
     "open_year": 1976},
    # CCW + Protocols
    {"mtdsg": "XXVI-2", "chapter": 26, "treaty_id": "ccw",
     "treaty_name": "Convention on Certain Conventional Weapons (with Protocols I, II and III)",
     "open_year": 1980},
    {"mtdsg": "XXVI-2-a", "chapter": 26, "treaty_id": "ccw_p4",
     "treaty_name": "CCW Protocol IV — Blinding Laser Weapons",
     "open_year": 1995},
    {"mtdsg": "XXVI-2-b", "chapter": 26, "treaty_id": "ccw_p2a",
     "treaty_name": "CCW Amended Protocol II — Mines, Booby-Traps and Other Devices",
     "open_year": 1996},
    {"mtdsg": "XXVI-2-c", "chapter": 26, "treaty_id": "ccw_art1",
     "treaty_name": "CCW Amendment to Article I",
     "open_year": 2001},
    {"mtdsg": "XXVI-2-d", "chapter": 26, "treaty_id": "ccw_p5",
     "treaty_name": "CCW Protocol V — Explosive Remnants of War",
     "open_year": 2003},
    # CWC
    {"mtdsg": "XXVI-3", "chapter": 26, "treaty_id": "cwc",
     "treaty_name": "Convention on the Prohibition of Chemical Weapons",
     "open_year": 1993},
    # CTBT
    {"mtdsg": "XXVI-4", "chapter": 26, "treaty_id": "ctbt",
     "treaty_name": "Comprehensive Nuclear-Test-Ban Treaty",
     "open_year": 1996},
    # Ottawa Treaty (Anti-Personnel Mines)
    {"mtdsg": "XXVI-5", "chapter": 26, "treaty_id": "ottawa",
     "treaty_name": "Convention on the Prohibition of Anti-Personnel Mines (Ottawa Treaty)",
     "open_year": 1997},
    # CCM (Cluster Munitions)
    {"mtdsg": "XXVI-6", "chapter": 26, "treaty_id": "ccm",
     "treaty_name": "Convention on Cluster Munitions",
     "open_year": 2008},
    # ATT
    {"mtdsg": "XXVI-8", "chapter": 26, "treaty_id": "att",
     "treaty_name": "Arms Trade Treaty",
     "open_year": 2013},
    # TPNW
    {"mtdsg": "XXVI-9", "chapter": 26, "treaty_id": "tpnw",
     "treaty_name": "Treaty on the Prohibition of Nuclear Weapons",
     "open_year": 2017},
]


def _utc_now_iso() -> str:
    return dt.datetime.utcnow().replace(microsecond=0).isoformat() + "Z"


def _today_str() -> str:
    return dt.date.today().isoformat()


def _fetch_html(url: str, timeout: int = 60) -> str:
    import urllib.request

    req = urllib.request.Request(url, headers={"User-Agent": DEFAULT_UA})
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read().decode("utf-8", errors="ignore")


def _clean_text(text: str) -> str:
    text = re.sub(r"\[[^\]]*\]", "", text)
    text = text.replace("\xa0", " ")
    text = re.sub(r"\s+", " ", text).strip()
    return text


def _clean_country_name(text: str) -> str:
    """Clean country name, stripping trailing footnote reference numbers."""
    text = _clean_text(text)
    text = re.sub(r"(?:\s+\d+(?:\s*,\s*\d+)*)+\s*$", "", text)
    return text.strip()


def _parse_date(text: str) -> str | None:
    text = _clean_text(text)
    if not text or text in {"", "—", "-", "–"}:
        return None
    text_no_suffix = re.sub(r"\s+[a-zA-Z]$", "", text.strip())
    patterns = [
        "%d %b %Y", "%d %B %Y", "%B %d, %Y", "%b %d, %Y",
        "%Y-%m-%d", "%d/%m/%Y",
    ]
    for fmt in patterns:
        try:
            return dt.datetime.strptime(text_no_suffix, fmt).date().isoformat()
        except ValueError:
            continue
    m = re.search(r"\d{1,2}\s+\w+\s+\d{4}", text)
    if m:
        for fmt in ["%d %b %Y", "%d %B %Y"]:
            try:
                return dt.datetime.strptime(m.group(0), fmt).date().isoformat()
            except ValueError:
                continue
    return None


def _extract_action_type(text: str) -> str:
    text = _clean_text(text).lower()
    if text.endswith(" a"):
        return "Accession"
    if text.endswith(" d"):
        return "Succession"
    if "accession" in text:
        return "Accession"
    if "succession" in text:
        return "Succession"
    return "Ratification"


class UNTreatyTableParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.rows: list[list[str]] = []
        self._in_table = False
        self._in_row = False
        self._in_cell = False
        self._current_row: list[str] = []
        self._current_cell: list[str] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        if tag == "table":
            self._in_table = True
        elif tag == "tr" and self._in_table:
            self._in_row = True
            self._current_row = []
        elif tag in ("td", "th") and self._in_row:
            self._in_cell = True
            self._current_cell = []

    def handle_data(self, data: str) -> None:
        if self._in_cell:
            self._current_cell.append(data)

    def handle_endtag(self, tag: str) -> None:
        if tag in ("td", "th") and self._in_cell:
            cell_text = _clean_text("".join(self._current_cell))
            self._current_row.append(cell_text)
            self._current_cell = []
            self._in_cell = False
        elif tag == "tr" and self._in_row:
            if self._current_row and any(c.strip() for c in self._current_row):
                self.rows.append(self._current_row)
            self._current_row = []
            self._in_row = False
        elif tag == "table":
            self._in_table = False


def _find_participant_rows(all_rows: list[list[str]]) -> tuple[list[str], list[list[str]]]:
    for i, row in enumerate(all_rows):
        row_lower = [c.lower() for c in row]
        if any("participant" in c for c in row_lower):
            return row, all_rows[i + 1:]
    if all_rows:
        return all_rows[0], all_rows[1:]
    return [], []


def _parse_participants(html: str) -> list[dict]:
    parser = UNTreatyTableParser()
    parser.feed(html)

    header, data_rows = _find_participant_rows(parser.rows)
    if not header:
        return []

    header_lower = [h.lower() for h in header]
    participant_idx = 0
    signature_idx = None
    ratification_idx = None

    for i, h in enumerate(header_lower):
        if "participant" in h:
            participant_idx = i
        elif "signature" in h:
            signature_idx = i
        elif any(kw in h for kw in ("ratification", "accession", "acceptance", "approval", "formal confirmation")):
            ratification_idx = i

    if signature_idx is None and len(header) >= 2:
        signature_idx = 1
    if ratification_idx is None and len(header) >= 3:
        ratification_idx = 2

    records = []
    for row in data_rows:
        if len(row) <= participant_idx:
            continue
        country = _clean_country_name(row[participant_idx])
        if not country or country.lower() in {"participant", ""}:
            continue
        if re.match(r"^\d+$", country) or "signator" in country.lower():
            continue

        sig_text = row[signature_idx] if signature_idx is not None and signature_idx < len(row) else ""
        rat_text = row[ratification_idx] if ratification_idx is not None and ratification_idx < len(row) else ""

        signature_date = _parse_date(sig_text)
        action_date = _parse_date(rat_text)
        action_type = _extract_action_type(rat_text) if action_date else ""

        if signature_date or action_date:
            records.append({
                "country_name": country,
                "signature_date": signature_date or "",
                "action_date": action_date or "",
                "action_type": action_type,
            })

    return records


def acquire_treaties(
    out_dir: str,
    delay: float = 2.0,
    limit: int | None = None,
    dry_run: bool = False,
) -> dict:
    os.makedirs(out_dir, exist_ok=True)
    html_dir = os.path.join(out_dir, "html")
    os.makedirs(html_dir, exist_ok=True)

    treaties = TREATIES[:limit] if limit else TREATIES
    all_records: list[dict] = []
    errors: list[str] = []
    notes: list[str] = []

    for info in treaties:
        url = VIEW_DETAILS.format(mtdsg=info["mtdsg"], chapter=info["chapter"])
        treaty_id = info["treaty_id"]
        treaty_name = info["treaty_name"]
        open_year = info["open_year"]

        if dry_run:
            notes.append(f"[dry-run] Would fetch {treaty_id} from {url}")
            continue

        print(f"Fetching {treaty_id} ({info['mtdsg']})...")
        html_path = os.path.join(html_dir, f"{treaty_id}.html")

        try:
            html = _fetch_html(url, timeout=90)
            with open(html_path, "w", encoding="utf-8") as f:
                f.write(html)
        except Exception as exc:
            errors.append(f"Failed to fetch {treaty_id}: {exc}")
            time.sleep(delay)
            continue

        records = _parse_participants(html)
        for rec in records:
            rec["treaty_id"] = treaty_id
            rec["treaty_name"] = treaty_name
            rec["treaty_open_year"] = open_year
            rec["mtdsg_no"] = info["mtdsg"]
            rec["source_url"] = url

        all_records.extend(records)
        notes.append(f"{treaty_id}: {len(records)} participants parsed")
        print(f"  -> {len(records)} participants")
        time.sleep(delay)

    # Deduplicate: prefer entries with action_date over signature-only
    deduped: dict[tuple[str, str], dict] = {}
    for rec in all_records:
        key = (rec["country_name"], rec["treaty_id"])
        existing = deduped.get(key)
        if existing is None:
            deduped[key] = rec
        else:
            if not existing.get("action_date") and rec.get("action_date"):
                deduped[key] = rec
    all_records = list(deduped.values())

    csv_path = os.path.join(out_dir, "arms_control_ratifications.csv")
    fieldnames = [
        "country_name", "treaty_id", "treaty_name", "action_type",
        "action_date", "signature_date", "treaty_open_year", "mtdsg_no", "source_url",
    ]

    if all_records:
        with open(csv_path, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
            writer.writeheader()
            writer.writerows(all_records)
        notes.append(f"Wrote {len(all_records)} total records to {csv_path}")
    else:
        notes.append("No records parsed.")

    notes.append(
        "Note: NPT (1968) and BWC (1972) are not in the UN Treaty Collection "
        "(different depositaries). Consider adding from IAEA/BWC ISU separately."
    )

    payload = {
        "source_name": "UN Treaty Collection — Chapter XXVI (Disarmament)",
        "landing_url": "https://treaties.un.org/Pages/Treaties.aspx?id=26&subid=A&clang=_en",
        "access_date": _today_str(),
        "downloaded_files": [csv_path] if all_records else [],
        "treaties_attempted": len(treaties),
        "total_records": len(all_records),
        "notes": notes,
        "errors": errors,
    }
    return payload


def validate(csv_path: str) -> list[str]:
    issues = []
    if not os.path.exists(csv_path):
        issues.append(f"Output file not found: {csv_path}")
        return issues

    with open(csv_path, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows = list(reader)

    countries = {r["country_name"] for r in rows}
    treaties = {r["treaty_id"] for r in rows}

    # CWC should have ~193 parties
    cwc_count = sum(1 for r in rows if r["treaty_id"] == "cwc" and r["action_date"])
    if cwc_count < 150:
        issues.append(f"CWC has only {cwc_count} ratifications (expected ~193)")

    # CTBT should have ~170+ ratifications
    ctbt_count = sum(1 for r in rows if r["treaty_id"] == "ctbt" and r["action_date"])
    if ctbt_count < 140:
        issues.append(f"CTBT has only {ctbt_count} ratifications (expected ~170+)")

    if len(countries) < 100:
        issues.append(f"Only {len(countries)} unique countries (expected >= 100)")

    # Check no action_date before treaty open year
    for r in rows:
        if r["action_date"] and r["treaty_open_year"]:
            try:
                action_year = int(r["action_date"][:4])
                open_year = int(r["treaty_open_year"])
                if action_year < open_year - 1:
                    issues.append(
                        f"Action date {r['action_date']} before open year {open_year} "
                        f"for {r['country_name']} / {r['treaty_id']}"
                    )
            except (ValueError, IndexError):
                pass

    return issues


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Acquire arms control treaty ratification data."
    )
    parser.add_argument("--out", default="data/raw/arms_control", help="Output directory")
    parser.add_argument("--delay", type=float, default=2.0, help="Delay between requests (seconds)")
    parser.add_argument("--limit", type=int, default=None, help="Limit number of treaties (for testing)")
    parser.add_argument("--dry-run", action="store_true", help="List URLs only")
    parser.add_argument("--skip-validate", action="store_true", help="Skip validation checks")
    args = parser.parse_args()

    payload = acquire_treaties(
        out_dir=args.out,
        delay=args.delay,
        limit=args.limit,
        dry_run=args.dry_run,
    )

    log_path = os.path.join(args.out, "access_log.json")
    with open(log_path, "w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2, sort_keys=True)

    csv_path = os.path.join(args.out, "arms_control_ratifications.csv")
    if not args.dry_run and not args.skip_validate and os.path.exists(csv_path):
        issues = validate(csv_path)
        if issues:
            print("\nValidation issues:")
            for issue in issues:
                print(f"  - {issue}")
            payload["validation_issues"] = issues
            with open(log_path, "w", encoding="utf-8") as f:
                json.dump(payload, f, indent=2, sort_keys=True)
        else:
            print("\nValidation passed.")

    if payload.get("errors"):
        print("\nErrors:")
        for err in payload["errors"]:
            print(f"  - {err}")

    print(f"\nTotal records: {payload.get('total_records', 0)}")
    print(f"Access log: {log_path}")

    return 0 if not payload.get("errors") else 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        sys.exit(1)
