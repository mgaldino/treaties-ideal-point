#!/usr/bin/env python3
"""Acquire UN human rights treaty ratification data from treaties.un.org (Chapter IV).

Scrapes participant tables for the 9 core UN human rights treaties + optional protocols
(~18 instruments). Each treaty has a ViewDetails page listing countries with signature
and ratification/accession/succession dates.

Outputs:
  data/raw/un_hr_treaties/un_hr_ratifications.csv
  data/raw/un_hr_treaties/access_log.json
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

# ---- helpers (standard library only) ----

DEFAULT_UA = "IdealPointDataBot/0.1 (contact: local)"

BASE_URL = "https://treaties.un.org"
VIEW_DETAILS = (
    BASE_URL
    + "/Pages/ViewDetails.aspx?src=TREATY&mtdsg_no={mtdsg}&chapter={chapter}&clang=_en"
)

# 9 core treaties + optional protocols from Chapter IV
TREATIES = [
    # CERD
    {"mtdsg": "IV-2", "chapter": 4, "treaty_id": "cerd",
     "treaty_name": "International Convention on the Elimination of All Forms of Racial Discrimination",
     "open_year": 1965},
    # ICESCR
    {"mtdsg": "IV-3", "chapter": 4, "treaty_id": "icescr",
     "treaty_name": "International Covenant on Economic, Social and Cultural Rights",
     "open_year": 1966},
    {"mtdsg": "IV-3-a", "chapter": 4, "treaty_id": "icescr_op",
     "treaty_name": "Optional Protocol to the ICESCR",
     "open_year": 2008},
    # ICCPR
    {"mtdsg": "IV-4", "chapter": 4, "treaty_id": "iccpr",
     "treaty_name": "International Covenant on Civil and Political Rights",
     "open_year": 1966},
    {"mtdsg": "IV-5", "chapter": 4, "treaty_id": "iccpr_op1",
     "treaty_name": "Optional Protocol to the ICCPR",
     "open_year": 1966},
    {"mtdsg": "IV-12", "chapter": 4, "treaty_id": "iccpr_op2",
     "treaty_name": "Second Optional Protocol to the ICCPR (abolition of death penalty)",
     "open_year": 1989},
    # CEDAW
    {"mtdsg": "IV-8", "chapter": 4, "treaty_id": "cedaw",
     "treaty_name": "Convention on the Elimination of All Forms of Discrimination Against Women",
     "open_year": 1979},
    {"mtdsg": "IV-8-b", "chapter": 4, "treaty_id": "cedaw_op",
     "treaty_name": "Optional Protocol to CEDAW",
     "open_year": 1999},
    # CAT
    {"mtdsg": "IV-9", "chapter": 4, "treaty_id": "cat",
     "treaty_name": "Convention Against Torture and Other Cruel, Inhuman or Degrading Treatment",
     "open_year": 1984},
    {"mtdsg": "IV-9-b", "chapter": 4, "treaty_id": "cat_op",
     "treaty_name": "Optional Protocol to the Convention Against Torture (OPCAT)",
     "open_year": 2002},
    # CRC
    {"mtdsg": "IV-11", "chapter": 4, "treaty_id": "crc",
     "treaty_name": "Convention on the Rights of the Child",
     "open_year": 1989},
    {"mtdsg": "IV-11-b", "chapter": 4, "treaty_id": "crc_op_ac",
     "treaty_name": "Optional Protocol to the CRC on children in armed conflict",
     "open_year": 2000},
    {"mtdsg": "IV-11-c", "chapter": 4, "treaty_id": "crc_op_sc",
     "treaty_name": "Optional Protocol to the CRC on sale of children",
     "open_year": 2000},
    {"mtdsg": "IV-11-d", "chapter": 4, "treaty_id": "crc_op_ic",
     "treaty_name": "Optional Protocol to the CRC on a communications procedure",
     "open_year": 2011},
    # CMW
    {"mtdsg": "IV-13", "chapter": 4, "treaty_id": "cmw",
     "treaty_name": "International Convention on the Protection of the Rights of All Migrant Workers",
     "open_year": 1990},
    # CRPD
    {"mtdsg": "IV-15", "chapter": 4, "treaty_id": "crpd",
     "treaty_name": "Convention on the Rights of Persons with Disabilities",
     "open_year": 2006},
    {"mtdsg": "IV-15-a", "chapter": 4, "treaty_id": "crpd_op",
     "treaty_name": "Optional Protocol to the CRPD",
     "open_year": 2006},
    # CED
    {"mtdsg": "IV-16", "chapter": 4, "treaty_id": "ced",
     "treaty_name": "International Convention for the Protection of All Persons from Enforced Disappearance",
     "open_year": 2006},
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
    text = re.sub(r"\[[^\]]*\]", "", text)  # remove footnote markers [1], [a] etc.
    text = text.replace("\xa0", " ")
    text = re.sub(r"\s+", " ", text).strip()
    return text


def _clean_country_name(text: str) -> str:
    """Clean country name, stripping trailing footnote reference numbers."""
    text = _clean_text(text)
    # Strip trailing standalone numbers/commas used as footnote refs:
    # "Netherlands (Kingdom of the) 12" -> "Netherlands (Kingdom of the)"
    # "United Kingdom of Great Britain and Northern Ireland 6, 15" -> "United Kingdom..."
    text = re.sub(r"(?:\s+\d+(?:\s*,\s*\d+)*)+\s*$", "", text)
    return text.strip()


def _parse_date(text: str) -> str | None:
    """Try to parse a date string in various formats used by treaties.un.org."""
    text = _clean_text(text)
    if not text or text in {"", "—", "-", "–"}:
        return None
    # Remove trailing action-type letters: "24 Jan 1983 a" -> "24 Jan 1983"
    text_no_suffix = re.sub(r"\s+[a-zA-Z]$", "", text.strip())
    patterns = [
        "%d %b %Y",     # 24 Jan 1983
        "%d %B %Y",     # 24 January 1983
        "%B %d, %Y",    # January 24, 1983
        "%b %d, %Y",    # Jan 24, 1983
        "%Y-%m-%d",     # 1983-01-24
        "%d/%m/%Y",     # 24/01/1983
    ]
    for fmt in patterns:
        try:
            return dt.datetime.strptime(text_no_suffix, fmt).date().isoformat()
        except ValueError:
            continue
    # Try extracting just the date portion from longer strings
    m = re.search(r"\d{1,2}\s+\w+\s+\d{4}", text)
    if m:
        for fmt in ["%d %b %Y", "%d %B %Y"]:
            try:
                return dt.datetime.strptime(m.group(0), fmt).date().isoformat()
            except ValueError:
                continue
    return None


def _extract_action_type(text: str) -> str:
    """Extract action type from trailing letter or keywords."""
    text = _clean_text(text).lower()
    if text.endswith(" a"):
        return "Accession"
    if text.endswith(" d"):
        return "Succession"
    if "accession" in text:
        return "Accession"
    if "succession" in text:
        return "Succession"
    if "ratification" in text or "ratifi" in text:
        return "Ratification"
    return "Ratification"  # default


# ---- HTML parser for UN treaty participant tables ----

class UNTreatyTableParser(HTMLParser):
    """Parse participant tables from treaties.un.org ViewDetails pages.

    The tables typically have columns: Participant | Signature | Ratification/Accession.
    Some pages have additional columns for declarations/reservations.
    """

    def __init__(self) -> None:
        super().__init__()
        self.rows: list[list[str]] = []
        self._in_table = False
        self._target_table = False
        self._in_row = False
        self._in_cell = False
        self._current_row: list[str] = []
        self._current_cell: list[str] = []
        self._table_count = 0

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        attrs_dict = dict(attrs)
        if tag == "table":
            self._in_table = True
            self._table_count += 1
        elif tag == "tr" and self._in_table:
            self._in_row = True
            self._current_row = []
        elif tag in ("td", "th") and self._in_row:
            self._in_cell = True
            self._current_cell = []
        # Track if we're in an anchor tag (for skip/link handling)
        elif tag == "a" and self._in_cell:
            pass  # Just capture text

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
    """Find the participant table rows; return (header, data_rows)."""
    # Look for a header row containing "Participant"
    for i, row in enumerate(all_rows):
        row_lower = [c.lower() for c in row]
        if any("participant" in c for c in row_lower):
            header = row
            data_rows = all_rows[i + 1:]
            return header, data_rows
    # Fallback: use first row as header if it looks like a header
    if all_rows:
        return all_rows[0], all_rows[1:]
    return [], []


def _parse_participants(html: str) -> list[dict]:
    """Parse participant data from a treaty ViewDetails page."""
    parser = UNTreatyTableParser()
    parser.feed(html)

    header, data_rows = _find_participant_rows(parser.rows)
    if not header:
        return []

    # Map column indices
    header_lower = [h.lower() for h in header]
    participant_idx = 0
    signature_idx = None
    ratification_idx = None

    for i, h in enumerate(header_lower):
        if "participant" in h:
            participant_idx = i
        elif "signature" in h:
            signature_idx = i
        elif any(kw in h for kw in ("ratification", "accession", "acceptance", "approval")):
            ratification_idx = i

    # If we couldn't find columns by name, use positional defaults
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
        # Skip summary rows like "Signatories: 74. Parties: 175"
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


# ---- main acquisition logic ----

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
            # Keep whichever has an action_date; if both do, keep the first
            if not existing.get("action_date") and rec.get("action_date"):
                deduped[key] = rec
    all_records = list(deduped.values())

    # Write CSV
    csv_path = os.path.join(out_dir, "un_hr_ratifications.csv")
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

    payload = {
        "source_name": "UN Treaty Collection — Chapter IV (Human Rights)",
        "landing_url": "https://treaties.un.org/Pages/Treaties.aspx?id=4&subid=A&clang=_en",
        "access_date": _today_str(),
        "downloaded_files": [csv_path] if all_records else [],
        "treaties_attempted": len(treaties),
        "total_records": len(all_records),
        "notes": notes,
        "errors": errors,
    }
    return payload


def validate(csv_path: str) -> list[str]:
    """Run validation checks on the output CSV."""
    issues = []
    if not os.path.exists(csv_path):
        issues.append(f"Output file not found: {csv_path}")
        return issues

    with open(csv_path, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows = list(reader)

    countries = {r["country_name"] for r in rows}
    treaties = {r["treaty_id"] for r in rows}

    if len(countries) < 150:
        issues.append(f"Only {len(countries)} countries found (expected >= 150)")
    if len(treaties) < 15:
        issues.append(f"Only {len(treaties)} treaty instruments found (expected >= 15)")

    # Check no action_date before treaty open year
    for r in rows:
        if r["action_date"] and r["treaty_open_year"]:
            try:
                action_year = int(r["action_date"][:4])
                open_year = int(r["treaty_open_year"])
                if action_year < open_year - 1:  # 1-year tolerance for signature timing
                    issues.append(
                        f"Action date {r['action_date']} before open year {open_year} "
                        f"for {r['country_name']} / {r['treaty_id']}"
                    )
            except (ValueError, IndexError):
                pass

    # Check for duplicates
    seen = set()
    for r in rows:
        key = (r["country_name"], r["treaty_id"])
        if key in seen:
            issues.append(f"Duplicate: {r['country_name']} / {r['treaty_id']}")
        seen.add(key)

    return issues


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Acquire UN human rights treaty ratification data."
    )
    parser.add_argument("--out", default="data/raw/un_hr_treaties", help="Output directory")
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

    csv_path = os.path.join(args.out, "un_hr_ratifications.csv")
    if not args.dry_run and not args.skip_validate and os.path.exists(csv_path):
        issues = validate(csv_path)
        if issues:
            print("\nValidation issues:")
            for issue in issues:
                print(f"  - {issue}")
            payload["validation_issues"] = issues
            # Re-write log with validation results
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
