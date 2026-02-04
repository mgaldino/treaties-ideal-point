#!/usr/bin/env python3
"""Acquire WIPO-administered treaty ratification data from WIPO Lex.

Scrapes contracting-parties tables from the WIPO Lex portal for ~26
WIPO-administered IP treaties. Each treaty has a contracting parties page at:
  https://www.wipo.int/wipolex/en/treaties/ShowResults?search_what=C&treaty_id={id}

Outputs:
  data/raw/wipo_treaties/wipo_ratifications.csv
  data/raw/wipo_treaties/access_log.json
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

WIPO_BASE = "https://www.wipo.int/wipolex/en/treaties/ShowResults"
WIPO_CP_URL = WIPO_BASE + "?search_what=C&treaty_id={treaty_id}"

# WIPO-administered treaties with their WIPO Lex treaty_id values
# IDs discovered by probing the WIPO Lex database
TREATIES = [
    # WIPO Convention
    {"wipo_id": 1, "treaty_id": "wipo_convention",
     "treaty_name": "WIPO Convention", "open_year": 1967},
    # IP Protection Treaties
    {"wipo_id": 2, "treaty_id": "paris",
     "treaty_name": "Paris Convention for the Protection of Industrial Property", "open_year": 1883},
    {"wipo_id": 15, "treaty_id": "berne",
     "treaty_name": "Berne Convention for the Protection of Literary and Artistic Works", "open_year": 1886},
    {"wipo_id": 3, "treaty_id": "madrid_indications",
     "treaty_name": "Madrid Agreement (Indications of Source)", "open_year": 1891},
    {"wipo_id": 21, "treaty_id": "madrid_marks",
     "treaty_name": "Madrid Agreement (Marks)", "open_year": 1891},
    {"wipo_id": 8, "treaty_id": "madrid_protocol",
     "treaty_name": "Protocol Relating to the Madrid Agreement (Madrid Protocol)", "open_year": 1989},
    {"wipo_id": 9, "treaty_id": "hague",
     "treaty_name": "Hague Agreement Concerning the International Deposit of Industrial Designs", "open_year": 1925},
    {"wipo_id": 10, "treaty_id": "lisbon",
     "treaty_name": "Lisbon Agreement for the Protection of Appellations of Origin", "open_year": 1958},
    {"wipo_id": 6, "treaty_id": "pct",
     "treaty_name": "Patent Cooperation Treaty", "open_year": 1970},
    {"wipo_id": 7, "treaty_id": "budapest",
     "treaty_name": "Budapest Treaty on the International Recognition of the Deposit of Microorganisms", "open_year": 1977},
    {"wipo_id": 22, "treaty_id": "nairobi",
     "treaty_name": "Nairobi Treaty on the Protection of the Olympic Symbol", "open_year": 1981},
    {"wipo_id": 5, "treaty_id": "tlt",
     "treaty_name": "Trademark Law Treaty", "open_year": 1994},
    {"wipo_id": 4, "treaty_id": "plt",
     "treaty_name": "Patent Law Treaty", "open_year": 2000},
    {"wipo_id": 30, "treaty_id": "singapore",
     "treaty_name": "Singapore Treaty on the Law of Trademarks", "open_year": 2006},
    # Copyright and related rights
    {"wipo_id": 16, "treaty_id": "wct",
     "treaty_name": "WIPO Copyright Treaty", "open_year": 1996},
    {"wipo_id": 20, "treaty_id": "wppt",
     "treaty_name": "WIPO Performances and Phonograms Treaty", "open_year": 1996},
    {"wipo_id": 17, "treaty_id": "rome",
     "treaty_name": "Rome Convention for the Protection of Performers, Producers of Phonograms and Broadcasting Organizations", "open_year": 1961},
    {"wipo_id": 18, "treaty_id": "phonograms",
     "treaty_name": "Convention for the Protection of Producers of Phonograms (Phonograms Convention)", "open_year": 1971},
    {"wipo_id": 19, "treaty_id": "brussels",
     "treaty_name": "Brussels Convention Relating to the Distribution of Programme-Carrying Signals", "open_year": 1974},
    {"wipo_id": 841, "treaty_id": "beijing",
     "treaty_name": "Beijing Treaty on Audiovisual Performances", "open_year": 2012},
    {"wipo_id": 843, "treaty_id": "marrakesh",
     "treaty_name": "Marrakesh Treaty to Facilitate Access to Published Works for the Visually Impaired", "open_year": 2013},
    # Classification Treaties
    {"wipo_id": 11, "treaty_id": "strasbourg",
     "treaty_name": "Strasbourg Agreement Concerning the International Patent Classification", "open_year": 1971},
    {"wipo_id": 12, "treaty_id": "nice",
     "treaty_name": "Nice Agreement Concerning the International Classification of Goods and Services for the Purposes of the Registration of Marks", "open_year": 1957},
    {"wipo_id": 13, "treaty_id": "vienna_classification",
     "treaty_name": "Vienna Agreement Establishing an International Classification of the Figurative Elements of Marks", "open_year": 1973},
    {"wipo_id": 14, "treaty_id": "locarno",
     "treaty_name": "Locarno Agreement Establishing an International Classification for Industrial Designs", "open_year": 1968},
    # Washington Treaty
    {"wipo_id": 29, "treaty_id": "washington",
     "treaty_name": "Washington Treaty on Intellectual Property in Respect of Integrated Circuits", "open_year": 1989},
]


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


def _parse_date(text: str) -> str | None:
    """Parse dates in WIPO format: 'September 9, 1886' or 'March 2, 2018'."""
    text = _clean_text(text)
    if not text or text in {"", "—", "-", "–"}:
        return None
    # Remove action type prefixes like "Ratification: " or "Accession: "
    text_clean = re.sub(r"^(Ratification|Accession|Succession|Declaration of [Cc]ontinued [Aa]pplication)\s*:\s*", "", text)
    patterns = [
        "%B %d, %Y",    # September 9, 1886
        "%b %d, %Y",    # Sep 9, 1886
        "%d %B %Y",     # 9 September 1886
        "%d %b %Y",     # 9 Sep 1886
        "%Y-%m-%d",     # 1886-09-09
    ]
    for fmt in patterns:
        try:
            return dt.datetime.strptime(text_clean.strip(), fmt).date().isoformat()
        except ValueError:
            continue
    # Try extracting date portion
    m = re.search(r"([A-Z][a-z]+\s+\d{1,2},\s+\d{4})", text)
    if m:
        for fmt in ["%B %d, %Y", "%b %d, %Y"]:
            try:
                return dt.datetime.strptime(m.group(1), fmt).date().isoformat()
            except ValueError:
                continue
    m = re.search(r"(\d{1,2}\s+[A-Z][a-z]+\s+\d{4})", text)
    if m:
        for fmt in ["%d %B %Y", "%d %b %Y"]:
            try:
                return dt.datetime.strptime(m.group(1), fmt).date().isoformat()
            except ValueError:
                continue
    return None


def _extract_action_type(text: str) -> str:
    """Extract action type from WIPO instrument column."""
    text = _clean_text(text).lower()
    if "accession" in text:
        return "Accession"
    if "succession" in text or "continued application" in text:
        return "Succession"
    if "ratification" in text:
        return "Ratification"
    return "Ratification"


class WIPOTableParser(HTMLParser):
    """Parse contracting parties tables from WIPO Lex."""

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


def _find_member_rows(all_rows: list[list[str]]) -> tuple[list[str], list[list[str]]]:
    """Find the member/contracting parties table rows."""
    for i, row in enumerate(all_rows):
        row_lower = [c.lower() for c in row]
        if any("member" in c for c in row_lower) and any(
            kw in c for c in row_lower for kw in ("signature", "instrument", "in force")
        ):
            return row, all_rows[i + 1:]
    # Fallback
    for i, row in enumerate(all_rows):
        row_lower = [c.lower() for c in row]
        if any("member" in c or "country" in c or "contracting" in c for c in row_lower):
            return row, all_rows[i + 1:]
    if all_rows:
        return all_rows[0], all_rows[1:]
    return [], []


def _parse_wipo_participants(html: str) -> list[dict]:
    """Parse contracting parties from a WIPO Lex ShowResults page."""
    parser = WIPOTableParser()
    parser.feed(html)

    header, data_rows = _find_member_rows(parser.rows)
    if not header:
        return []

    header_lower = [h.lower() for h in header]
    member_idx = 0
    signature_idx = None
    instrument_idx = None
    in_force_idx = None

    for i, h in enumerate(header_lower):
        if "member" in h or "country" in h:
            member_idx = i
        elif "signature" in h:
            signature_idx = i
        elif "instrument" in h:
            instrument_idx = i
        elif "in force" in h or "entry" in h:
            in_force_idx = i

    # Positional fallbacks
    if signature_idx is None and len(header) >= 2:
        signature_idx = 1
    if instrument_idx is None and len(header) >= 3:
        instrument_idx = 2
    if in_force_idx is None and len(header) >= 4:
        in_force_idx = 3

    records = []
    for row in data_rows:
        if len(row) <= member_idx:
            continue
        country = _clean_text(row[member_idx])
        if not country or country.lower() in {"member", "country", ""}:
            continue
        # Skip non-country entries like "Details" or row numbers
        if country.lower() in {"details", "remarks"} or re.match(r"^\d+$", country):
            continue
        # Skip "Total Members" summary rows
        if "total" in country.lower() and "member" in country.lower():
            continue

        sig_text = row[signature_idx] if signature_idx is not None and signature_idx < len(row) else ""
        inst_text = row[instrument_idx] if instrument_idx is not None and instrument_idx < len(row) else ""
        force_text = row[in_force_idx] if in_force_idx is not None and in_force_idx < len(row) else ""

        # Signature date (from signature column)
        signature_date = _parse_date(sig_text) if sig_text else None

        # Action date from instrument column (e.g., "Accession: March 2, 2018")
        action_date = _parse_date(inst_text)
        action_type = _extract_action_type(inst_text) if inst_text else ""

        # Entry into force date
        entry_into_force = _parse_date(force_text)

        if signature_date or action_date or entry_into_force:
            records.append({
                "country_name": country,
                "signature_date": signature_date or "",
                "action_date": action_date or "",
                "action_type": action_type,
                "entry_into_force_date": entry_into_force or "",
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
        url = WIPO_CP_URL.format(treaty_id=info["wipo_id"])
        treaty_id = info["treaty_id"]
        treaty_name = info["treaty_name"]
        open_year = info["open_year"]

        if dry_run:
            notes.append(f"[dry-run] Would fetch {treaty_id} (wipo_id={info['wipo_id']}) from {url}")
            continue

        print(f"Fetching {treaty_id} (wipo_id={info['wipo_id']})...")
        html_path = os.path.join(html_dir, f"{treaty_id}.html")

        try:
            html = _fetch_html(url, timeout=90)
            with open(html_path, "w", encoding="utf-8") as f:
                f.write(html)
        except Exception as exc:
            errors.append(f"Failed to fetch {treaty_id}: {exc}")
            time.sleep(delay)
            continue

        records = _parse_wipo_participants(html)
        for rec in records:
            rec["treaty_id"] = treaty_id
            rec["treaty_name"] = treaty_name
            rec["treaty_open_year"] = open_year
            rec["wipo_treaty_id"] = info["wipo_id"]
            rec["source_url"] = url

        all_records.extend(records)
        notes.append(f"{treaty_id}: {len(records)} contracting parties parsed")
        print(f"  -> {len(records)} contracting parties")
        time.sleep(delay)

    csv_path = os.path.join(out_dir, "wipo_ratifications.csv")
    fieldnames = [
        "country_name", "treaty_id", "treaty_name", "action_type",
        "action_date", "signature_date", "entry_into_force_date",
        "treaty_open_year", "wipo_treaty_id", "source_url",
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
        "source_name": "WIPO Lex — WIPO-Administered Treaties",
        "landing_url": "https://www.wipo.int/wipolex/en/treaties",
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

    # Paris Convention should have ~177+ parties
    paris_count = sum(1 for r in rows if r["treaty_id"] == "paris" and (r["action_date"] or r["entry_into_force_date"]))
    if paris_count < 150:
        issues.append(f"Paris Convention has only {paris_count} parties (expected ~177+)")

    # PCT should have ~157+ parties
    pct_count = sum(1 for r in rows if r["treaty_id"] == "pct" and (r["action_date"] or r["entry_into_force_date"]))
    if pct_count < 130:
        issues.append(f"PCT has only {pct_count} parties (expected ~157+)")

    # Berne Convention should have ~181+ parties
    berne_count = sum(1 for r in rows if r["treaty_id"] == "berne" and (r["action_date"] or r["entry_into_force_date"]))
    if berne_count < 150:
        issues.append(f"Berne Convention has only {berne_count} parties (expected ~181+)")

    if len(countries) < 100:
        issues.append(f"Only {len(countries)} unique countries (expected >= 100)")

    if len(treaties) < 20:
        issues.append(f"Only {len(treaties)} treaties parsed (expected >= 20)")

    # Check no action_date before treaty open year (with tolerance)
    for r in rows:
        date_to_check = r.get("action_date") or r.get("entry_into_force_date") or ""
        open_year = r.get("treaty_open_year", "")
        if date_to_check and open_year:
            try:
                action_year = int(date_to_check[:4])
                oy = int(open_year)
                if action_year < oy - 5:  # wider tolerance for very old treaties
                    issues.append(
                        f"Date {date_to_check} well before open year {open_year} "
                        f"for {r['country_name']} / {r['treaty_id']}"
                    )
            except (ValueError, IndexError):
                pass

    return issues


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Acquire WIPO treaty ratification data."
    )
    parser.add_argument("--out", default="data/raw/wipo_treaties", help="Output directory")
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

    csv_path = os.path.join(args.out, "wipo_ratifications.csv")
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
