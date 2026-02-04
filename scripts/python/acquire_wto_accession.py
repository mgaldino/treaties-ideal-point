#!/usr/bin/env python3
"""Acquire WTO accession dates from Wikipedia (open access).

Outputs:
  data/raw/wto_accession/wto_members.csv
  data/raw/wto_accession/access_log.json
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import html
import json
import os
import re
import sys
import urllib.request
from html.parser import HTMLParser

DEFAULT_URL = "https://en.wikipedia.org/wiki/Member_states_of_the_World_Trade_Organization"


def _utc_now_iso() -> str:
    return dt.datetime.utcnow().replace(microsecond=0).isoformat() + "Z"


class WikiTableParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.tables: list[list[list[str]]] = []
        self._in_table = False
        self._table_is_wikitable = False
        self._current_table: list[list[str]] = []
        self._current_row: list[str] = []
        self._current_cell: list[str] = []
        self._in_cell = False

    def handle_starttag(self, tag: str, attrs):
        if tag == "table":
            attrs_dict = dict(attrs)
            class_attr = attrs_dict.get("class", "")
            self._table_is_wikitable = "wikitable" in class_attr
            self._in_table = self._table_is_wikitable
            if self._in_table:
                self._current_table = []
        elif self._in_table and tag == "tr":
            self._current_row = []
        elif self._in_table and tag in ("td", "th"):
            self._in_cell = True
            self._current_cell = []

    def handle_data(self, data: str):
        if self._in_cell:
            self._current_cell.append(data)

    def handle_endtag(self, tag: str):
        if self._in_table and tag in ("td", "th"):
            cell_text = "".join(self._current_cell)
            cell_text = html.unescape(cell_text).strip()
            self._current_row.append(cell_text)
            self._current_cell = []
            self._in_cell = False
        elif self._in_table and tag == "tr":
            if any(cell.strip() for cell in self._current_row):
                self._current_table.append(self._current_row)
            self._current_row = []
        elif tag == "table" and self._in_table:
            self.tables.append(self._current_table)
            self._current_table = []
            self._in_table = False
            self._table_is_wikitable = False


def _clean_text(text: str) -> str:
    text = re.sub(r"\[[^\]]+\]", "", text)
    text = text.replace("\xa0", " ")
    text = re.sub(r"\s+", " ", text).strip()
    return text


def _extract_date(text: str) -> str | None:
    text = _clean_text(text)
    if not text or text in {"—", "-", "–"}:
        return None
    patterns = [
        r"\b\d{1,2}\s+[A-Za-z]+\s+\d{4}\b",
        r"\b[A-Za-z]+\s+\d{1,2},\s+\d{4}\b",
        r"\b\d{4}-\d{2}-\d{2}\b",
    ]
    for pat in patterns:
        m = re.search(pat, text)
        if m:
            date_str = m.group(0)
            for fmt in ("%d %B %Y", "%d %b %Y", "%B %d, %Y", "%Y-%m-%d"):
                try:
                    return dt.datetime.strptime(date_str, fmt).date().isoformat()
                except ValueError:
                    continue
    return None


def _find_members_table(tables: list[list[list[str]]]) -> list[list[str]]:
    for table in tables:
        if not table:
            continue
        header = [cell.lower().strip() for cell in table[0]]
        if any("member" in h for h in header) and any("accession" in h for h in header):
            return table
    raise RuntimeError("Could not find WTO members table in page.")


def fetch_html(url: str, timeout: int = 30) -> str:
    req = urllib.request.Request(
        url,
        headers={"User-Agent": "treaties-ideal-point/0.1 (contact: local)"},
    )
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read().decode("utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", default=DEFAULT_URL)
    parser.add_argument("--outdir", default="data/raw/wto_accession")
    args = parser.parse_args()

    os.makedirs(args.outdir, exist_ok=True)
    html_text = fetch_html(args.url)

    parser_obj = WikiTableParser()
    parser_obj.feed(html_text)
    members_table = _find_members_table(parser_obj.tables)

    header = members_table[0]
    col_map = {name.lower().strip(): idx for idx, name in enumerate(header)}
    member_idx = col_map.get("member", 0)
    date_idx = None
    for key in col_map:
        if "date of accession" in key:
            date_idx = col_map[key]
            break
    if date_idx is None:
        # fallback to second column
        date_idx = 1

    rows = []
    for row in members_table[1:]:
        if len(row) <= max(member_idx, date_idx):
            continue
        member = _clean_text(row[member_idx])
        if not member:
            continue
        date_raw = _clean_text(row[date_idx])
        date_iso = _extract_date(date_raw)
        is_original = "1995-01-01" == date_iso
        rows.append(
            {
                "country_name": member,
                "accession_date": date_iso or "",
                "accession_date_raw": date_raw,
                "is_original_member": "TRUE" if is_original else "FALSE",
                "country_code_iso3": "",
                "notes": "",
            }
        )

    if not rows:
        raise RuntimeError("No rows parsed from WTO members table.")

    out_csv = os.path.join(args.outdir, "wto_members.csv")
    with open(out_csv, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=[
                "country_name",
                "accession_date",
                "accession_date_raw",
                "is_original_member",
                "country_code_iso3",
                "notes",
            ],
        )
        writer.writeheader()
        writer.writerows(rows)

    access_log = {
        "source_url": args.url,
        "accessed_at": _utc_now_iso(),
        "rows": len(rows),
    }
    with open(os.path.join(args.outdir, "access_log.json"), "w", encoding="utf-8") as f:
        json.dump(access_log, f, indent=2)

    if len(rows) < 150 or len(rows) > 210:
        raise RuntimeError(f"Unexpected number of rows parsed: {len(rows)}")

    missing_dates = sum(1 for r in rows if not r["accession_date"])
    if missing_dates > 10:
        raise RuntimeError(f"Too many missing accession dates: {missing_dates}")

    print(f"Parsed {len(rows)} WTO members; missing dates: {missing_dates}")
    print(f"Wrote: {out_csv}")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        sys.exit(1)
