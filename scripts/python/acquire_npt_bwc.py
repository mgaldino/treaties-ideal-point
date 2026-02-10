#!/usr/bin/env python3
"""Acquire NPT and BWC ratification data from Wikipedia API.

NPT and BWC are not in the UN Treaty Collection (different depositaries),
so we scrape the structured tables from Wikipedia's API.

Outputs:
  data/raw/arms_control/npt_ratifications.csv
  data/raw/arms_control/bwc_ratifications.csv
"""
import csv
import json
import os
import re
import sys
import urllib.request
import urllib.parse

UA = "IdealPointDataBot/0.1 (academic research; Python urllib)"

TREATIES = {
    "npt": {
        "page": "List_of_parties_to_the_Treaty_on_the_Non-Proliferation_of_Nuclear_Weapons",
        "treaty_id": "npt",
        "treaty_name": "Treaty on the Non-Proliferation of Nuclear Weapons",
        "open_year": 1968,
    },
    "bwc": {
        "page": "List_of_parties_to_the_Biological_Weapons_Convention",
        "treaty_id": "bwc",
        "treaty_name": "Convention on the Prohibition of Biological Weapons",
        "open_year": 1972,
    },
}


def fetch_wikitext(page_title: str) -> str:
    """Fetch raw wikitext for a Wikipedia page via the API."""
    url = (
        "https://en.wikipedia.org/w/api.php?"
        + urllib.parse.urlencode({
            "action": "parse",
            "page": page_title,
            "prop": "wikitext",
            "format": "json",
        })
    )
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=30) as resp:
        data = json.loads(resp.read().decode("utf-8"))
    return data["parse"]["wikitext"]["*"]


MONTH_MAP = {
    "January": "01", "February": "02", "March": "03", "April": "04",
    "May": "05", "June": "06", "July": "07", "August": "08",
    "September": "09", "October": "10", "November": "11", "December": "12",
    "Jan": "01", "Feb": "02", "Mar": "03", "Apr": "04",
    "Jun": "06", "Jul": "07", "Aug": "08",
    "Sep": "09", "Oct": "10", "Nov": "11", "Dec": "12",
    "1": "01", "2": "02", "3": "03", "4": "04", "5": "05", "6": "06",
    "7": "07", "8": "08", "9": "09", "10": "10", "11": "11", "12": "12",
    "01": "01", "02": "02", "03": "03", "04": "04", "05": "05", "06": "06",
    "07": "07", "08": "08", "09": "09",
}


def parse_date(text: str) -> str:
    """Parse the FIRST date from wikitext (may have multiple dates with <br />)."""
    # Match {{dts|format=dmy|YYYY|Mon|DD}} (NPT format)
    m = re.search(r"\{\{dts\|format=dmy\|(\d{4})\|(\w+)\|(\d{1,2})\}\}", text)
    if m:
        year, month_str, day = m.group(1), m.group(2), m.group(3)
        mm = MONTH_MAP.get(month_str, month_str.zfill(2))
        return f"{year}-{mm}-{day.zfill(2)}"

    # Match {{dts|YYYY|Month|DD}} (BWC format)
    m = re.search(r"\{\{dts\|(\d{4})\|(\w+)\|(\d{1,2})\}\}", text)
    if m:
        year, month_str, day = m.group(1), m.group(2), m.group(3)
        mm = MONTH_MAP.get(month_str, month_str.zfill(2))
        return f"{year}-{mm}-{day.zfill(2)}"

    # Match {{start date|YYYY|MM|DD}}
    m = re.search(r"\{\{start date\|(\d{4})\|(\d{1,2})\|(\d{1,2})\}\}", text)
    if m:
        return f"{m.group(1)}-{m.group(2).zfill(2)}-{m.group(3).zfill(2)}"

    # Match plain dates like "1 July 1968"
    m = re.search(r"(\d{1,2})\s+(January|February|March|April|May|June|July|August|September|October|November|December)\s+(\d{4})", text)
    if m:
        return f"{m.group(3)}-{MONTH_MAP[m.group(2)]}-{m.group(1).zfill(2)}"

    # Match "July 1, 1968"
    m = re.search(r"(January|February|March|April|May|June|July|August|September|October|November|December)\s+(\d{1,2}),?\s+(\d{4})", text)
    if m:
        return f"{m.group(3)}-{MONTH_MAP[m.group(1)]}-{m.group(2).zfill(2)}"

    return ""


def clean_country_name(text: str) -> str:
    """Extract country name from wikitext cell."""
    # Handle {{flag|CountryName|year}} -> CountryName
    m = re.search(r"\{\{flag\|([^}|]+)", text)
    if m:
        return m.group(1).strip()
    # Handle {{flagcountry|...}}
    m = re.search(r"\{\{flagcountry\|([^}|]+)", text, re.IGNORECASE)
    if m:
        return m.group(1).strip()
    # Remove wikilinks: [[foo|bar]] -> bar, [[foo]] -> foo
    text = re.sub(r"\[\[([^\]|]+)\|([^\]]+)\]\]", r"\2", text)
    text = re.sub(r"\[\[([^\]]+)\]\]", r"\1", text)
    # Remove remaining templates
    text = re.sub(r"\{\{[^}]*\}\}", "", text)
    # Remove refs
    text = re.sub(r"<ref[^>]*>.*?</ref>", "", text, flags=re.DOTALL)
    text = re.sub(r"<ref[^/]*/>", "", text)
    # Remove HTML tags
    text = re.sub(r"<[^>]+>", "", text)
    # Clean up
    text = text.strip().strip("'").strip()
    return text


def parse_action_type(text: str) -> str:
    """Determine if action is ratification, accession, or succession."""
    # Remove wikitext markup
    text_clean = re.sub(r"\{\{[^}]*\}\}", "", text)
    text_lower = text_clean.lower().strip()
    if "succession" in text_lower:
        return "Succession"
    if "accession" in text_lower or "acceded" in text_lower:
        return "Accession"
    if "ratification" in text_lower or "ratified" in text_lower:
        return "Ratification"
    return "Ratification"


def parse_npt_wikitext(wikitext: str) -> list[dict]:
    """Parse NPT parties from wikitext table rows."""
    records = []

    # Find table rows: lines starting with |-
    # NPT Wikipedia table has columns: State | Signed | Deposited | Method
    # The table uses wikitable format

    # Split into lines and look for table rows
    lines = wikitext.split("\n")
    i = 0
    in_table = False

    while i < len(lines):
        line = lines[i].strip()

        # Detect table start
        if "{|" in line and "wikitable" in line.lower():
            in_table = True
            i += 1
            continue

        if in_table and line.startswith("|}"):
            in_table = False
            i += 1
            continue

        # Table row separator
        if in_table and line.startswith("|-"):
            # Collect cells for this row
            cells = []
            i += 1
            while i < len(lines):
                cline = lines[i].strip()
                if cline.startswith("|-") or cline.startswith("|}") or cline.startswith("{|"):
                    break
                if cline.startswith("|") or cline.startswith("!"):
                    # Split by || for multiple cells on one line
                    raw = cline[1:].strip()  # Remove leading | or !
                    parts = re.split(r"\|\|", raw)
                    for p in parts:
                        cells.append(p.strip())
                i += 1

            # Process cells if we have enough (at least country + some dates)
            if len(cells) >= 2:
                country = clean_country_name(cells[0])
                if not country or country.lower() in ("state", "country", "participant", ""):
                    continue
                # Skip header-like rows
                if "signed" in country.lower() or "deposited" in country.lower():
                    continue

                sig_date = parse_date(cells[1]) if len(cells) > 1 else ""
                action_date = parse_date(cells[2]) if len(cells) > 2 else ""
                action_type = parse_action_type(cells[3] if len(cells) > 3 else cells[2] if len(cells) > 2 else "")

                if action_date or sig_date:
                    records.append({
                        "country_name": country,
                        "signature_date": sig_date,
                        "action_date": action_date,
                        "action_type": action_type,
                    })
            continue

        i += 1

    return records


def parse_bwc_wikitext(wikitext: str) -> list[dict]:
    """Parse BWC parties from wikitext table rows."""
    # Same structure as NPT
    return parse_npt_wikitext(wikitext)


def write_csv(records: list[dict], out_path: str, treaty_info: dict) -> None:
    """Write records to CSV in the same format as arms_control_ratifications.csv."""
    fieldnames = [
        "country_name", "treaty_id", "treaty_name", "action_type",
        "action_date", "signature_date", "treaty_open_year", "mtdsg_no", "source_url",
    ]
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        for rec in records:
            row = {
                "country_name": rec["country_name"],
                "treaty_id": treaty_info["treaty_id"],
                "treaty_name": treaty_info["treaty_name"],
                "action_type": rec["action_type"],
                "action_date": rec["action_date"],
                "signature_date": rec["signature_date"],
                "treaty_open_year": treaty_info["open_year"],
                "mtdsg_no": "",
                "source_url": f"https://en.wikipedia.org/wiki/{treaty_info['page']}",
            }
            writer.writerow(row)


def main() -> int:
    out_dir = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                           "data", "raw", "arms_control")
    os.makedirs(out_dir, exist_ok=True)

    for key, info in TREATIES.items():
        print(f"\n=== Fetching {key} from Wikipedia ===")
        try:
            wikitext = fetch_wikitext(info["page"])
            print(f"  Got {len(wikitext)} chars of wikitext")

            # Save raw wikitext for debugging
            debug_path = os.path.join(out_dir, f"{key}_wikitext.txt")
            with open(debug_path, "w", encoding="utf-8") as f:
                f.write(wikitext)
            print(f"  Saved wikitext to {debug_path}")

            if key == "npt":
                records = parse_npt_wikitext(wikitext)
            else:
                records = parse_bwc_wikitext(wikitext)

            # Filter: only keep records with action_date (ratified/acceded)
            ratified = [r for r in records if r["action_date"]]
            print(f"  Parsed {len(records)} total entries, {len(ratified)} with ratification/accession dates")

            csv_path = os.path.join(out_dir, f"{key}_ratifications.csv")
            write_csv(ratified, csv_path, info)
            print(f"  Wrote {len(ratified)} records to {csv_path}")

        except Exception as exc:
            print(f"  ERROR: {exc}", file=sys.stderr)
            import traceback
            traceback.print_exc()
            return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
