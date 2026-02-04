from __future__ import annotations

import argparse
import datetime as dt
import json
import pathlib
import re
import shutil
import time

from html.parser import HTMLParser

from _common import download_from_landing, fetch_html, print_summary, write_access_log

SOURCE_NAME = "UNCTAD IIA Navigator"
LANDING_URLS = [
    "https://investmentpolicy.unctad.org/international-investment-agreements/",
    "https://investmentpolicy.unctad.org/international-investment-agreements/by-country-grouping",
]

DATA_EXTS = [".zip", ".csv", ".xlsx", ".xls", ".json"]

CATALOG_URL = "https://investmentpolicy.unctad.org/international-investment-agreements/advanced-search"
COUNTRY_URL_TEMPLATE = (
    "https://investmentpolicy.unctad.org/international-investment-agreements/countries/{id}/{slug}"
)


def today_str() -> str:
    return dt.date.today().isoformat()


def extract_country_catalog(html: str) -> list[dict]:
    marker = "window.countryCatalog"
    idx = html.find(marker)
    if idx == -1:
        raise ValueError("country catalog marker not found")
    start = html.find("[", idx)
    if start == -1:
        raise ValueError("country catalog start not found")
    depth = 0
    in_str = False
    escape = False
    end = None
    for i in range(start, len(html)):
        ch = html[i]
        if in_str:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == "\"":
                in_str = False
        else:
            if ch == "\"":
                in_str = True
            elif ch == "[":
                depth += 1
            elif ch == "]":
                depth -= 1
                if depth == 0:
                    end = i + 1
                    break
    if end is None:
        raise ValueError("country catalog end not found")
    payload = html[start:end]
    return json.loads(payload)


def clean_text(value: str) -> str:
    return re.sub(r"\s+", " ", value).strip()


def normalize_header(value: str) -> str:
    value = clean_text(value).lower()
    value = re.sub(r"[^a-z0-9]+", "_", value)
    value = value.strip("_")
    return value


class TableParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.tables: list[dict] = []
        self._in_table = False
        self._in_thead = False
        self._in_tbody = False
        self._in_tr = False
        self._in_cell = False
        self._in_heading = False
        self._heading_text: list[str] = []
        self._last_heading: str | None = None
        self._current_table: dict | None = None
        self._current_row: list[str] = []
        self._current_row_links: list[list[str]] = []
        self._current_cell_text: list[str] = []
        self._current_cell_links: list[str] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        if tag in {"h2", "h3", "h4"}:
            self._in_heading = True
            self._heading_text = []
            return
        if tag == "table":
            self._in_table = True
            self._current_table = {
                "name": self._last_heading,
                "header_rows": [],
                "body_rows": [],
            }
            return
        if not self._in_table:
            return
        if tag == "thead":
            self._in_thead = True
        elif tag == "tbody":
            self._in_tbody = True
        elif tag == "tr":
            self._in_tr = True
            self._current_row = []
            self._current_row_links = []
        elif tag in {"th", "td"}:
            self._in_cell = True
            self._current_cell_text = []
            self._current_cell_links = []
        elif tag == "a" and self._in_cell:
            attrs_dict = dict(attrs)
            href = attrs_dict.get("href")
            if href:
                self._current_cell_links.append(href)

    def handle_data(self, data: str) -> None:
        if self._in_heading:
            self._heading_text.append(data)
        if self._in_cell:
            self._current_cell_text.append(data)

    def handle_endtag(self, tag: str) -> None:
        if tag in {"h2", "h3", "h4"}:
            heading = clean_text("".join(self._heading_text))
            if heading:
                self._last_heading = heading
            self._in_heading = False
            self._heading_text = []
            return
        if tag == "table":
            if self._current_table is not None:
                self.tables.append(self._current_table)
            self._current_table = None
            self._in_table = False
            return
        if not self._in_table:
            return
        if tag == "thead":
            self._in_thead = False
        elif tag == "tbody":
            self._in_tbody = False
        elif tag == "tr":
            if self._current_table is None:
                self._in_tr = False
                return
            if self._current_row:
                row = {
                    "cells": self._current_row,
                    "links": self._current_row_links,
                }
                if self._in_thead:
                    self._current_table["header_rows"].append(row)
                else:
                    self._current_table["body_rows"].append(row)
            self._current_row = []
            self._current_row_links = []
            self._in_tr = False
        elif tag in {"th", "td"}:
            text = clean_text("".join(self._current_cell_text))
            self._current_row.append(text)
            self._current_row_links.append(self._current_cell_links)
            self._current_cell_text = []
            self._current_cell_links = []
            self._in_cell = False


def parse_country_tables(html: str) -> list[dict]:
    parser = TableParser()
    parser.feed(html)
    records: list[dict] = []
    for table in parser.tables:
        header_rows = table.get("header_rows", [])
        body_rows = table.get("body_rows", [])
        if not body_rows:
            continue
        header_cells = []
        if header_rows:
            header_cells = header_rows[0]["cells"]
        header_cells = [clean_text(cell) for cell in header_cells]
        normalized_header = [normalize_header(cell) for cell in header_cells]
        drop_no = False
        if normalized_header and normalized_header[0] in {"no", "no_"}:
            drop_no = True
            normalized_header = normalized_header[1:]

        for row in body_rows:
            cells = row["cells"]
            links = row["links"]
            if not any(cells):
                continue
            if drop_no and len(cells) == len(normalized_header) + 1:
                cells = cells[1:]
                links = links[1:]
            if len(cells) < len(normalized_header):
                cells = cells + [""] * (len(normalized_header) - len(cells))
                links = links + [[]] * (len(normalized_header) - len(links))
            if len(cells) > len(normalized_header) and normalized_header:
                cells = cells[: len(normalized_header)]
                links = links[: len(normalized_header)]

            record = {normalized_header[i]: cells[i] for i in range(len(normalized_header))}

            flat_links = [href for sub in links for href in sub]
            record["treaty_url"] = flat_links[0] if flat_links else ""

            if "full_title" in record:
                idx = normalized_header.index("full_title")
                record["full_title_url"] = links[idx][0] if idx < len(links) and links[idx] else ""
            elif "short_title" in record:
                idx = normalized_header.index("short_title")
                record["full_title_url"] = links[idx][0] if idx < len(links) and links[idx] else ""
            else:
                record["full_title_url"] = record["treaty_url"]

            if "text" in normalized_header:
                idx = normalized_header.index("text")
                record["text_url"] = links[idx][0] if idx < len(links) and links[idx] else ""
            else:
                record["text_url"] = ""

            record["table_name"] = table.get("name") or ""
            records.append(record)
    return records


def scrape_country_pages(
    out_dir: pathlib.Path,
    delay: float,
    limit: int | None,
    resume: bool,
    parse_only: bool,
) -> dict:
    out_dir.mkdir(parents=True, exist_ok=True)
    html_dir = out_dir / "html"
    html_dir.mkdir(parents=True, exist_ok=True)

    catalog_html = fetch_html(CATALOG_URL, timeout=60)
    catalog = extract_country_catalog(catalog_html)

    catalog_path = out_dir / "country_catalog.json"
    with open(catalog_path, "w", encoding="utf-8") as f:
        json.dump(catalog, f, indent=2, sort_keys=True)

    if limit is not None:
        catalog = catalog[:limit]

    catalog_by_id = {str(entry.get("Id")): entry for entry in catalog if entry.get("Id") is not None}

    records: list[dict] = []
    errors: list[str] = []

    if parse_only:
        html_files = sorted(html_dir.glob("*.html"))
        for html_path in html_files:
            name = html_path.stem
            parts = name.split("_", 1)
            if not parts:
                continue
            country_id = parts[0]
            entry = catalog_by_id.get(country_id)
            if entry is None:
                continue
            slug = entry.get("UrlName")
            url = COUNTRY_URL_TEMPLATE.format(id=country_id, slug=slug)
            html = html_path.read_text(encoding="utf-8", errors="ignore")
            rows = parse_country_tables(html)
            for row in rows:
                row["country_id"] = entry.get("Id")
                row["country_name"] = entry.get("Name") or ""
                row["country_code"] = entry.get("Code") or ""
                row["country_slug"] = slug or ""
                row["source_url"] = url
            records.extend(rows)
    else:
        for entry in catalog:
            country_id = entry.get("Id")
            slug = entry.get("UrlName")
            if country_id is None or not slug:
                continue
            url = COUNTRY_URL_TEMPLATE.format(id=country_id, slug=slug)
            html_path = html_dir / f"{country_id}_{slug}.html"
            if resume and html_path.exists():
                html = html_path.read_text(encoding="utf-8", errors="ignore")
            else:
                try:
                    html = fetch_html(url, timeout=60)
                    with open(html_path, "w", encoding="utf-8") as f:
                        f.write(html)
                except Exception as exc:  # noqa: BLE001
                    errors.append(f"Failed to fetch {url}: {exc}")
                    time.sleep(delay)
                    continue

            rows = parse_country_tables(html)
            for row in rows:
                row["country_id"] = country_id
                row["country_name"] = entry.get("Name") or ""
                row["country_code"] = entry.get("Code") or ""
                row["country_slug"] = slug
                row["source_url"] = url
            records.extend(rows)
            time.sleep(delay)

    output_csv = out_dir / "unctad_iia_country_treaties.csv"
    if records:
        fieldnames = sorted({key for row in records for key in row.keys()})
        with open(output_csv, "w", encoding="utf-8") as f:
            f.write(",".join(fieldnames) + "\n")
            for row in records:
                values = []
                for field in fieldnames:
                    value = row.get(field, "")
                    if value is None:
                        value = ""
                    value = str(value)
                    value = value.replace("\"", '""')
                    if "," in value or "\n" in value or "\r" in value:
                        value = f'"{value}"'
                    values.append(value)
                f.write(",".join(values) + "\n")

    html_count = len(list(html_dir.glob("*.html")))
    payload = {
        "source_name": SOURCE_NAME,
        "landing_url": CATALOG_URL,
        "access_date": today_str(),
        "downloaded_files": [str(output_csv), str(catalog_path)],
        "notes": [
            f"Scraped {len(catalog)} country pages.",
            f"Saved raw HTML to {html_dir}.",
            f"HTML files available: {html_count}.",
        ],
        "errors": errors,
    }
    if parse_only:
        payload["notes"].append("Parse-only mode; no fetching performed.")
    if resume:
        payload["notes"].append("Resume mode enabled (skipped existing HTML files).")
    return payload


def main() -> int:
    parser = argparse.ArgumentParser(description="Acquire UNCTAD IIA Navigator data.")
    parser.add_argument("--out", default="data/raw/unctad_iia", help="Output directory")
    parser.add_argument(
        "--manual-file",
        default=None,
        help="Optional path to a manually exported file (CSV/XLSX) to copy into the raw folder",
    )
    parser.add_argument("--scrape", action="store_true", help="Scrape country pages directly")
    parser.add_argument("--delay", type=float, default=1.5, help="Delay between requests (seconds)")
    parser.add_argument("--limit", type=int, default=None, help="Limit number of countries (for testing)")
    parser.add_argument("--resume", action="store_true", help="Skip already downloaded HTML pages")
    parser.add_argument("--parse-only", action="store_true", help="Parse existing HTML without fetching")
    parser.add_argument("--dry-run", action="store_true", help="List candidate links only")
    args = parser.parse_args()

    out_dir = pathlib.Path(args.out)
    payload = None

    if args.scrape:
        try:
            payload = scrape_country_pages(
                out_dir,
                delay=args.delay,
                limit=args.limit,
                resume=args.resume,
                parse_only=args.parse_only,
            )
        except Exception as exc:  # noqa: BLE001
            payload = {
                "source_name": SOURCE_NAME,
                "landing_url": CATALOG_URL,
                "access_date": today_str(),
                "downloaded_files": [],
                "notes": [],
                "errors": [f"Scrape failed: {exc}"],
            }
    else:
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

    if args.manual_file:
        manual_path = pathlib.Path(args.manual_file)
        if manual_path.exists():
            dest = out_dir / manual_path.name
            out_dir.mkdir(parents=True, exist_ok=True)
            shutil.copy2(manual_path, dest)
            payload["downloaded_files"].append(str(dest))
        else:
            payload["errors"].append(f"Manual file not found: {manual_path}")

    if payload.get("errors"):
        payload["notes"].append(
            "If no direct download links are detected, export data manually from the IIA Navigator and rerun with --manual-file."
        )

    log_path = out_dir / "access_log.json"
    write_access_log(log_path, payload)
    print_summary(payload)

    return 0 if not payload.get("errors") else 1


if __name__ == "__main__":
    raise SystemExit(main())
