from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import pathlib
import re
import sys
import time
import urllib.parse
import urllib.request

DEFAULT_UA = "IdealPointDataBot/0.1 (contact: local)"


def today_str() -> str:
    return dt.date.today().isoformat()


def ensure_dir(path: str | pathlib.Path) -> pathlib.Path:
    p = pathlib.Path(path)
    p.mkdir(parents=True, exist_ok=True)
    return p


def fetch_html(url: str, timeout: int = 60) -> str:
    req = urllib.request.Request(url, headers={"User-Agent": DEFAULT_UA})
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read().decode("utf-8", errors="ignore")


def extract_links(html: str, base_url: str) -> list[str]:
    # Simple href/src parser; avoids extra dependencies.
    links = set()
    for match in re.findall(r"(?:href|src)=[\"']?([^\"' >]+)", html, flags=re.IGNORECASE):
        abs_url = urllib.parse.urljoin(base_url, match)
        links.add(abs_url)
    return sorted(links)


def filter_links(links: list[str], exts: list[str]) -> list[str]:
    out = []
    lower_exts = [e.lower() for e in exts]
    for url in links:
        url_lower = url.lower()
        if any(url_lower.endswith(ext) for ext in lower_exts):
            out.append(url)
    return out


def download_file(url: str, dest_path: pathlib.Path, timeout: int = 120) -> None:
    req = urllib.request.Request(url, headers={"User-Agent": DEFAULT_UA})
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        with open(dest_path, "wb") as f:
            while True:
                chunk = resp.read(1024 * 1024)
                if not chunk:
                    break
                f.write(chunk)


def write_access_log(log_path: pathlib.Path, payload: dict) -> None:
    with open(log_path, "w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2, sort_keys=True)


def download_from_landing(
    source_name: str,
    landing_url: str,
    out_dir: pathlib.Path,
    exts: list[str],
    include_links: list[str] | None = None,
    dry_run: bool = False,
    timeout: int = 60,
) -> dict:
    out_dir = ensure_dir(out_dir)
    access_date = today_str()
    payload = {
        "source_name": source_name,
        "landing_url": landing_url,
        "access_date": access_date,
        "downloaded_files": [],
        "notes": [],
        "errors": [],
    }

    try:
        html = fetch_html(landing_url, timeout=timeout)
    except Exception as exc:  # noqa: BLE001
        payload["errors"].append(f"Failed to fetch landing page: {exc}")
        return payload

    links = extract_links(html, landing_url)
    if include_links:
        links.extend(include_links)
        links = sorted(set(links))

    candidates = filter_links(links, exts)

    if dry_run:
        payload["notes"].append("Dry run: no downloads executed.")
        payload["candidate_links"] = candidates
        return payload

    if not candidates:
        payload["errors"].append("No downloadable files found on landing page.")
        payload["candidate_links"] = candidates
        return payload

    for url in candidates:
        filename = pathlib.Path(urllib.parse.urlparse(url).path).name
        if not filename:
            continue
        dest = out_dir / filename
        try:
            download_file(url, dest)
            payload["downloaded_files"].append(str(dest))
            time.sleep(0.5)
        except Exception as exc:  # noqa: BLE001
            payload["errors"].append(f"Failed to download {url}: {exc}")

    return payload


def print_summary(payload: dict) -> None:
    if payload.get("errors"):
        print("Errors:")
        for err in payload["errors"]:
            print(f"- {err}")
    if payload.get("downloaded_files"):
        print("Downloaded files:")
        for f in payload["downloaded_files"]:
            print(f"- {f}")
    if payload.get("candidate_links"):
        print("Candidate links:")
        for url in payload["candidate_links"]:
            print(f"- {url}")
