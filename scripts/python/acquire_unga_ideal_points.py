#!/usr/bin/env python3
"""Download UNGA ideal points (Bailey, Strezhnev, Voeten) from Harvard Dataverse.

Outputs:
  data/raw/unga_ideal_points/<file>
  data/raw/unga_ideal_points/access_log.json
  data/raw/unga_ideal_points/manifest.json
"""
from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import os
import re
import sys
import urllib.request

PERSISTENT_ID = "doi:10.7910/DVN/LEJUQZ"
API_URL = "https://dataverse.harvard.edu/api/datasets/:persistentId/?persistentId=" + PERSISTENT_ID


def _utc_now_iso() -> str:
    return dt.datetime.utcnow().replace(microsecond=0).isoformat() + "Z"


def _fetch_json(url: str) -> dict:
    req = urllib.request.Request(
        url,
        headers={"User-Agent": "treaties-ideal-point/0.1 (contact: local)"},
    )
    with urllib.request.urlopen(req, timeout=60) as resp:
        return json.loads(resp.read().decode("utf-8"))


def _download_file(file_id: int, out_path: str) -> None:
    url = f"https://dataverse.harvard.edu/api/access/datafile/{file_id}?format=original"
    req = urllib.request.Request(
        url,
        headers={"User-Agent": "treaties-ideal-point/0.1 (contact: local)"},
    )
    with urllib.request.urlopen(req, timeout=120) as resp, open(out_path, "wb") as f:
        f.write(resp.read())


def _score_label(label: str) -> int:
    label_l = label.lower()
    score = 0
    if "year" in label_l:
        score += 3
    if any(x in label_l for x in ("ideal", "point", "estimate")):
        score += 3
    if label_l.endswith(".tab") or label_l.endswith(".csv"):
        score += 2
    if "session" in label_l:
        score -= 2
    return score


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--outdir", default="data/raw/unga_ideal_points")
    parser.add_argument("--file-label", default="")
    args = parser.parse_args()

    os.makedirs(args.outdir, exist_ok=True)

    meta = _fetch_json(API_URL)
    if meta.get("status") != "OK":
        raise RuntimeError("Dataverse API returned non-OK status")

    files = meta["data"]["latestVersion"]["files"]
    if not files:
        raise RuntimeError("No files found in Dataverse metadata")

    chosen = None
    if args.file_label:
        for f in files:
            if f.get("label", "") == args.file_label:
                chosen = f
                break
        if chosen is None:
            raise RuntimeError(f"File label not found: {args.file_label}")
    else:
        scored = []
        for f in files:
            label = f.get("label", "")
            score = _score_label(label)
            scored.append((score, label, f))
        scored.sort(key=lambda x: (x[0], x[1]), reverse=True)
        best_score, _, chosen = scored[0]
        if best_score < 2:
            labels = [f.get("label", "") for f in files]
            raise RuntimeError("No suitable file found; labels: " + ", ".join(labels))

    data_file = chosen.get("dataFile", {})
    file_id = data_file.get("id")
    label = chosen.get("label", f"dataverse_file_{file_id}")

    if file_id is None:
        raise RuntimeError("Could not find file id in metadata")

    safe_label = re.sub(r"[^A-Za-z0-9._-]+", "_", label)
    out_path = os.path.join(args.outdir, safe_label)

    _download_file(file_id, out_path)

    sha256 = hashlib.sha256()
    with open(out_path, "rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            sha256.update(chunk)

    access_log = {
        "persistent_id": PERSISTENT_ID,
        "api_url": API_URL,
        "file_id": file_id,
        "file_label": label,
        "downloaded_at": _utc_now_iso(),
        "sha256": sha256.hexdigest(),
    }
    with open(os.path.join(args.outdir, "access_log.json"), "w", encoding="utf-8") as f:
        json.dump(access_log, f, indent=2)

    manifest = {
        "file_path": out_path,
        "file_label": label,
        "file_id": file_id,
    }
    with open(os.path.join(args.outdir, "manifest.json"), "w", encoding="utf-8") as f:
        json.dump(manifest, f, indent=2)

    print(f"Downloaded: {out_path}")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        sys.exit(1)
