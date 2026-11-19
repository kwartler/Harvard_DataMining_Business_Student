#!/usr/bin/env python3
"""Downdetector big-brand outage collector.

Additive collector: appends new 15-minute report buckets for each brand in
brands.json to one long CSV (downdetector_outages.csv). Same design rules as
the repo's tokenomics collectors:

  - additive only: never edits or deletes existing rows
  - a failed poll is recorded as a failed row, never fabricated numbers
  - originals and workflow retries are distinguishable (run id + run attempt)
  - row counts are verified after every run; verification failure exits 1

How the data is read: each Downdetector status page embeds its 24-hour chart
and "most reported problems" breakdown in the page source (Next.js flight
payload as ChartDataPointType and LastIndicatorType records). This script
extracts them with tolerant text parsing, no third-party packages.

Stdlib only. Python 3.10+.
"""

import csv
import json
import os
import random
import re
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime, timezone
from http.cookiejar import CookieJar
from pathlib import Path

HERE = Path(__file__).resolve().parent
BRANDS_PATH = HERE / "brands.json"
CSV_PATH = HERE / "downdetector_outages.csv"

PAGE_URL = "https://downdetector.com/status/{slug}/"

# A small rotation of ordinary desktop browser agents. The point is politeness
# (look like a normal read), not evasion: if Cloudflare still challenges the
# fetch, the run records a cloudflare_challenge row and moves on.
USER_AGENTS = [
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36",
]

FETCH_TIMEOUT_S = 25
ATTEMPTS = 3
BACKOFF_S = [5, 20, 60]          # sleep before attempts 2 and 3 uses the tail of this list
BRAND_PAUSE_S = (3.0, 6.0)       # polite pacing between brands

FIELDNAMES = [
    "collected_at_utc",      # when this run fetched the page
    "workflow_run_id",       # GITHUB_RUN_ID, or "local"
    "workflow_run_attempt",  # GITHUB_RUN_ATTEMPT; >1 means a retried workflow run
    "brand",
    "category",
    "slug",
    "bucket_ts_utc",         # ok rows: 15-minute bucket floored to the quarter hour;
                     # failure rows: exact poll moment (keeps keys distinct so a
                     # later run can still backfill the bucket with real data)
    "reports",               # user reports Downdetector counted in the bucket
    "baseline",              # Downdetector's baseline for the bucket
    "page_status",           # success | warning | danger (best effort)
    "top1_label", "top1_count", "top1_pct",
    "top2_label", "top2_count", "top2_pct",
    "top3_label", "top3_count", "top3_pct",
    "fetch_status",          # ok | cloudflare_challenge | http_error | timeout | parse_error
    "http_status",
    "error",
]

# Parsed from the page after unescaping one layer of flight-payload quoting.
POINT_RE = re.compile(
    r'"timestampUtc":\s*"([^"]+)"\s*,\s*"reportsValue":\s*(\d+)\s*,\s*"baselineValue":\s*([\d.]+)'
)
INDICATOR_RE = re.compile(
    r'"localizedName":\s*"([^"]+)"\s*,\s*"count":\s*(\d+)\s*,\s*"percentage":\s*([\d.]+)\s*,\s*"id":\s*\d+'
)
STATUS_RE = re.compile(r'"status":\s*"(success|warning|danger)"')


def utcnow():
    return datetime.now(timezone.utc)


def iso(dt):
    return dt.strftime("%Y-%m-%dT%H:%M:%SZ")


def quarter_floor(dt):
    """Floor a datetime to its 15-minute bucket start (UTC)."""
    return dt.replace(minute=(dt.minute // 15) * 15, second=0, microsecond=0)


def unescape_flight(text):
    """Undo the quoting layers of the embedded Next.js flight payload.

    The raw page contains sequences like \\"timestampUtc\\". Collapsing the
    backslash-quote layers once or twice yields plain JSON-ish text the
    regexes above can read. Tolerant to either depth.
    """
    return text.replace('\\\\"', '"').replace('\\"', '"')


def parse_page(html):
    """Return (points, indicators, status) from one status page's HTML.

    points: list of (bucket_dt, reports:int, baseline:float)
    indicators: list of (label, count:int, pct:float) most-reported-first
    status: "success" | "warning" | "danger" | ""
    """
    text = unescape_flight(html)
    points = []
    for ts, reports, baseline in POINT_RE.findall(text):
        try:
            raw = datetime.fromisoformat(ts.replace("Z", "+00:00"))
            points.append((quarter_floor(raw.astimezone(timezone.utc)),
                           int(reports), float(baseline)))
        except ValueError:
            continue
    indicators = [(label, int(count), float(pct))
                  for label, count, pct in INDICATOR_RE.findall(text)]
    m = STATUS_RE.search(text)
    status = m.group(1) if m else ""
    return points, indicators, status


def make_opener():
    cj = CookieJar()
    return urllib.request.build_opener(urllib.request.HTTPCookieProcessor(cj))


def fetch_page(opener, slug):
    """Fetch one status page. Returns (html, http_status) or raises.

    Raises FetchError with kind in {cloudflare_challenge, http_error,
    timeout}. Never fabricates: any failure raises.
    """
    url = PAGE_URL.format(slug=slug)
    last = None
    for i in range(ATTEMPTS):
        if i:
            time.sleep(BACKOFF_S[min(i - 1, len(BACKOFF_S) - 1)] + random.uniform(0, 3))
        req = urllib.request.Request(url, headers={
            "User-Agent": random.choice(USER_AGENTS),
            "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            "Accept-Language": "en-US,en;q=0.9",
        })
        try:
            with opener.open(req, timeout=FETCH_TIMEOUT_S) as resp:
                body = resp.read().decode("utf-8", errors="replace")
                status = resp.status
        except urllib.error.HTTPError as e:
            body = ""
            try:
                body = e.read().decode("utf-8", errors="replace")
            except Exception:
                pass
            if e.code in (403, 429, 503) and ("__cf_chl" in body or "cloudflare" in body.lower()
                                              or e.code == 403):
                last = FetchError("cloudflare_challenge", e.code, "Cloudflare challenge/bot block")
            else:
                last = FetchError("http_error", e.code, f"HTTP {e.code}")
            continue
        except (urllib.error.URLError, TimeoutError, OSError) as e:
            last = FetchError("timeout", "", str(e)[:120])
            continue
        if "__cf_chl" in body and "ChartDataPointType" not in body:
            last = FetchError("cloudflare_challenge", status, "Cloudflare interstitial page")
            continue
        return body, status
    raise last


class FetchError(Exception):
    def __init__(self, kind, http_status, message):
        super().__init__(message)
        self.kind = kind
        self.http_status = http_status
        self.message = message


def existing_keys(path):
    """(slug, bucket_ts) pairs already stored, so reruns never duplicate."""
    keys = set()
    if not path.exists():
        return keys
    with path.open(newline="") as f:
        for row in csv.DictReader(f):
            keys.add((row["slug"], row["bucket_ts_utc"]))
    return keys


def count_rows(path):
    if not path.exists():
        return 0
    with path.open() as f:
        return max(0, sum(1 for _ in f) - 1)


def main():
    if not BRANDS_PATH.exists():
        print(f"missing brand config: {BRANDS_PATH}", file=sys.stderr)
        return 1
    brands = json.loads(BRANDS_PATH.read_text())["brands"]

    run_id = os.environ.get("GITHUB_RUN_ID", "local")
    run_attempt = os.environ.get("GITHUB_RUN_ATTEMPT", "1")
    collected_at = iso(utcnow())
    # The quarter-hour bucket currently open at fetch time. Only buckets
    # strictly older than this are final enough to store.
    open_bucket = quarter_floor(utcnow())

    keys = existing_keys(CSV_PATH)
    before = count_rows(CSV_PATH)
    new_rows = []
    summary = {}

    opener = make_opener()
    for b in brands:
        slug, name, cat = b["slug"], b["brand"], b.get("category", "")
        try:
            html, http_status = fetch_page(opener, slug)
        except FetchError as e:
            # Failure rows key on the exact poll moment, not the bucket, so a
            # later successful run can still backfill real data for that span.
            key = (slug, collected_at)
            if key not in keys:
                keys.add(key)
                new_rows.append(base_row(collected_at, run_id, run_attempt, b, collected_at)
                                | {"fetch_status": e.kind, "http_status": e.http_status,
                                   "error": e.message})
            summary[name] = e.kind
            print(f"[{e.kind}] {name} ({slug}): {e.message}", file=sys.stderr)
        else:
            points, indicators, status = parse_page(html)
            if not points:
                key = (slug, collected_at)
                if key not in keys:
                    keys.add(key)
                    new_rows.append(base_row(collected_at, run_id, run_attempt, b, collected_at)
                                    | {"fetch_status": "parse_error", "http_status": http_status,
                                       "error": "page fetched but no chart data found"})
                summary[name] = "parse_error"
                print(f"[parse_error] {name} ({slug}): no chart data in page", file=sys.stderr)
            else:
                written = 0
                for bucket, reports, baseline in points:
                    if bucket >= open_bucket:
                        continue  # still filling; next run picks it up
                    key = (slug, iso(bucket))
                    if key in keys:
                        continue  # already stored by an earlier run (24h window overlaps)
                    keys.add(key)
                    row = base_row(collected_at, run_id, run_attempt, b, iso(bucket))
                    row.update({"reports": reports, "baseline": baseline,
                                "page_status": status, "fetch_status": "ok",
                                "http_status": http_status})
                    for rank, ind in enumerate(indicators[:3], start=1):
                        row[f"top{rank}_label"] = ind[0]
                        row[f"top{rank}_count"] = ind[1]
                        row[f"top{rank}_pct"] = round(ind[2], 2)
                    new_rows.append(row)
                    written += 1
                summary[name] = f"ok +{written}"
        time.sleep(random.uniform(*BRAND_PAUSE_S))

    if new_rows:
        write_header = not CSV_PATH.exists()
        with CSV_PATH.open("a", newline="") as f:
            w = csv.DictWriter(f, fieldnames=FIELDNAMES)
            if write_header:
                w.writeheader()
            w.writerows(new_rows)

    # Verification, same spirit as the tokenomics collectors: the CSV only
    # ever grows and keys stay unique. Any violation fails the run loudly.
    after = count_rows(CSV_PATH)
    problems = []
    if after != before + len(new_rows):
        problems.append(f"row count mismatch: before={before} wrote={len(new_rows)} after={after}")
    all_keys = existing_keys(CSV_PATH)
    if len(all_keys) != after:
        problems.append(f"duplicate (slug,bucket) keys: rows={after} unique={len(all_keys)}")
    for p in problems:
        print(f"VERIFY FAIL: {p}", file=sys.stderr)

    print(f"run {run_id} attempt {run_attempt}: {after - before} rows appended, {after} total")
    for name, s in summary.items():
        print(f"  {name}: {s}")
    return 1 if problems else 0


def base_row(collected_at, run_id, run_attempt, brand, bucket_iso):
    return {k: "" for k in FIELDNAMES} | {
        "collected_at_utc": collected_at,
        "workflow_run_id": run_id,
        "workflow_run_attempt": run_attempt,
        "brand": brand["brand"],
        "category": brand.get("category", ""),
        "slug": brand["slug"],
        "bucket_ts_utc": bucket_iso,
    }


if __name__ == "__main__":
    sys.exit(main())
