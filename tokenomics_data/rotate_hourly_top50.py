#!/usr/bin/env python3
"""Move hourly_top50.csv rows older than KEEP_DAYS into monthly gzip archives.

Run after fetch_hourly_top50.py, before push_collected.sh. Rows are archived by
the month of fetched_at_utc. Archive appends skip rows already present, so a
re-run is safe. Nothing is deleted from the live file until the archive write
has been flushed and renamed into place.
"""
import csv, gzip, os, sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

LIVE = Path('tokenomics_data/hourly_top50.csv')
ARCH = Path('tokenomics_data/hourly_archive')
KEEP_DAYS = 28

def key(r):
    return (r['fetched_at_utc'], r['source_date'], r['model_permaslug'], r['variant'])

def rotate(live=LIVE, arch=ARCH, keep_days=KEEP_DAYS, now=None):
    now = now or datetime.now(timezone.utc)
    arch.mkdir(parents=True, exist_ok=True)  # push step expects this folder to exist
    cutoff = (now - timedelta(days=keep_days)).strftime('%Y-%m-%dT%H:%M:%S')
    with live.open(newline='') as f:
        reader = csv.DictReader(f); fields = reader.fieldnames; rows = list(reader)
    old = [r for r in rows if r['fetched_at_utc'] < cutoff]
    if not old:
        print('nothing to archive'); return 0
    keep = [r for r in rows if r['fetched_at_utc'] >= cutoff]
    arch.mkdir(parents=True, exist_ok=True)
    months = {}
    for r in old: months.setdefault(r['fetched_at_utc'][:7], []).append(r)
    for month, mrows in months.items():
        path = arch / f'hourly_top50_{month}.csv.gz'
        existing = []
        if path.exists():
            with gzip.open(path, 'rt', newline='') as f: existing = list(csv.DictReader(f))
        seen = {key(r) for r in existing}
        merged = existing + [r for r in mrows if key(r) not in seen]
        tmp = path.with_suffix('.tmp')
        with gzip.open(tmp, 'wt', newline='') as f:
            w = csv.DictWriter(f, fieldnames=fields); w.writeheader(); w.writerows(merged)
        os.replace(tmp, path)
    tmp = live.with_suffix('.tmp')
    with tmp.open('w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields); w.writeheader(); w.writerows(keep)
        f.flush(); os.fsync(f.fileno())
    os.replace(tmp, live)
    print(f'archived {len(old)} rows into {len(months)} month file(s); live keeps {len(keep)}')
    return len(old)

if __name__ == '__main__':
    rotate()
