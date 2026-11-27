#!/usr/bin/env python3
"""Build a daily top 50 from hourly day-view snapshots.

Companion to fetch_hourly_top50.py. Around 2026-11-23 OpenRouter's day view
stopped returning previous-day rows and began returning trailing-24h totals
labeled with each model's last-active hour, so true calendar-day totals are
no longer available from the source. This script derives the closest honest
daily series from the hourly snapshots we already collect: for each completed
UTC day it takes every model's value at the day's latest recorded hour bucket
(a trailing-24h total ending at that hour), groups variants into base models,
ranks the top 50, and appends them to a separate CSV. It never rewrites a
completed day and never touches the broad collector, the daily top-50 job,
or their CSVs.
"""
import argparse
import csv
import hashlib
import json
import os
from datetime import datetime, timezone
from pathlib import Path

SOURCE_URL = 'https://openrouter.ai/api/frontend/v1/rankings/models?view=day'
HOURLY_FIELDS = ['fetched_at_utc', 'source_date', 'model_permaslug', 'variant',
                 'variant_permaslug', 'prompt_tokens', 'completion_tokens',
                 'total_tokens', 'requests', 'source_view', 'source_url',
                 'payload_sha256']
FIELDS = ['date', 'rank', 'model_permaslug', 'input_tokens', 'output_tokens',
          'total_tokens', 'requests', 'variant_count', 'ranking_unit',
          'measurement_window', 'source_url', 'observed_at_utc', 'raw_sha256',
          'source_hour_utc']
# A real day has ~600 base models; a partial collection day has far fewer and
# would produce an arbitrary bottom of the top 50, so it is left as a gap.
MIN_MODELS = 300


def latest_rows(hourly_path, day):
    """Per (base model, variant), the row at the day's latest hour bucket."""
    best = {}
    with hourly_path.open(newline='') as f:
        reader = csv.DictReader(f)
        if reader.fieldnames != HOURLY_FIELDS:
            raise ValueError('Hourly CSV schema differs')
        for row in reader:
            source_date = row['source_date']
            if source_date[:10] != day:
                continue
            key = (row['model_permaslug'], row['variant'])
            if key not in best or source_date > best[key]['source_date']:
                best[key] = row
    return best


def build_day(hourly_path, day):
    best = latest_rows(hourly_path, day)
    totals = {}
    for row in best.values():
        base = row['model_permaslug']
        v = totals.setdefault(base, [0, 0, 0, 0])
        v[0] += int(row['prompt_tokens'])
        v[1] += int(row['completion_tokens'])
        v[2] += int(row['requests'])
        v[3] += 1
    if len(totals) < MIN_MODELS:
        return None, len(totals)
    ranked = sorted(totals.items(), key=lambda x: (-(x[1][0] + x[1][1]), x[0]))[:50]
    digest = hashlib.sha256(json.dumps(
        [[k[0], k[1], v['source_date']] for k, v in sorted(best.items())]
    ).encode()).hexdigest()
    hour_counts = {}
    for row in best.values():
        h = row['source_date'][11:13]
        hour_counts[h] = hour_counts.get(h, 0) + 1
    modal_hour = max(hour_counts.items(), key=lambda kv: kv[1])[0]
    return (ranked, digest, modal_hour), None


def collect(hourly, output, now=None):
    now = now or datetime.now(timezone.utc)
    today = now.date().isoformat()
    stamp = now.isoformat().replace('+00:00', 'Z')
    hourly, output = Path(hourly), Path(output)
    old = []
    done = set()
    if output.exists():
        with output.open(newline='') as f:
            reader = csv.DictReader(f)
            if reader.fieldnames != FIELDS:
                raise ValueError('Existing CSV schema differs')
            old = list(reader)
        done = {r['date'] for r in old}
        if any(r['ranking_unit'] != 'base_model' or
               r['measurement_window'] != 'rolling_24h' for r in old):
            raise ValueError('Refusing mixed ranking units/windows')
    days = set()
    with hourly.open(newline='') as f:
        for row in csv.DictReader(f):
            day = row['source_date'][:10]
            if day < today:
                days.add(day)
    records = []
    for day in sorted(days):
        if day in done:
            continue
        result, model_count = build_day(hourly, day)
        if result is None:
            print(f'{day}: only {model_count} base models recorded '
                  f'(need {MIN_MODELS}); leaving as a gap (partial collection day)')
            continue
        (ranked, digest, last_hour) = result
        for rank, (model, (inp, out, requests, variants)) in enumerate(ranked, 1):
            records.append(dict(zip(FIELDS, [
                day, rank, model, inp, out, inp + out, requests, variants,
                'base_model', 'rolling_24h', SOURCE_URL, stamp, digest,
                f'{day} {last_hour}:00:00'])))
        print(f'{day}: added 50 rolling-24h rows (modal source hour {last_hour}:00 UTC)')
    if not records:
        print('No new completed days to add')
        return 0
    output.parent.mkdir(parents=True, exist_ok=True)
    temp = output.with_suffix('.tmp')
    with temp.open('w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=FIELDS)
        writer.writeheader()
        writer.writerows(old + records)
        f.flush()
        os.fsync(f.fileno())
    os.replace(temp, output)
    return len(records)


if __name__ == '__main__':
    p = argparse.ArgumentParser()
    p.add_argument('--hourly', default='tokenomics_data/hourly_top50.csv')
    p.add_argument('--output', default='tokenomics_data/daily_top50_rolling24h.csv')
    a = p.parse_args()
    collect(a.hourly, a.output)
