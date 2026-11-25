#!/usr/bin/env python3
"""Hourly snapshot of the OpenRouter frontend rankings day view.

Diagnostic companion to fetch_daily_top50.py. Around 2026-11-23 the day view
stopped returning previous-day rows and began returning hourly rows labeled
with the current UTC day, which the daily job correctly refuses. This job
records exactly what the endpoint returns, about once per hour, so daily
totals can be reconstructed and endpoint behavior can be observed over time.

It does not modify the broad collector, the daily top-50 job, or their CSVs.
"""
import argparse
import csv
import hashlib
import json
import os
from datetime import datetime, timezone
from pathlib import Path
from urllib.request import Request, urlopen

URL = 'https://openrouter.ai/api/frontend/v1/rankings/models?view=day'
FIELDS = ['fetched_at_utc', 'source_date', 'model_permaslug', 'variant',
          'variant_permaslug', 'prompt_tokens', 'completion_tokens',
          'total_tokens', 'requests', 'source_view', 'source_url',
          'payload_sha256']
METRICS = ('total_prompt_tokens', 'total_completion_tokens', 'count')

def count(row, key):
    value = row.get(key)
    if isinstance(value, bool) or not isinstance(value, int) or value < 0:
        raise ValueError(f'Invalid nonnegative integer: {key}')
    return value

def select(payload):
    rows = payload.get('data')
    if not isinstance(rows, list) or not rows:
        raise ValueError('Missing nonempty data list')
    out = []
    seen = set()
    for row in rows:
        base = row.get('model_permaslug')
        variant = row.get('variant')
        # Frontend sometimes emits a blank all-zero placeholder, not a model.
        if base == '' and all(row.get(k) == 0 for k in METRICS):
            continue
        if not isinstance(base, str) or not base or not isinstance(variant, str) or not variant:
            raise ValueError('Missing model/variant identity')
        date = row.get('date')
        if not isinstance(date, str) or not date:
            raise ValueError('Missing source date')
        key = (date, base, variant)
        if key in seen:
            raise ValueError('Duplicate source date/model/variant in one payload')
        seen.add(key)
        vslug = row.get('variant_permaslug')
        if not isinstance(vslug, str) or not vslug:
            raise ValueError('Missing variant_permaslug')
        prompt = count(row, 'total_prompt_tokens')
        completion = count(row, 'total_completion_tokens')
        requests = count(row, 'count')
        out.append((date, base, variant, vslug, prompt, completion, requests))
    return out

def collect(output, raw_dir, state_path, fetch=None, now=None):
    now = now or datetime.now(timezone.utc)
    stamp = now.isoformat().replace('+00:00', 'Z')
    output, raw_dir, state_path = Path(output), Path(raw_dir), Path(state_path)
    body = fetch() if fetch else urlopen(
        Request(URL, headers={'User-Agent': 'TokenomicsHourlyTop50/1.0',
                              'Accept': 'application/json'}), timeout=60).read()
    digest = hashlib.sha256(body).hexdigest()
    state = {}
    if state_path.exists():
        state = json.loads(state_path.read_text())
    if state.get('last_payload_sha256') == digest:
        print(f"{stamp}: payload unchanged since {state.get('last_fetch_at_utc')}, no-op")
        return 0
    rows = select(json.loads(body))
    old = []
    old_keys = set()
    if output.exists():
        with output.open(newline='') as f:
            reader = csv.DictReader(f)
            if reader.fieldnames != FIELDS:
                raise ValueError('Existing CSV schema differs')
            old = list(reader)
        old_keys = {(r['source_date'], r['model_permaslug'], r['variant']) for r in old}
    records = []
    for date, base, variant, vslug, prompt, completion, requests in rows:
        if (date, base, variant) in old_keys:
            continue  # first observation of an hour bucket is kept
        records.append(dict(zip(FIELDS, [stamp, date, base, variant, vslug,
                                         prompt, completion, prompt + completion,
                                         requests, 'day', URL, digest])))
    raw_dir.mkdir(parents=True, exist_ok=True)
    # Latest snapshot per UTC fetch day, overwritten through the day.
    (raw_dir / f'{now.date().isoformat()}.json').write_bytes(body)
    state_path.write_text(json.dumps(
        {'last_payload_sha256': digest, 'last_fetch_at_utc': stamp}, indent=2) + '\n')
    if not records:
        print(f'{stamp}: payload changed but all rows already recorded')
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
    print(f'{stamp}: added {len(records)} hourly rows; raw sha256={digest}')
    return len(records)

if __name__ == '__main__':
    p = argparse.ArgumentParser()
    p.add_argument('--output', default='tokenomics_data/hourly_top50.csv')
    p.add_argument('--raw-dir', default='tokenomics_data/hourly_top50_raw')
    p.add_argument('--state', default='tokenomics_data/hourly_state.json')
    a = p.parse_args()
    collect(a.output, a.raw_dir, a.state)
