#!/usr/bin/env python3
"""Forward-only UTC daily top 50. Does not modify the broad collector or its CSV."""
import argparse
import csv
import hashlib
import io
import json
import os
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.request import Request, urlopen

URL = 'https://openrouter.ai/api/frontend/v1/rankings/models?view=day'
FIELDS = ['date', 'rank', 'model_permaslug', 'input_tokens', 'output_tokens',
          'total_tokens', 'requests', 'variant_count', 'ranking_unit',
          'measurement_window', 'source_url', 'observed_at_utc', 'raw_sha256']

def count(row, key):
    value = row.get(key)
    if isinstance(value, bool) or not isinstance(value, int) or value < 0:
        raise ValueError(f'Invalid nonnegative integer: {key}')
    return value

def select(payload, expected, unit):
    rows = payload.get('data')
    if not isinstance(rows, list) or not rows:
        raise ValueError('Missing nonempty data list')
    totals = {}
    seen = set()
    for row in rows:
        if str(row.get('date', ''))[:10] != expected:
            raise ValueError('Source returned unexpected activity date; refusing mixed windows')
        base = row.get('model_permaslug')
        variant = row.get('variant')
        # Frontend sometimes emits a blank all-zero placeholder, not a model.
        if base == '' and all(row.get(k) == 0 for k in ('total_prompt_tokens','total_completion_tokens','count')):
            continue
        if not isinstance(base, str) or not base or not isinstance(variant, str) or not variant:
            raise ValueError('Missing model/variant identity')
        key = (base, variant)
        if key in seen:
            raise ValueError('Duplicate source model/variant')
        seen.add(key)
        identity = base if unit == 'base_model' else row.get('variant_permaslug')
        if not isinstance(identity, str) or not identity:
            raise ValueError('Missing variant_permaslug')
        v = totals.setdefault(identity, [0, 0, 0, 0])
        v[0] += count(row, 'total_prompt_tokens')
        v[1] += count(row, 'total_completion_tokens')
        v[2] += count(row, 'count')
        v[3] += 1
    if len(totals) < 50:
        raise ValueError('Fewer than 50 source models; refusing incomplete day')
    return sorted(totals.items(), key=lambda x: (-(x[1][0]+x[1][1]), x[0]))[:50]

def collect(output, raw_dir, unit, fetch=None, now=None):
    now = now or datetime.now(timezone.utc)
    expected = (now.date()-timedelta(days=1)).isoformat()
    stamp = now.isoformat().replace('+00:00', 'Z')
    output, raw_dir = Path(output), Path(raw_dir)
    old = []
    if output.exists():
        with output.open(newline='') as f:
            reader = csv.DictReader(f)
            if reader.fieldnames != FIELDS:
                raise ValueError('Existing CSV schema differs')
            old = list(reader)
        if any(r['ranking_unit'] != unit or r['measurement_window'] != 'day' for r in old):
            raise ValueError('Refusing mixed ranking units/windows')
        if any(r['date'] == expected for r in old):
            existing = [r for r in old if r['date'] == expected]
            if len(existing) != 50 or len({r['model_permaslug'] for r in existing}) != 50 or {r['rank'] for r in existing} != {str(i) for i in range(1,51)}:
                raise ValueError('Existing date is partial or corrupt; manual review required')
            print(f'{expected}: complete 50 rows already present, unchanged')
            return 0
    body = fetch() if fetch else urlopen(Request(URL, headers={'User-Agent':'TokenomicsDailyTop50/1.0', 'Accept':'application/json'}), timeout=60).read()
    selected = select(json.loads(body), expected, unit)
    digest = hashlib.sha256(body).hexdigest()
    raw_dir.mkdir(parents=True, exist_ok=True)
    # Immutable response paired with source/hash and observed time in output.
    raw = raw_dir / f'{expected}_{digest}.json'
    raw.write_bytes(body)
    records = []
    for rank, (model, (inp, out, requests, variants)) in enumerate(selected, 1):
        records.append(dict(zip(FIELDS, [expected, rank, model, inp, out, inp+out, requests, variants, unit, 'day', URL, stamp, digest])))
    output.parent.mkdir(parents=True, exist_ok=True)
    temp = output.with_suffix('.tmp')
    with temp.open('w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=FIELDS);writer.writeheader();writer.writerows(old+records)
        f.flush();os.fsync(f.fileno())
    os.replace(temp, output)
    print(f'{expected}: added 50 actual daily {unit} rows; raw sha256={digest}')
    return 50

if __name__ == '__main__':
    p=argparse.ArgumentParser();p.add_argument('--ranking-unit', choices=['base_model','variant'], required=True)
    p.add_argument('--output', default='tokenomics_data/daily_top50.csv')
    p.add_argument('--raw-dir', default='tokenomics_data/daily_top50_raw')
    a=p.parse_args();collect(a.output,a.raw_dir,a.ranking_unit)
