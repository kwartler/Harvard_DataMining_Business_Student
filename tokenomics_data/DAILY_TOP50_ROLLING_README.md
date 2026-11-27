# Daily top 50, rolling 24h window

`daily_top50_rolling24h.csv` is the daily top-50 series that replaces
`daily_top50.csv` from 2026-11-25 onward. It does not change
`fetch_tokens.py`, `fetch_daily_top50.py`, `fetch_hourly_top50.py`,
`model_tokens.csv`, `daily_top50.csv`, or any existing workflow's collection
behavior. Rollback: delete this README, `build_daily_top50_rolling.py`, and
`daily_top50_rolling24h.csv`, and remove the one added step in
`tokenomics_hourly.yml`.

## Why a new series

Around 2026-11-23 OpenRouter's rankings day view
(`api/frontend/v1/rankings/models?view=day`) stopped returning previous-day
rows. It now returns trailing-24-hour totals labeled with each model's
last-active hour of the current UTC day, and it ignores a date parameter.
True midnight-to-midnight daily totals are no longer available from this
source, and they cannot be reconstructed exactly: differencing a rolling-24h
value between two snapshots gives tokens gained this hour minus tokens from
the same hour yesterday, never a calendar-day total. The original daily job
(`fetch_daily_top50.py`) correctly refuses the changed feed and is archived.

## What each row means

For each completed UTC day, `build_daily_top50_rolling.py` reads
`hourly_top50.csv` (collected about every 6 hours by the hourly snapshot job),
takes every model+variant's value at the day's latest recorded hour bucket,
merges variants into each exact base `model_permaslug` (same grouping rule as
the other collectors; batch/standard summed, different dated releases kept
separate), ranks by prompt + completion tokens, and appends the top 50.

`measurement_window` is `rolling_24h`: each value is a trailing-24-hour total
ending at the hour in `source_hour_utc`, which is typically the day's last
captured snapshot (around 20:00-21:00 UTC at the current collection cadence),
not midnight to midnight. A trailing-24h window ending near day end covers
almost the same traffic as the calendar day, so days are comparable to each
other, but do not concatenate this series with `daily_top50.csv`
(`measurement_window` = `day`) as though the windows were identical.

A completed day is written once and never rewritten by reruns. A day with
fewer than 300 recorded base models (a partial collection day) is left as a
gap rather than producing an arbitrary bottom of the top 50; 2026-11-24 is
such a gap, and 2026-11-23 predates hourly collection.

CSV fields: the `daily_top50.csv` fields plus `source_hour_utc`, the source
hour bucket of each row's value.

## Operation

One added step in `tokenomics_hourly.yml` runs the builder after each hourly
snapshot and the result is committed with the same push. The builder is
idempotent: it appends only completed UTC days that are not already present.
