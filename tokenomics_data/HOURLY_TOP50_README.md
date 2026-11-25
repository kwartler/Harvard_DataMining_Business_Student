# Hourly day-view snapshot collector

This addition does not change `fetch_tokens.py`, `fetch_daily_top50.py`, `model_tokens.csv`, `daily_top50.csv`, the registry, or any existing workflow.

## Why it exists

Around 2026-11-23 the OpenRouter frontend rankings day view stopped returning rows for the previous UTC day and began returning rows labeled with hourly timestamps of the current UTC day. The daily top-50 job validates that every row matches the expected date and correctly refuses that data, so no daily rows were written for 2026-11-23. This job records what the day view actually returns, about once per hour, so that daily totals can be reconstructed from the hourly labels and the endpoint's behavior can be watched over time.

## What it does

`fetch_hourly_top50.py` fetches `https://openrouter.ai/api/frontend/v1/rankings/models?view=day`, checks only the response structure (there is deliberately no date expectation, because the endpoint's behavior is the thing under observation), and appends each newly seen (`source_date`, `model_permaslug`, `variant`) row to `hourly_top50.csv`. The source caches responses for about five minutes and appears to update hourly; if a fetch returns a payload identical to the previous one, the run is a no-op and commits nothing.

The workflow runs hourly at minute 14 UTC plus manual dispatch. As with the daily job, GitHub may delay or skip scheduled runs, so the cadence is best-effort. Rows are variant-level and unranked. To reconstruct a UTC day in the style of `daily_top50.csv`, sum variants by exact `model_permaslug` and use each model's row from the latest hour label of that day, then rank by prompt + completion tokens.

## Caveats

- Whether a row's values are cumulative for the day up to its hour label, or only that hour's activity, had not been determined when this was installed. The first days of this CSV plus the raw snapshots settle it; update this README once known.
- The CSV keeps the first observation of each hour bucket. If the source revises a bucket later, the latest raw snapshot per UTC fetch day in `hourly_top50_raw/` and the 30-day run artifacts capture the revision.
- Rows labeled with any date are recorded as returned, including unexpected ones. Do not assume a row's label means the same thing it meant before 2026-11-23.
- The source represents public OpenRouter rankings traffic, not all AI usage. No API key is used and no historical API calls are made.

## CSV fields

`fetched_at_utc` (when this job fetched), `source_date` (the row's date label verbatim from the source, e.g. `2026-11-24 22:00:00`), `model_permaslug`, `variant`, `variant_permaslug`, `prompt_tokens`, `completion_tokens`, `total_tokens`, `requests`, `source_view` (`day`), `source_url`, `payload_sha256` (hash of the raw response the row came from).

## Commits, storage, rollback

One commit per changed payload, titled "data: hourly day-view snapshot <date>", pushed with the same no-force fetch/rebase helper as the other additive jobs and serialized with them in the same concurrency group. Expect up to roughly one commit per hour while the endpoint updates hourly. `hourly_state.json` records the last payload hash for no-op detection. To stop the job, disable the workflow; to remove it, revert its commit. Neither touches the broad collector or the daily top-50 job.

## Rolling live file and monthly archives

`hourly_top50.csv` keeps only the last 14 days of rows (by `fetched_at_utc`). After each fetch, `rotate_hourly_top50.py` moves older rows into `hourly_archive/hourly_top50_YYYY-MM.csv.gz`, one gzip file per month of fetch time. Nothing is deleted: the full history is the archives plus the live file, and raw payloads stay in `hourly_top50_raw/` and git history.

To rebuild the full history from the repo root:

- R: `source("tokenomics_data/rebuild_hourly_history.R")` creates a data frame called `hourly` (archives plus live file, duplicates removed). `read.csv` reads the `.gz` files directly.
- Terminal: `bash tokenomics_data/rebuild_hourly_history.sh` writes `tokenomics_data/hourly_top50_full.csv` with one header.

To change the window, edit `KEEP_DAYS` in `rotate_hourly_top50.py`.
