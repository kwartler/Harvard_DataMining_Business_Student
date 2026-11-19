# Downdetector big-brand outage collector

Polls 20 big-brand Downdetector status pages every 15 minutes and appends
their 15-minute report buckets to one long CSV, `downdetector_outages.csv`.
Same house rules as the tokenomics collectors: additive only, failures are
recorded as failures (never fabricated), originals and workflow retries are
distinguishable, row counts are verified after every run, and rollback is
just git history.

## Files

- `downdetector_data/brands.json` - the brand list. Add or remove brands
  here; nothing else changes. All 20 slugs were verified live on
  2026-11-18.
- `downdetector_data/fetch_downdetector.py` - the scraper. Stdlib only,
  no pip installs. Safe to run by hand: `python downdetector_data/fetch_downdetector.py`.
- `downdetector_data/downdetector_outages.csv` - the data, created on the
  first run and appended forever after.
- `.github/workflows/downdetector_collect.yml` - the 15-minute GitHub
  Action. Commits only when new rows exist; rebase + push with retries,
  never force-push. It does not touch the tokenomics files or workflows.

## What each run does

1. Reads `brands.json`.
2. For each brand, fetches `https://downdetector.com/status/<slug>/` with
   browser-like headers, a cookie jar, up to 3 attempts with backoff, and a
   random 3-6s pause between brands (politeness; it also lowers the chance
   of a Cloudflare challenge).
3. Parses the chart data embedded in the page (24h of 15-minute buckets:
   report count + baseline) and the "most reported problems" breakdown.
4. Appends only buckets not already in the CSV. Each page carries 24h of
   buckets, so a late or missed run backfills automatically. The bucket
   still open at poll time is skipped; the next run picks it up.
5. A failed poll writes one failure row (`fetch_status` = `cloudflare_challenge`,
   `http_error`, `timeout`, or `parse_error`) keyed to the exact poll
   moment, so a later run can still backfill real numbers for that span.
6. Verifies the CSV afterwards: row count only grows and (slug, bucket)
   keys stay unique. A verification problem exits nonzero and shows up as
   a failed run.

Expected size: 20 brands x 96 buckets/day is about 1,900 rows/day,
roughly 150KB/day as CSV, under 60MB/year.

## Columns

`collected_at_utc, workflow_run_id, workflow_run_attempt, brand, category,
slug, bucket_ts_utc, reports, baseline, page_status, top1_label, top1_count,
top1_pct, top2_label, top2_count, top2_pct, top3_label, top3_count,
top3_pct, fetch_status, http_status, error`

Notes:

- `bucket_ts_utc` is floored to the quarter hour for `ok` rows. Failure
  rows store the exact poll moment instead (keeps keys distinct).
- `page_status` is Downdetector's own status word (`success` / `warning` /
  `danger`), best effort.
- `workflow_run_attempt` above 1 marks a retried workflow run, so originals
  and retries stay distinguishable.
- Values are first-observed per bucket. Downdetector occasionally revises
  recent buckets; those small revisions are not tracked (append-only).

## Risks and honest limitations

1. **Cloudflare bot protection is the big one.** A plain script fetch from
   a datacenter IP (tested 2026-11-18) gets a 403 challenge page, while a
   real browser passes. GitHub-hosted runners are datacenter IPs, so
   expect some or all polls to land as `cloudflare_challenge` rows. The
   collector treats this as recorded failure, never a crash, and backfills
   whenever a fetch does get through. If blocking turns out to be total,
   the realistic fallbacks are:
   - run the same script from a home machine on a cron/launchd schedule
     (residential IPs are challenged far less), committing to the repo, or
   - switch the fetch step to a headless browser (Playwright) in the
     Action, which sometimes passes but adds weight and is not guaranteed.
2. **Page format drift.** The parser reads Downdetector's embedded chart
   payload (`ChartDataPointType` / `LastIndicatorType` records). If
   Downdetector changes its page format, polls become `parse_error` rows
   (visible immediately in the data and run logs) rather than wrong numbers.
3. **No backfill before turn-on.** Downdetector history before the first
   run is not available at this price point; each run only sees the
   trailing 24h.
4. **GitHub scheduled crons can run late** (this repo has seen that). The
   24h per-page window plus bucket dedupe makes late runs harmless.

## Rollback

Everything is one append-only CSV plus two new files. To undo a bad run,
revert its commit. To remove the collector, delete `downdetector_data/`
and the workflow file; the tokenomics pipelines are untouched either way.
