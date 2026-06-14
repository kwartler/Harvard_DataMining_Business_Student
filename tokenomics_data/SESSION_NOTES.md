# Tokenomics Data Collection: Session Notes

**Last updated:** 2026-06-14

## Purpose

Daily collection of OpenRouter model usage to support teaching material on new
technology diffusion and time-series analysis. Three things are captured:

1. **All-model daily activity** (input/output tokens, request counts)
2. **Daily input and output costs** in USD (tokens x published pricing)
3. **A model manifest** (registry) of every model, its pricing, context window,
   and when it was first/last seen.

## How it works

`fetch_tokens.py` makes three plain anonymous HTTPS GETs (no headless browser):

| Endpoint | Gives |
|----------|-------|
| `/api/frontend/rankings/models?view=day` | platform-wide per-model tokens for the most recent COMPLETE day |
| `/api/v1/models` | per-token pricing + names |
| `/api/frontend/v1/catalog/models` | permaslug <-> slug, author, context |

Costs are computed by joining the daily tokens to pricing on the model slug.

The daily ranking reports the latest *complete* day, so a run on day N records
day N-1. Dedup is keyed on the activity date inside the data, not the run date,
so re-running the same day is a no-op.

## Output files

| File | Schema |
|------|--------|
| `model_tokens.csv` | `date, model_permaslug, model_id, model_name, provider, requests, input_tokens, output_tokens, total_tokens, input_price_per_1m, output_price_per_1m, input_cost_usd, output_cost_usd, total_cost_usd, source` |
| `model_registry.csv` | `model_id, model_name, provider, permaslug, context_length, context_display, input_price_per_1m, output_price_per_1m, first_seen, last_seen` |
| `model_tokens_legacy.csv`, `model_registry_legacy.csv` | Archived pre-2026-06-14 data from the old (unreliable) collection method. |

Typical day: ~393 models, ~292 with pricing (unpriced are free / preview /
dated-variant models not listed in `api/v1/models`).

## Workflow

`.github/workflows/tokenomics_daily.yml` runs 08:00 UTC daily (plus
`workflow_dispatch`). It installs only `requests`, runs the script, and commits
any new rows. Runtime is ~1 minute.

## History: why the old approach was replaced (2026-06-14)

The previous collector called `/api/frontend/v1/stats/router-activity?permaslug=`
once per model to get daily (`tokens_1d`) and 7-day (`tokens_7d`) counts. It
never produced daily data. Investigation found:

- That endpoint returns **404 "Router model not found"** for every individual
  model, anonymously *and* via same-origin browser fetch. The word "Router" is
  literal: it only resolves actual router models. `permaslug=openrouter/auto`
  returns 200; base models never do. It was the wrong endpoint, not an auth or
  parameter bug.
- A later cards-JSON heuristic populated `tokens_7d` with whatever large number
  it found, yielding bogus round values (e.g. several models at exactly
  10,000,000). Those values are in `model_tokens_legacy.csv` and should not be
  trusted.

The `rankings/models` endpoint is the data behind OpenRouter's own rankings
charts and gives clean platform-wide totals, so it replaced both.

## Knobs

- Stop collection: create `tokenomics_data/STOP`.
- 7-day or 30-day windows are available from the same endpoint via
  `?view=week` / `?view=month` if a rolling total is ever wanted (the daily
  series already supports computing any rolling window downstream).
