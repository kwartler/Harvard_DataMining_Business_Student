<!-- AI assisted with Claude -->
# OpenRouter tokenomics data: read this first

Two datasets live here. They measure different things, so use each for what it is and do not combine them into one series.

| | Rolling 7 day TOTAL dataset | Daily top 50 |
|---|---|---|
| File | `model_tokens.csv` (with `model_registry.csv`) | `daily_top50.csv` (raw JSON in `daily_top50_raw/`) |
| Script | `fetch_tokens.py` | `fetch_daily_top50.py` |
| Coverage | Every model that appears in the rankings feed (about 600 per day) | The 50 largest base models per day |
| Measurement | **Trailing 7 day total** ending on the date shown (rolling window) | **One UTC day** of activity |
| Dates | 2026-06-13 to present, with a gap and a break (below) | 2026-11-09 to present |
| Cost columns | Yes, an estimate (see Pricing) | None, on purpose |

## What the dates mean

* Both files label a row with the activity date inside the source data, not the day the job ran. Jobs run late in the day (GitHub schedules drift), so a row for day N usually appears on day N+1.
* **Rolling 7 day TOTAL dataset:** since 2026-08-22 the collector reads the default rankings feed, which is a trailing 7 day window. A row dated D holds tokens and requests for roughly D-6 through D. Daily changes are smoothed and consecutive rows overlap, so they are autocorrelated by construction. Do not sum the rows across dates (each day would be counted about 7 times), and do not call a row a daily value.
* **Daily top 50:** requests `view=day` explicitly and refuses any response that contains more than one date. Variants (standard, free, batch and so on) of the same model are merged into the base model. The public leaderboard ranks variants separately, so its top 50 can differ.
* **How we know the rolling file is a window:** on the days both files cover, the rolling value for a model is about 6 to 8 times its daily value (median ratio about 6.2 to 8.2 over Nov 9 to 18). New models enter the window part way, so their ratio is lower. The history note is in `DAILY_TOP50_README.md` and commit a148228, which dropped `view=day`.

## Known breaks and gaps in the rolling 7 day TOTAL dataset

* 2026-06-13 to 06-17: collected with `view=day`, so these are daily values from the earlier method. Not fully validated.
* 2026-06-18 to 08-16: no data.
* 2026-08-17 to 08-20: partial rows (1 to 9 models). Treat as a ramp up, not real activity.
* 2026-08-21 onward: the 7 day window series, about 460 to 600 models per day. Daily totals are about 15 times those of mid June because of the window change plus real growth. Do not compare June with August onward without adjusting.
* `model_tokens_legacy.csv` and `model_registry_legacy.csv` hold older data from a collection method that did not work. Do not use them.
* A missing date stays missing. Neither collector backfills or inserts zeros.

## Overlap and how to compare

Dates from 2026-11-09 appear in both files, but the numbers are not interchangeable. To line them up, divide the rolling value by about 7 for a rough daily level, or better, use the daily file as the daily series and the rolling file for breadth (all models). Example in base R:

```r
rolling <- read.csv('tokenomics_data/model_tokens.csv')
daily <- read.csv('tokenomics_data/daily_top50.csv')
both  <- merge(rolling[, c('date', 'model_permaslug', 'total_tokens')],
               daily[, c('date', 'model_permaslug', 'total_tokens')],
               by = c('date', 'model_permaslug'),
               suffixes = c('_7day', '_daily'))
both$ratio <- both$total_tokens_7day / both$total_tokens_daily
summary(both$ratio)
```

## Pricing: read before using any cost number

* `input_price_per_1m` and `output_price_per_1m` are OpenRouter's published list prices (dollars per million tokens) on the day the job ran, taken from the public models API. Each row keeps the price from its own collection day.
* `input_cost_usd`, `output_cost_usd` and `total_cost_usd` are **estimates**: tokens times list price, rounded to cents per row. They are not actual spend or revenue.
* In the rolling file the tokens are a 7 day total, so each cost is about one week of list price spend for that model, not a daily cost. One price (the collection day's) is applied to the whole window, so a price change inside the window is not reflected.
* Rows whose model is not in the public models API have blank prices and costs (free, preview and dated variants). On a typical day about 55 to 75 percent of models are priced, but they cover about 95 to 99 percent of tokens.
* Variants are merged into one row, and the cost uses the base model price. Tokens served on a free or batch variant are therefore priced at the base model's list price, which can overstate cost.
* Cached token discounts, bring your own key traffic, tool or search fees, image, audio and video usage, and negotiated or promotional prices are not reflected.
* The daily top 50 file has no price or cost columns. To estimate its cost, join prices from `model_registry.csv` yourself and state the same caveats.

## What the data represents

* Public OpenRouter traffic only, taken from OpenRouter's own rankings feed. It is not all AI usage, and not all OpenRouter usage if the feed excludes some traffic.
* The feed is an unversioned frontend endpoint. If its format or window changes, the daily collector fails loudly, but the rolling collector may not. Re check the window if the ratio above changes.

## Files

| File | What it is |
|---|---|
| `model_tokens.csv` | Rolling 7 day TOTAL dataset, one row per model per date |
| `model_registry.csv` | One row per model with provider, context size, current prices, first and last seen |
| `daily_top50.csv` | Daily top 50 base models (true UTC day) |
| `daily_top50_raw/` | Raw source JSON per day, named by date and SHA-256 hash |
| `fetch_tokens.py`, `fetch_daily_top50.py` | The two collectors |
| `DAILY_TOP50_README.md` | Detailed design notes for the daily collector |
| `SESSION_NOTES.md` | Older notes. Its description of the rolling collector as daily is out of date |
| `analyze_tokenomics.R`, `wip_time_series.R` | Analysis scripts, work in progress |

## Starting points

You are welcome to explore beyond these questions. Answer them, or your own, using R code. State which dataset you used and why. Read "What the dates mean" and "Pricing" first.

**Rolling 7 day TOTAL dataset**
1. Which models earned the most in the last 7 days?
2. How long does a popular model stay popular? Decide what "popular" and "longevity" mean, then measure them.
3. Which providers gained or lost share of tokens over time, and when did the change happen?

**Daily top 50 dataset**
4. Which models are rising and falling in the daily ranking? Is the movement a real shift in use, or something else, such as a new release?
5. Which models are verbose (lots of output) and which are read heavy (lots of input)? Can you speculate as to the difference?

**Both datasets**
6. What does the 7 day window hide? Compare the two for the same model on the dates they share.
