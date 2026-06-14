# Tokenomics Data Collection — Session Handoff Notes
**Date:** 2026-06-14  
**Branch:** `claude/upbeat-edison-rkh582`

---

## What This Project Does

Daily GitHub Actions job that collects OpenRouter model data and appends it to two CSVs in `tokenomics_data/`:

- **`model_tokens.csv`** — daily append, one row per model per day  
  Fields: `date, model_id, model_name, tokens_7d_raw, tokens_7d, tokens_1d, input_price_per_1m, output_price_per_1m, source`

- **`model_registry.csv`** — upserted crosswalk of model metadata  
  Fields: `model_id, model_name, provider, context_length, context_display, permaslug, first_seen, last_seen`

Workflow file: `.github/workflows/tokenomics_daily.yml` — runs 08:00 UTC daily, `workflow_dispatch` for manual runs.

---

## Current Problem

**`tokens_7d` is populated for only ~11 of 337 models** (from text-proximity DOM scraping).  
**`tokens_1d` is entirely NA** (0 models).

### Root Cause

The `router-activity` endpoint (`/api/frontend/v1/stats/router-activity?permaslug=...`) requires authentication — all 755 same-origin fetch calls return HTTP errors. This is the intended source for both `tokens_7d` and `tokens_1d`.

### Current Workaround (in local branch — NOT yet on master)

The local branch (`claude/upbeat-edison-rkh582`) has a new approach:  
**Read 7d token counts directly from the intercepted `catalog/models` and `models/find?fmt=cards` JSON** that the page fetches on load (no auth needed, already captured by Playwright's response interceptor).

These JSON responses contain model card objects with `slug`, `permaslug`, and (likely) token stats fields. A heuristic (`_largest_token_like`) extracts the biggest numeric value > 1M from each model object, excluding context/price/cost/etc keys.

---

## What's in the Local Branch vs Master

### Local branch `claude/upbeat-edison-rkh582` (7 commits ahead of origin):
```
4eb9f42 feat: read 7d tokens from intercepted cards JSON; diagnose router-activity
f3d87ea fix: recursively parse permaslugs from real intercepted endpoints
a5cd6cf chore: gitignore python bytecode and tmp_* files
ab557e5 fix: self-heal stale model_tokens.csv header (tokens_1d migration)
fbb9bfe fix: escape-tolerant permaslug discovery + frontend probe
```

### What the local `fetch_tokens.py` adds (vs what's on master):

1. **`_NON_TOKEN_KEYS`** — tuple of key names to exclude from heuristic  
2. **`_largest_token_like(model)`** — recursive largest number heuristic, ignores non-token keys, requires > 1M  
3. **`extract_token_stats_from_json(all_json)`** — walks intercepted JSON for model objects (slug+permaslug), dumps shape once for debugging, extracts 7d via heuristic  
4. **Modified `batch_fetch_router_activity`** — captures `_body: (await res.text()).slice(0, 160)` on errors  
5. **Modified `fetch_token_counts`** — new flow:
   - Step 1: extract permaslugs (same as before)
   - Step 2: call `extract_token_stats_from_json(all_json)` → `cards_tokens` (primary 7d source)
   - Step 3: call router-activity for `tokens_1d` (with error diagnostics: logs `error codes={}` and `sample error body`)
   - Merge: `seven_day = dict(cards_tokens)`, supplement with any router-activity 7d data
   - Return if `seven_day` or `daily_counts` non-empty (source: `"cards_json"`)
   - Fall back to text-proximity only if nothing else works

---

## What Happened in Today's Run (2026-06-14, job 81265143440)

The run used the **OLD master code** (the local changes were never pushed). Key log lines:
```
API: 337 models
intercepted 8 JSON responses
  catalog/models  permaslug=True
  models/find?fmt=cards...  permaslug=True
permaslugs from intercepted JSON: 755 found
router-activity: 0/755 models returned data, 741 errors
router-activity returned 0 counts — trying fallback
text-proximity fallback: 11 matched
Tokens CSV: 337 rows | 11 with 7d | 0 with 1d | 311 with pricing | source=text_proximity
```

The `extract_token_stats_from_json` function was **not in the deployed code** — that's why there are no "cards JSON: extracted token counts for N models" log lines.

---

## Immediate Next Step

**Merge the local branch to master so the next run picks up the cards-JSON extraction.**

```bash
# On your local machine:
git fetch origin
git checkout master
git merge claude/upbeat-edison-rkh582
git push origin master
```

Or create a PR from `claude/upbeat-edison-rkh582` → `master` and merge it.

---

## After Merging — What to Look For in the Next Run

The next run's log should show:

1. `cards JSON: NNN model objects; sample slug=...` — if NNN ~ 337, cards JSON found the models  
2. `sample top-level keys: [...]` — the actual fields in the model card JSON  
3. `sample token-ish fields: [...]` — fields with values > 100K (helps pin the exact token field name)  
4. `cards JSON: extracted token counts for NNN models (heuristic)` — if NNN ~ 337, 7d is working  
5. `router-activity error codes={NNN: count}` — HTTP status of errors (e.g., 401, 403)  
6. `router-activity sample error body: '...'` — first 160 chars of error response (may show auth redirect)

If the heuristic works: `Tokens CSV: 337 rows | ~337 with 7d | 0 with 1d`  
If `tokens_1d` is still 0: that's expected — router-activity is auth-gated.

---

## After the Next Run — Likely Follow-Ups

### If cards JSON extraction works (tokens_7d populated):
The shape dump will show the exact field name. Replace the heuristic with a direct field lookup:
```python
# Example: if the field is named "num_tokens_7d"
t = m.get("num_tokens_7d")
```

### If tokens_1d is still all NA:
The router-activity error body will show what kind of auth is needed. Options:
- If it requires a user JWT: tokens_1d may be permanently inaccessible for anonymous scraping
- Accept `tokens_1d` as NA and document this limitation

### Dedup issue (if re-running after a failed run):
```bash
# Remove today's rows if a partial run left bad data:
grep -v "^2026-06-14," tokenomics_data/model_tokens.csv > /tmp/clean.csv && mv /tmp/clean.csv tokenomics_data/model_tokens.csv
```

---

## Node.js Deprecation Warning (deadline: June 16, 2026)

The workflow uses `actions/checkout@v4` and `actions/setup-python@v5` which run on Node.js 20. GitHub will force Node.js 24 on June 16. The warning in the logs:
```
Node.js 20 actions are deprecated... forced to run with Node.js 24 starting June 16th
```

**Fix** — update `.github/workflows/tokenomics_daily.yml`:
```yaml
- uses: actions/checkout@v4          # stays at v4, it already has node24 support
- uses: actions/setup-python@v5      # stays at v5, ditto
```
Actually both `checkout@v4` and `setup-python@v5` already have Node.js 24 support (they just need to update the runner). You may need to add:
```yaml
env:
  FORCE_JAVASCRIPT_ACTIONS_TO_NODE24: true
```
to the workflow to opt in early, avoiding the forced cutover surprise.

---

## File Locations

| File | Purpose |
|------|---------|
| `tokenomics_data/fetch_tokens.py` | Main collection script |
| `tokenomics_data/model_tokens.csv` | Daily token/price data (append-only) |
| `tokenomics_data/model_registry.csv` | Model metadata crosswalk (upserted) |
| `.github/workflows/tokenomics_daily.yml` | GitHub Actions workflow |
| `tokenomics_data/STOP` | Create this file to halt collection |
| `.gitignore` | Includes `__pycache__/`, `*.pyc`, `tokenomics_data/tmp_*`, `tmp_*` |

---

## Key OpenRouter Endpoints

| Endpoint | Auth needed | What it returns |
|----------|-------------|-----------------|
| `GET /api/v1/models` | No | Model list + pricing (no token stats) |
| `GET /api/frontend/v1/catalog/models` | No (same-origin only) | Model cards with permaslug, likely token stats |
| `GET /api/frontend/v1/models/find?active=true&fmt=cards&input_modalities=text` | No (same-origin) | Same, filtered to text models |
| `GET /api/frontend/v1/stats/router-activity?permaslug=...` | **Yes** (user JWT) | Per-day token analytics — auth-gated |

"Same-origin only" means: works when fetched from inside the `openrouter.ai` page context via Playwright's `page.evaluate()`. Direct HTTP requests return 401/404.

---

## Architecture Notes

- **Permaslug** — OpenRouter's internal dated canonical ID (e.g., `anthropic/claude-4.5-sonnet-20250929`). Distinct from URL slug. Required for router-activity calls. Found in model card JSON objects alongside `slug` and `id`.
- **`_permaslug_cache`** — module-level dict populated by `fetch_token_counts`, read by `update_registry` to save permaslugs to the registry CSV.
- **`migrate_tokens_csv()`** — runs first in `main()`, self-heals 8-column→9-column header mismatch by inserting empty `tokens_1d` in old rows.
- **Dedup guard** — `should_stop()` checks if today's date is already in the CSV before collecting.
- **STOP mechanism** — create `tokenomics_data/STOP` to halt; the script exits without collecting.

---

## How to Continue With Claude Code Locally

If continuing with Claude Code on your local machine:

1. Clone/pull the repo locally
2. Check out branch `claude/upbeat-edison-rkh582` 
3. The full conversation history is at `/root/.claude/projects/-home-user-Harvard-DataMining-Business-Student/32d114dd-5824-55ab-b05c-45d32ee122cf.jsonl` (on the remote machine, not accessible locally)
4. Brief Claude on this session using this file as context
5. After the next run completes, pull the run logs via `gh run view --log` or GitHub MCP tools
