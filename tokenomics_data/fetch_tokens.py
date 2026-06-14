#!/usr/bin/env python3
"""
Daily OpenRouter model activity + cost collector.

Two output files (both in this directory):
  model_registry.csv  Manifest. One row per model (upserted) with provider,
                      context window, permaslug, current pricing, and the dates
                      the model was first/last seen.
  model_tokens.csv    Daily time series. One row per model per day with input,
                      output, and reasoning token counts, request count, and the
                      computed input / output / total cost in USD.

Data sources (all plain anonymous HTTPS GETs, no headless browser needed):
  /api/v1/models                          model list + per-token pricing
  /api/frontend/v1/catalog/models         permaslug <-> slug, author, context
  /api/frontend/rankings/models?view=day  platform-wide per-model daily tokens

The daily ranking reports the most recent COMPLETE day, so a run on day N
records the activity for day N-1. Deduplication is keyed on that activity date
(the date inside the data), not on the day the script happens to run.

Stop collection by disabling the workflow or committing tokenomics_data/STOP.
"""

import csv
import sys
from datetime import date
from pathlib import Path

DATA_DIR     = Path(__file__).parent
TOKENS_CSV   = DATA_DIR / "model_tokens.csv"
REGISTRY_CSV = DATA_DIR / "model_registry.csv"
STOP_FILE    = DATA_DIR / "STOP"

BROWSER_UA = (
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
)

API_MODELS_URL     = "https://openrouter.ai/api/v1/models"
CATALOG_URL        = "https://openrouter.ai/api/frontend/v1/catalog/models"
RANKINGS_DAY_URL   = "https://openrouter.ai/api/frontend/rankings/models?view=day"

TOKENS_FIELDS = [
    "date", "model_permaslug", "model_id", "model_name", "provider",
    "requests", "input_tokens", "output_tokens", "total_tokens",
    "input_price_per_1m", "output_price_per_1m",
    "input_cost_usd", "output_cost_usd", "total_cost_usd",
    "source",
]
REGISTRY_FIELDS = [
    "model_id", "model_name", "provider", "permaslug",
    "context_length", "context_display",
    "input_price_per_1m", "output_price_per_1m",
    "first_seen", "last_seen",
]

SOURCE = "rankings_day"


# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------

def existing_dates() -> set:
    if not TOKENS_CSV.exists():
        return set()
    with open(TOKENS_CSV, newline="") as f:
        return {row["date"] for row in csv.DictReader(f) if row.get("date")}


def format_context(n: int) -> str:
    if not n:
        return ""
    if n >= 1_000_000:
        v = n / 1_000_000
        return f"{int(v)}M" if v == int(v) else f"{v:.1f}M"
    if n >= 1_000:
        v = n / 1_000
        return f"{int(v)}K" if v == int(v) else f"{v:.1f}K"
    return str(n)


def provider_from(model_id: str, model_name: str) -> str:
    if model_name and ":" in model_name:
        return model_name.split(":")[0].strip()
    base = (model_id or "").split("/")[0]
    return base.replace("-", " ").title() if base else ""


def get_json(url: str):
    import requests
    r = requests.get(
        url,
        headers={"User-Agent": BROWSER_UA, "Accept": "application/json"},
        timeout=30,
    )
    r.raise_for_status()
    return r.json()


# ---------------------------------------------------------------------------
# Data sources
# ---------------------------------------------------------------------------

def fetch_pricing() -> dict:
    """slug -> {prompt, completion, name, context} from the public models API.

    prompt/completion are per-token USD floats (as published by OpenRouter).
    """
    out = {}
    try:
        items = get_json(API_MODELS_URL).get("data", [])
    except Exception as e:
        print(f"  api/v1/models error: {e}")
        return out
    for m in items:
        mid = m.get("id")
        if not mid:
            continue
        p = m.get("pricing", {}) or {}
        try:
            prompt = float(p.get("prompt", 0) or 0)
        except (TypeError, ValueError):
            prompt = 0.0
        try:
            completion = float(p.get("completion", 0) or 0)
        except (TypeError, ValueError):
            completion = 0.0
        out[mid] = {
            "prompt": prompt,
            "completion": completion,
            "name": m.get("name", mid),
            "context": int(m.get("context_length", 0) or 0),
        }
    print(f"  pricing: {len(out)} models from api/v1/models")
    return out


def fetch_catalog() -> dict:
    """permaslug -> {slug, name, author, context} from the frontend catalog.

    Walks the response tree because model objects are nested.
    """
    out = {}

    def walk(o):
        if isinstance(o, dict):
            ps = o.get("permaslug")
            slug = o.get("slug")
            if isinstance(ps, str) and ps and isinstance(slug, str) and slug:
                out.setdefault(ps, {
                    "slug": slug,
                    "name": o.get("name") or o.get("short_name") or slug,
                    "author": o.get("author") or o.get("author_display_name") or "",
                    "context": int(o.get("context_length", 0) or 0),
                })
            for v in o.values():
                walk(v)
        elif isinstance(o, list):
            for v in o:
                walk(v)

    try:
        walk(get_json(CATALOG_URL))
    except Exception as e:
        print(f"  catalog error: {e}")
    print(f"  catalog: {len(out)} permaslug->slug pairs")
    return out


def fetch_daily_activity() -> dict:
    """Aggregate the daily ranking into {date: {permaslug: totals}}.

    Sums across model variants (e.g. free + standard) per model per day.
    """
    try:
        rows = get_json(RANKINGS_DAY_URL).get("data", [])
    except Exception as e:
        print(f"  rankings error: {e}")
        return {}

    by_date: dict = {}
    for r in rows:
        d = (r.get("date") or "")[:10]
        ps = r.get("model_permaslug")
        if not d or not ps:
            continue
        agg = by_date.setdefault(d, {}).setdefault(ps, {
            "requests": 0, "input": 0, "output": 0,
        })
        agg["requests"] += int(r.get("count", 0) or 0)
        agg["input"]    += int(r.get("total_prompt_tokens", 0) or 0)
        agg["output"]   += int(r.get("total_completion_tokens", 0) or 0)

    dates = sorted(by_date)
    n = sum(len(v) for v in by_date.values())
    print(f"  rankings: {n} model-rows across dates {dates}")
    return by_date


# ---------------------------------------------------------------------------
# Manifest (registry) upsert
# ---------------------------------------------------------------------------

def update_registry(pricing: dict, catalog: dict, run_date: str):
    slug_to_permaslug = {v["slug"]: ps for ps, v in catalog.items()}

    existing: dict = {}
    if REGISTRY_CSV.exists():
        with open(REGISTRY_CSV, newline="") as f:
            for row in csv.DictReader(f):
                existing[row["model_id"]] = row

    changed = False
    for slug, pr in pricing.items():
        ctx = pr["context"]
        name = pr["name"]
        permaslug = slug_to_permaslug.get(slug, "")
        fields = {
            "model_name": name,
            "provider": provider_from(slug, name),
            "permaslug": permaslug,
            "context_length": ctx,
            "context_display": format_context(ctx),
            "input_price_per_1m": round(pr["prompt"] * 1_000_000, 6),
            "output_price_per_1m": round(pr["completion"] * 1_000_000, 6),
            "last_seen": run_date,
        }
        if slug not in existing:
            row = {"model_id": slug, "first_seen": run_date}
            row.update(fields)
            existing[slug] = row
            changed = True
        else:
            row = existing[slug]
            for k, v in fields.items():
                if str(row.get(k, "")) != str(v):
                    row[k] = v
                    changed = True

    if changed:
        rows = sorted(existing.values(), key=lambda r: r["model_id"])
        with open(REGISTRY_CSV, "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=REGISTRY_FIELDS)
            w.writeheader()
            w.writerows(rows)
        new_today = sum(1 for r in rows if r.get("first_seen") == run_date)
        print(f"Registry: {len(rows)} models, {new_today} new today")
    else:
        print(f"Registry: no changes ({len(existing)} models)")


# ---------------------------------------------------------------------------
# Daily time-series append
# ---------------------------------------------------------------------------

def build_and_append(by_date: dict, pricing: dict, catalog: dict, known: set):
    new_dates = sorted(d for d in by_date if d not in known)
    if not new_dates:
        print("No new activity dates to record.")
        return 0

    records = []
    for d in new_dates:
        for permaslug, agg in by_date[d].items():
            meta = catalog.get(permaslug, {})
            slug = meta.get("slug", "")
            pr = pricing.get(slug)
            name = (pr or {}).get("name") or meta.get("name") or permaslug
            provider = meta.get("author") or provider_from(slug or permaslug, name)

            inp, out = agg["input"], agg["output"]
            if pr:
                in_price = round(pr["prompt"] * 1_000_000, 6)
                out_price = round(pr["completion"] * 1_000_000, 6)
                in_cost = round(inp * pr["prompt"], 2)
                out_cost = round(out * pr["completion"], 2)
                total_cost = round(in_cost + out_cost, 2)
            else:
                in_price = out_price = ""
                in_cost = out_cost = total_cost = ""

            records.append({
                "date": d,
                "model_permaslug": permaslug,
                "model_id": slug,
                "model_name": name,
                "provider": provider,
                "requests": agg["requests"],
                "input_tokens": inp,
                "output_tokens": out,
                "total_tokens": inp + out,
                "input_price_per_1m": in_price,
                "output_price_per_1m": out_price,
                "input_cost_usd": in_cost,
                "output_cost_usd": out_cost,
                "total_cost_usd": total_cost,
                "source": SOURCE,
            })

    needs_header = not TOKENS_CSV.exists() or TOKENS_CSV.stat().st_size == 0
    with open(TOKENS_CSV, "a", newline="") as f:
        w = csv.DictWriter(f, fieldnames=TOKENS_FIELDS)
        if needs_header:
            w.writeheader()
        w.writerows(records)

    priced = sum(1 for r in records if r["total_cost_usd"] != "")
    day_cost = sum(r["total_cost_usd"] for r in records
                   if isinstance(r["total_cost_usd"], (int, float)))
    print(f"Tokens CSV: +{len(records)} rows for {new_dates} "
          f"| {priced} priced | est. total spend ${day_cost:,.0f}")
    return len(records)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    run_date = date.today().isoformat()

    if STOP_FILE.exists():
        print("STOP file found — halted.")
        sys.exit(0)

    print(f"OpenRouter tokenomics collector — run {run_date}")
    by_date = fetch_daily_activity()
    if not by_date:
        print("ERROR: no daily activity data returned", file=sys.stderr)
        sys.exit(1)

    known = existing_dates()
    if all(d in known for d in by_date):
        print(f"Activity for {sorted(by_date)} already collected — skipping.")
        sys.exit(0)

    pricing = fetch_pricing()
    catalog = fetch_catalog()

    update_registry(pricing, catalog, run_date)
    build_and_append(by_date, pricing, catalog, known)
    print("Done.")


if __name__ == "__main__":
    main()
