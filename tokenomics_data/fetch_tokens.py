#!/usr/bin/env python3
"""
Daily OpenRouter model token usage + pricing collector.

Two output files:
  model_registry.csv  — one row per model (upserted): provider, context, first/last seen
  model_tokens.csv    — daily append: token usage + input/output prices

Runs indefinitely. To stop: disable the GitHub Actions workflow, or commit
a file named STOP inside tokenomics_data/.
"""

import csv
import re
import sys
from datetime import date
from pathlib import Path

DATA_DIR = Path(__file__).parent
TOKENS_CSV  = DATA_DIR / "model_tokens.csv"
REGISTRY_CSV = DATA_DIR / "model_registry.csv"
STOP_FILE   = DATA_DIR / "STOP"
URL = "https://openrouter.ai/models?input_modalities=text"

TOKENS_FIELDS = [
    "date", "model_id", "model_name",
    "tokens_7d_raw", "tokens_7d",
    "input_price_per_1m", "output_price_per_1m",
    "source",
]
REGISTRY_FIELDS = [
    "model_id", "model_name", "provider",
    "context_length", "context_display",
    "first_seen", "last_seen",
]

BROWSER_UA = (
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def existing_dates():
    if not TOKENS_CSV.exists():
        return set()
    with open(TOKENS_CSV, newline="") as f:
        return {row["date"] for row in csv.DictReader(f) if row.get("date")}


def should_stop(today: str) -> bool:
    if STOP_FILE.exists():
        print("STOP file found — collection halted.")
        return True
    if today in existing_dates():
        print(f"Data for {today} already collected — skipping.")
        return True
    return False


def parse_tokens(text: str):
    """'212B' / '1.5T' / '500M' / '300K' → integer, else None."""
    if not text:
        return None
    m = re.search(r"([\d,.]+)\s*([KMBT])", str(text).upper())
    if not m:
        try:
            v = int(str(text).replace(",", "").strip())
            return v if v > 1_000_000 else None
        except ValueError:
            return None
    try:
        val = float(m.group(1).replace(",", ""))
    except ValueError:
        return None
    return int(val * {"K": 1e3, "M": 1e6, "B": 1e9, "T": 1e12}[m.group(2)])


def format_context(n: int) -> str:
    """200000 → '200K', 1000000 → '1M'."""
    if not n:
        return ""
    if n >= 1_000_000:
        v = n / 1_000_000
        return f"{int(v)}M" if v == int(v) else f"{v:.1f}M"
    if n >= 1_000:
        v = n / 1_000
        return f"{int(v)}K" if v == int(v) else f"{v:.1f}K"
    return str(n)


def provider_from_model(model_id: str, model_name: str) -> str:
    """Extract provider label, e.g. 'anthropic/claude-3' → 'Anthropic'."""
    # Prefer the part before ':' in the display name ("Anthropic: Claude 3 Haiku")
    if ":" in model_name:
        return model_name.split(":")[0].strip()
    # Fall back to the id prefix ("anthropic/...")
    prefix = model_id.split("/")[0]
    return prefix.replace("-", " ").title()


def pricing_from_model(m: dict):
    """Return (input_per_1m, output_per_1m) as floats, both in USD."""
    pricing = m.get("pricing", {}) or {}
    try:
        inp = round(float(pricing.get("prompt", 0) or 0) * 1_000_000, 6)
    except (ValueError, TypeError):
        inp = 0.0
    try:
        out = round(float(pricing.get("completion", 0) or 0) * 1_000_000, 6)
    except (ValueError, TypeError):
        out = 0.0
    return inp, out


# ---------------------------------------------------------------------------
# Strategy 1 — requests API (metadata + pricing; fast and reliable)
# ---------------------------------------------------------------------------

def fetch_api_models():
    """Returns list of raw model dicts from /api/v1/models, or None."""
    try:
        import requests
    except ImportError:
        return None

    try:
        resp = requests.get(
            "https://openrouter.ai/api/v1/models",
            headers={"User-Agent": BROWSER_UA, "Accept": "application/json"},
            timeout=30,
        )
        if resp.status_code == 200:
            items = resp.json().get("data", [])
            if items and "id" in items[0]:
                print(f"API: {len(items)} models")
                return items
    except Exception as e:
        print(f"API fetch failed: {e}")
    return None


# ---------------------------------------------------------------------------
# Strategy 2 — Playwright (token usage counts from page)
# ---------------------------------------------------------------------------

def fetch_playwright_tokens():
    """
    Returns dict {model_id: token_text} scraped from the rendered page.
    Uses network interception first, DOM scraping as fallback.
    """
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        print("playwright not installed")
        return {}

    all_json = {}

    def on_response(response):
        try:
            if "openrouter.ai" not in response.url or response.status != 200:
                return
            if "json" not in response.headers.get("content-type", ""):
                return
            all_json[response.url] = response.json()
        except Exception:
            pass

    with sync_playwright() as pw:
        browser = pw.chromium.launch(headless=True)
        page = browser.new_context(
            user_agent=BROWSER_UA, viewport={"width": 1920, "height": 1080}
        ).new_page()
        page.on("response", on_response)

        print(f"Playwright: loading {URL}")
        try:
            page.goto(URL, wait_until="networkidle", timeout=90_000)
        except Exception as e:
            print(f"  nav warning: {e}")
        page.wait_for_timeout(6_000)

        # Scroll to trigger lazy-loaded rows
        prev_h = 0
        for _ in range(25):
            page.keyboard.press("End")
            page.wait_for_timeout(1_200)
            h = page.evaluate("document.body.scrollHeight")
            if h == prev_h:
                break
            prev_h = h

        # A — intercepted API response that already has token data
        for url, body in all_json.items():
            items = body.get("data", body) if isinstance(body, dict) else body
            if not isinstance(items, list) or not items:
                continue
            if not isinstance(items[0], dict) or "id" not in items[0]:
                continue
            # Look for a numeric token field
            sample = items[:5]
            for candidate_key in ["tokens_7d", "weekly_tokens", "tokens_processed",
                                   "usage_7d", "volume", "traffic", "popularity"]:
                if any(candidate_key in m for m in sample):
                    result = {
                        m["id"]: str(m.get(candidate_key, ""))
                        for m in items if m.get("id")
                    }
                    print(f"  token field '{candidate_key}' found in {url}")
                    browser.close()
                    return result

        # B — DOM scrape for visible "212B tokens" text
        print("  scraping token counts from DOM...")
        dom = page.evaluate(r"""() => {
            const out = {};
            const TOK = /([\d,.]+)\s*([KMBT])\s*(tok|token)/i;
            const BARE = /([\d,.]+)\s*([KMBT])\b/i;
            for (const a of document.querySelectorAll('a[href*="/models/"]')) {
                const after = (a.getAttribute('href') || '').split('/models/')[1];
                if (!after) continue;
                const mid = after.split('?')[0];
                if (!mid || !mid.includes('/') || out[mid]) continue;
                let el = a, text = '';
                for (let i = 0; i < 12; i++) {
                    text = (el.innerText || '').replace(/\n/g, ' ');
                    if (TOK.test(text)) { out[mid] = text.match(TOK)[0]; break; }
                    if (!el.parentElement) break;
                    el = el.parentElement;
                }
                // fallback: any large "XB" figure near the link
                if (!out[mid]) {
                    const m = text.match(BARE);
                    if (m) out[mid] = m[0];
                }
            }
            return out;
        }""")
        browser.close()
        return dom or {}


# ---------------------------------------------------------------------------
# Registry upsert
# ---------------------------------------------------------------------------

def update_registry(api_models: list, today: str):
    """Upsert model_registry.csv — add new models, update last_seen."""
    # Load existing
    existing = {}
    if REGISTRY_CSV.exists():
        with open(REGISTRY_CSV, newline="") as f:
            for row in csv.DictReader(f):
                existing[row["model_id"]] = row

    changed = False
    for m in api_models:
        mid = m.get("id", "")
        if not mid:
            continue
        name = m.get("name", mid)
        provider = provider_from_model(mid, name)
        ctx = m.get("context_length", 0) or 0
        ctx_display = format_context(int(ctx))

        if mid not in existing:
            existing[mid] = {
                "model_id": mid,
                "model_name": name,
                "provider": provider,
                "context_length": ctx,
                "context_display": ctx_display,
                "first_seen": today,
                "last_seen": today,
            }
            changed = True
        else:
            row = existing[mid]
            # Update mutable fields
            updates = {
                "model_name": name,
                "provider": provider,
                "context_length": ctx,
                "context_display": ctx_display,
                "last_seen": today,
            }
            for k, v in updates.items():
                if str(row.get(k, "")) != str(v):
                    row[k] = v
                    changed = True

    if changed:
        rows = sorted(existing.values(), key=lambda r: r["model_id"])
        with open(REGISTRY_CSV, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=REGISTRY_FIELDS)
            writer.writeheader()
            writer.writerows(rows)
        new_count = sum(1 for r in rows if r["first_seen"] == today)
        print(f"Registry: {len(rows)} models total, {new_count} new today")
    else:
        print(f"Registry: no changes ({len(existing)} models)")


# ---------------------------------------------------------------------------
# Build daily token records
# ---------------------------------------------------------------------------

def build_token_records(api_models: list, token_lookup: dict, today: str, source: str):
    records = []
    for m in api_models:
        mid = m.get("id", "")
        name = m.get("name", mid)
        tok_raw = token_lookup.get(mid, "")
        inp, out = pricing_from_model(m)
        records.append({
            "date": today,
            "model_id": mid,
            "model_name": name,
            "tokens_7d_raw": tok_raw,
            "tokens_7d": parse_tokens(tok_raw) or "",
            "input_price_per_1m": inp,
            "output_price_per_1m": out,
            "source": source,
        })
    return records


def append_tokens_csv(records: list):
    needs_header = not TOKENS_CSV.exists() or TOKENS_CSV.stat().st_size == 0
    with open(TOKENS_CSV, "a", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=TOKENS_FIELDS)
        if needs_header:
            writer.writeheader()
        writer.writerows(records)
    with_tok = sum(1 for r in records if r["tokens_7d"] != "")
    with_price = sum(1 for r in records if r["input_price_per_1m"])
    print(f"Tokens CSV: {len(records)} rows, {with_tok} with token counts, "
          f"{with_price} with pricing")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    today = date.today().isoformat()

    if should_stop(today):
        sys.exit(0)

    days_so_far = len(existing_dates())
    print(f"Collection day {days_so_far + 1} — {today}")

    # Step 1: get model metadata + pricing from API
    api_models = fetch_api_models()
    if not api_models:
        print("ERROR: Could not fetch model list from API", file=sys.stderr)
        sys.exit(1)

    # Step 2: get token usage counts from page (Playwright)
    token_lookup = fetch_playwright_tokens()
    tok_coverage = sum(1 for v in token_lookup.values() if v)
    print(f"Token lookup: {tok_coverage}/{len(token_lookup)} models have counts")

    # Step 3: determine source label
    source = "api+playwright_tokens" if tok_coverage > 0 else "api_only"

    # Step 4: upsert registry
    update_registry(api_models, today)

    # Step 5: build and append daily records
    records = build_token_records(api_models, token_lookup, today, source)
    if not records:
        print("ERROR: Zero records built", file=sys.stderr)
        sys.exit(1)

    append_tokens_csv(records)
    print(f"Done. Total days collected: {days_so_far + 1}.")


if __name__ == "__main__":
    main()
