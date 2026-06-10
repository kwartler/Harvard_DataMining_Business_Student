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

DATA_DIR    = Path(__file__).parent
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
    if ":" in model_name:
        return model_name.split(":")[0].strip()
    prefix = model_id.split("/")[0]
    return prefix.replace("-", " ").title()


def pricing_from_model(m: dict):
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
# Strategy 1 — requests API (metadata + pricing)
# ---------------------------------------------------------------------------

def fetch_api_models():
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
                print(f"API: {len(items)} models fetched")
                return items
        print(f"API: HTTP {resp.status_code}")
    except Exception as e:
        print(f"API fetch failed: {e}")
    return None


# ---------------------------------------------------------------------------
# Strategy 2 — Playwright (token usage counts)
# ---------------------------------------------------------------------------

def fetch_playwright_tokens():
    """Returns dict {model_id: token_text_raw}."""
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
            user_agent=BROWSER_UA,
            viewport={"width": 1920, "height": 1080},
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

        # --- A: inspect all intercepted JSON for token usage fields ---
        print(f"  intercepted {len(all_json)} JSON responses")
        TOKEN_CANDIDATE_KEYS = [
            "tokens_7d", "weekly_tokens", "tokens_processed", "tokens_processed_7d",
            "usage_7d", "7d_tokens", "token_count", "volume", "traffic", "popularity",
            "token_volume", "weekly_volume", "tokens",
        ]
        for url, body in all_json.items():
            items = body.get("data", body) if isinstance(body, dict) else body
            if not isinstance(items, list) or not items:
                continue
            if not isinstance(items[0], dict) or "id" not in items[0]:
                continue
            fields = list(items[0].keys())
            print(f"    {url}: {len(items)} items, fields={fields[:12]}")
            for key in TOKEN_CANDIDATE_KEYS:
                if any(key in m for m in items[:5]):
                    result = {
                        m["id"]: str(m.get(key, ""))
                        for m in items if m.get("id")
                    }
                    found = sum(1 for v in result.values() if v and v != "0")
                    print(f"  SUCCESS: key='{key}', {found} non-zero values")
                    browser.close()
                    return result

        # --- B: text-node DOM walk looking for "XB tokens" pattern ---
        print("  no token field found in API — trying DOM text-node walk...")
        dom_result = page.evaluate(r"""() => {
            const out = {};
            // Match "212B tokens", "1.5T tokens", "500M tok", etc.
            const TOK_RE = /([\d,.]+)\s*([KMBT])\s*(tok|token)/i;

            const walker = document.createTreeWalker(
                document.body,
                NodeFilter.SHOW_TEXT,
                null, false
            );
            while (walker.nextNode()) {
                const raw = (walker.currentNode.textContent || '').trim();
                if (!raw) continue;
                const m = raw.match(TOK_RE);
                if (!m) continue;

                // Walk up to find the nearest ancestor that contains a model link
                let el = walker.currentNode.parentElement;
                for (let depth = 0; depth < 15 && el; depth++) {
                    const link = el.querySelector('a[href*="/models/"]');
                    if (link) {
                        const href = link.getAttribute('href') || '';
                        const mid = (href.split('/models/')[1] || '').split('?')[0];
                        if (mid && mid.includes('/') && !out[mid]) {
                            out[mid] = m[0].trim();
                        }
                        break;
                    }
                    el = el.parentElement;
                }
            }
            return out;
        }""")

        dom_count = sum(1 for v in dom_result.values() if v)
        print(f"  DOM text-node walk: {dom_count} models with token counts")
        if dom_count > 0:
            sample = dict(list(dom_result.items())[:3])
            print(f"  sample: {sample}")
            browser.close()
            return dom_result

        # --- C: attribute search (title / aria-label containing "token") ---
        print("  trying attribute search (title/aria-label)...")
        attr_result = page.evaluate(r"""() => {
            const out = {};
            const NUM_RE = /([\d,.]+)\s*([KMBT])/i;
            // Find elements whose title or aria-label mentions "token"
            const els = document.querySelectorAll('[title*="token"],[title*="Token"],[aria-label*="token"],[aria-label*="Token"]');
            for (const el of els) {
                const attrVal = el.getAttribute('title') || el.getAttribute('aria-label') || '';
                const numMatch = attrVal.match(NUM_RE) || (el.innerText || '').match(NUM_RE);
                if (!numMatch) continue;
                // Find nearest model link
                const link = el.closest('a[href*="/models/"]') ||
                             el.querySelector('a[href*="/models/"]');
                let ancestor = el;
                let modelLink = null;
                for (let i = 0; i < 10 && ancestor; i++) {
                    const l = ancestor.querySelector('a[href*="/models/"]');
                    if (l) { modelLink = l; break; }
                    ancestor = ancestor.parentElement;
                }
                if (!modelLink) continue;
                const href = modelLink.getAttribute('href') || '';
                const mid = (href.split('/models/')[1] || '').split('?')[0];
                if (mid && mid.includes('/') && !out[mid]) {
                    out[mid] = numMatch[0].trim();
                }
            }
            return out;
        }""")

        attr_count = sum(1 for v in attr_result.values() if v)
        print(f"  attribute search: {attr_count} models with token counts")

        # --- D: diagnostic — sample page text to see what's actually there ---
        print("  === PAGE DIAGNOSTIC (first 20 lines containing numbers) ===")
        sample_lines = page.evaluate(r"""() => {
            const lines = (document.body.innerText || '').split('\n');
            return lines
                .map(l => l.trim())
                .filter(l => l.length > 3 && /\d/.test(l))
                .slice(0, 20);
        }""")
        for line in sample_lines:
            print(f"    {line}")

        # Also check if the token text format uses different words
        tok_lines = page.evaluate(r"""() => {
            const lines = (document.body.innerText || '').split('\n');
            return lines
                .map(l => l.trim())
                .filter(l => /\d.*[KMBT].*tok/i.test(l) || /tok.*\d.*[KMBT]/i.test(l))
                .slice(0, 10);
        }""")
        print(f"  lines matching 'NUMBERx tok*': {tok_lines}")

        browser.close()
        return attr_result or {}


# ---------------------------------------------------------------------------
# Registry upsert
# ---------------------------------------------------------------------------

def update_registry(api_models: list, today: str):
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
        ctx = int(m.get("context_length", 0) or 0)
        ctx_display = format_context(ctx)

        if mid not in existing:
            existing[mid] = {
                "model_id": mid, "model_name": name, "provider": provider,
                "context_length": ctx, "context_display": ctx_display,
                "first_seen": today, "last_seen": today,
            }
            changed = True
        else:
            row = existing[mid]
            updates = {"model_name": name, "provider": provider,
                       "context_length": ctx, "context_display": ctx_display,
                       "last_seen": today}
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
        new_today = sum(1 for r in rows if r["first_seen"] == today)
        print(f"Registry: {len(rows)} models, {new_today} new today")
    else:
        print(f"Registry: no changes ({len(existing)} models)")


# ---------------------------------------------------------------------------
# Build + append daily token records
# ---------------------------------------------------------------------------

def build_and_append(api_models: list, token_lookup: dict, today: str):
    tok_coverage = sum(1 for v in token_lookup.values() if v)
    source = "api+playwright" if tok_coverage > 0 else "api_only"

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

    needs_header = not TOKENS_CSV.exists() or TOKENS_CSV.stat().st_size == 0
    with open(TOKENS_CSV, "a", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=TOKENS_FIELDS)
        if needs_header:
            writer.writeheader()
        writer.writerows(records)

    with_price = sum(1 for r in records if r["input_price_per_1m"])
    print(f"Tokens CSV: {len(records)} rows, {tok_coverage} with token counts, "
          f"{with_price} with pricing ({source})")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    today = date.today().isoformat()

    if should_stop(today):
        sys.exit(0)

    days_so_far = len(existing_dates())
    print(f"Collection day {days_so_far + 1} — {today}")

    api_models = fetch_api_models()
    if not api_models:
        print("ERROR: Could not fetch model list", file=sys.stderr)
        sys.exit(1)

    token_lookup = fetch_playwright_tokens()

    update_registry(api_models, today)
    build_and_append(api_models, token_lookup, today)

    print(f"Done. Total days collected: {days_so_far + 1}.")


if __name__ == "__main__":
    main()
