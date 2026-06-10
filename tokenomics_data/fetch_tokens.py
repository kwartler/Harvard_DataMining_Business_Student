#!/usr/bin/env python3
"""
Daily OpenRouter model token usage collector.

Fetches the "tokens processed in the last 7 days" value for every model
listed on https://openrouter.ai/models and appends a row per model to
tokenomics_data/model_tokens.csv.

Stop collection early: commit a file named STOP inside tokenomics_data/.
Auto-stops after MAX_DAYS unique collection dates.
"""

import csv
import re
import sys
from datetime import date
from pathlib import Path

DATA_DIR = Path(__file__).parent
CSV_FILE = DATA_DIR / "model_tokens.csv"
STOP_FILE = DATA_DIR / "STOP"
MAX_DAYS = 30
URL = "https://openrouter.ai/models?input_modalities=text"

FIELDNAMES = ["date", "model_id", "model_name", "tokens_7d_raw", "tokens_7d"]

BROWSER_UA = (
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
)


# ---------------------------------------------------------------------------
# Stop / dedup guards
# ---------------------------------------------------------------------------

def existing_dates():
    if not CSV_FILE.exists():
        return set()
    with open(CSV_FILE, newline="") as f:
        return {row["date"] for row in csv.DictReader(f) if row.get("date")}


def should_stop(today: str) -> bool:
    if STOP_FILE.exists():
        print("STOP file found — collection halted.")
        return True
    days = existing_dates()
    if today in days:
        print(f"Data for {today} already collected — skipping.")
        return True
    if len(days) >= MAX_DAYS:
        print(f"Reached {MAX_DAYS}-day limit — collection complete.")
        return True
    return False


# ---------------------------------------------------------------------------
# Token string parser
# ---------------------------------------------------------------------------

def parse_tokens(text: str):
    """Convert '212B', '1.5T', '500M', '300K' to an integer, else None."""
    if not text:
        return None
    m = re.search(r"([\d,.]+)\s*([KMBT])", text.upper())
    if not m:
        return None
    try:
        val = float(m.group(1).replace(",", ""))
    except ValueError:
        return None
    mult = {"K": 1_000, "M": 1_000_000, "B": 1_000_000_000, "T": 1_000_000_000_000}
    return int(val * mult[m.group(2)])


# ---------------------------------------------------------------------------
# Strategy 1: plain HTTP (works if OpenRouter doesn't block server-side UA)
# ---------------------------------------------------------------------------

def try_requests_api():
    try:
        import requests
    except ImportError:
        return None

    headers = {
        "User-Agent": BROWSER_UA,
        "Accept": "application/json",
    }
    try:
        resp = requests.get(
            "https://openrouter.ai/api/v1/models",
            headers=headers,
            timeout=30,
        )
        if resp.status_code == 200:
            body = resp.json()
            items = body.get("data", [])
            if items and isinstance(items[0], dict) and "id" in items[0]:
                print(f"requests API: got {len(items)} models")
                return items
    except Exception as e:
        print(f"requests API failed: {e}")
    return None


# ---------------------------------------------------------------------------
# Strategy 2: Playwright — intercept the internal API call the page makes
# ---------------------------------------------------------------------------

def try_playwright():
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        print("playwright not installed")
        return None, None

    intercepted: list = []

    def on_response(response):
        try:
            url = response.url
            if "openrouter.ai" not in url or response.status != 200:
                return
            ct = response.headers.get("content-type", "")
            if "json" not in ct:
                return
            body = response.json()
            if not isinstance(body, dict):
                return
            items = body.get("data", [])
            if isinstance(items, list) and items and isinstance(items[0], dict) and "id" in items[0]:
                print(f"  intercepted: {url}  ({len(items)} items)")
                intercepted.extend(items)
        except Exception:
            pass

    with sync_playwright() as pw:
        browser = pw.chromium.launch(headless=True)
        ctx = browser.new_context(
            user_agent=BROWSER_UA,
            viewport={"width": 1920, "height": 1080},
        )
        page = ctx.new_page()
        page.on("response", on_response)

        print(f"Playwright: navigating to {URL}")
        try:
            page.goto(URL, wait_until="networkidle", timeout=90_000)
        except Exception as e:
            print(f"  navigation warning: {e}")
        page.wait_for_timeout(5_000)

        # Scroll to trigger lazy-loaded model rows
        prev_height = 0
        for _ in range(20):
            page.keyboard.press("End")
            page.wait_for_timeout(1_500)
            height = page.evaluate("document.body.scrollHeight")
            if height == prev_height:
                break
            prev_height = height

        # If we captured structured model data from intercepted calls, done.
        if intercepted:
            # Deduplicate by model id (the same endpoint may fire multiple times)
            seen, unique = set(), []
            for m in intercepted:
                if m.get("id") not in seen:
                    seen.add(m["id"])
                    unique.append(m)
            browser.close()
            return unique, "api_intercept"

        # Fall back: DOM extraction
        print("  no API intercepted — falling back to DOM parsing")
        dom_models = extract_from_dom(page)
        browser.close()
        return dom_models, "dom"


def extract_from_dom(page):
    """Pull model ids, names, and token counts from the rendered page."""
    # 1. Try Next.js __NEXT_DATA__ server-side data first
    page_data = page.evaluate("""() => {
        try { return window.__NEXT_DATA__; } catch { return null; }
    }""")
    if page_data:
        print("  found __NEXT_DATA__")
        return {"__next_data__": page_data}

    # 2. Walk the DOM looking for rows that contain both a model link and a
    #    token-count string like "212B" or "1.5T tokens".
    results = page.evaluate(r"""() => {
        const TOKEN_RE = /([\d,.]+)\s*([KMBT])\s*tokens?/i;
        const rows = [];
        const seen = new Set();

        // Gather candidate containers
        const candidates = [
            ...document.querySelectorAll('a[href*="/models/"]'),
        ];

        for (const anchor of candidates) {
            const href = anchor.getAttribute('href') || '';
            const modelId = href.replace(/^.*\/models\//, '').split('?')[0];
            if (!modelId || seen.has(modelId)) continue;

            // Walk up to find a container that includes a token count
            let container = anchor;
            for (let i = 0; i < 8; i++) {
                const t = container.innerText || '';
                if (TOKEN_RE.test(t)) break;
                if (!container.parentElement) break;
                container = container.parentElement;
            }

            const text = container.innerText || '';
            const tokenMatch = text.match(TOKEN_RE);
            if (!tokenMatch) continue;

            // Model name: look for heading-like elements first
            const nameEl = container.querySelector('h1,h2,h3,h4,[class*="name"],[class*="title"]');
            const name = (nameEl ? nameEl.innerText : anchor.innerText || modelId)
                .trim().replace(/\s+/g, ' ').slice(0, 120);

            seen.add(modelId);
            rows.push({
                id: modelId,
                name: name,
                token_text: tokenMatch[0].trim(),
            });
        }
        return rows;
    }""")
    return results


# ---------------------------------------------------------------------------
# Record builder
# ---------------------------------------------------------------------------

def build_records(raw, source: str, today: str):
    records = []

    if source == "requests_api":
        for m in raw:
            mid = m.get("id", "")
            name = m.get("name", mid)
            # The public /api/v1/models endpoint may or may not include usage stats.
            # Probe every field for something that looks like a token count.
            tok_raw = ""
            for k, v in m.items():
                if any(kw in k.lower() for kw in ("token", "usage", "weekly", "processed")):
                    tok_raw = str(v)
                    break
            records.append({
                "date": today,
                "model_id": mid,
                "model_name": name,
                "tokens_7d_raw": tok_raw,
                "tokens_7d": parse_tokens(tok_raw) or "",
            })

    elif source == "api_intercept":
        for m in raw:
            mid = m.get("id", "")
            name = m.get("name", mid)
            tok_raw = ""
            for k, v in m.items():
                if any(kw in k.lower() for kw in ("token", "usage", "weekly", "processed")):
                    tok_raw = str(v)
                    break
            records.append({
                "date": today,
                "model_id": mid,
                "model_name": name,
                "tokens_7d_raw": tok_raw,
                "tokens_7d": parse_tokens(tok_raw) or "",
            })

    elif source == "dom":
        if isinstance(raw, dict):
            # Raw __NEXT_DATA__ or other structured fallback — store as single debug row
            import json
            records.append({
                "date": today,
                "model_id": "__debug__",
                "model_name": "raw_page_data",
                "tokens_7d_raw": json.dumps(raw)[:500],
                "tokens_7d": "",
            })
        else:
            for m in raw:
                records.append({
                    "date": today,
                    "model_id": m.get("id", ""),
                    "model_name": m.get("name", ""),
                    "tokens_7d_raw": m.get("token_text", ""),
                    "tokens_7d": parse_tokens(m.get("token_text", "")) or "",
                })

    return records


# ---------------------------------------------------------------------------
# CSV writer
# ---------------------------------------------------------------------------

def append_csv(records):
    needs_header = not CSV_FILE.exists() or CSV_FILE.stat().st_size == 0
    with open(CSV_FILE, "a", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=FIELDNAMES)
        if needs_header:
            writer.writeheader()
        writer.writerows(records)
    print(f"Wrote {len(records)} records to {CSV_FILE.name}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    today = date.today().isoformat()

    if should_stop(today):
        sys.exit(0)

    days_so_far = len(existing_dates())
    print(f"Collection day {days_so_far + 1}/{MAX_DAYS} — {today}")

    # Strategy 1: plain requests (fast, no browser overhead)
    raw = try_requests_api()
    if raw:
        source = "requests_api"
    else:
        # Strategy 2: Playwright (real browser, network intercept + DOM fallback)
        raw, source = try_playwright()

    if not raw:
        print("ERROR: No model data retrieved.", file=sys.stderr)
        sys.exit(1)

    records = build_records(raw, source, today)
    if not records:
        print("ERROR: Zero records parsed.", file=sys.stderr)
        sys.exit(1)

    append_csv(records)
    remaining = MAX_DAYS - days_so_far - 1
    print(f"Done ({source}). {remaining} collection day(s) remaining.")


if __name__ == "__main__":
    main()
