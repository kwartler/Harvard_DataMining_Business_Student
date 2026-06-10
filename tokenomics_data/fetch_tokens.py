#!/usr/bin/env python3
"""
Daily OpenRouter model token usage collector.

Fetches the "tokens processed in the last 7 days" value for every model
listed on https://openrouter.ai/models and appends a row per model to
tokenomics_data/model_tokens.csv.

Runs indefinitely. To stop: disable the GitHub Actions workflow in the
Actions tab, or commit a file named STOP inside tokenomics_data/.
"""

import csv
import json
import re
import sys
from datetime import date
from pathlib import Path

DATA_DIR = Path(__file__).parent
CSV_FILE = DATA_DIR / "model_tokens.csv"
STOP_FILE = DATA_DIR / "STOP"
URL = "https://openrouter.ai/models?input_modalities=text"

FIELDNAMES = ["date", "model_id", "model_name", "tokens_7d_raw", "tokens_7d", "source"]

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
    if today in existing_dates():
        print(f"Data for {today} already collected — skipping.")
        return True
    return False


# ---------------------------------------------------------------------------
# Token string parser
# ---------------------------------------------------------------------------

def parse_tokens(text: str):
    """Convert '212B', '1.5T', '500M', '300K' to an integer, else None."""
    if not text:
        return None
    m = re.search(r"([\d,.]+)\s*([KMBT])", str(text).upper())
    if not m:
        # Try plain integer
        try:
            v = int(str(text).replace(",", "").strip())
            return v if v > 1_000_000 else None
        except ValueError:
            return None
    try:
        val = float(m.group(1).replace(",", ""))
    except ValueError:
        return None
    mult = {"K": 1_000, "M": 1_000_000, "B": 1_000_000_000, "T": 1_000_000_000_000}
    return int(val * mult[m.group(2)])


# ---------------------------------------------------------------------------
# Recursive search through nested dicts/lists for model arrays
# ---------------------------------------------------------------------------

def find_model_lists(obj, depth=0):
    """Recursively find lists that look like OpenRouter model arrays."""
    if depth > 8:
        return []
    found = []
    if isinstance(obj, list) and len(obj) > 5:
        if obj and isinstance(obj[0], dict) and "id" in obj[0]:
            if "/" in str(obj[0].get("id", "")):  # e.g. "openai/gpt-4"
                found.append(obj)
    if isinstance(obj, dict):
        for v in obj.values():
            found.extend(find_model_lists(v, depth + 1))
    elif isinstance(obj, list):
        for item in obj[:3]:  # only first few items to avoid explosion
            found.extend(find_model_lists(item, depth + 1))
    return found


def token_fields_from_model(m: dict):
    """Try every field to find one that looks like a 7-day token count."""
    # Direct token-related keys (various naming conventions seen in APIs)
    priority_keys = [
        "tokens_7d", "tokens_7_days", "weekly_tokens", "tokens_processed",
        "tokens_processed_7d", "usage_7d", "7d_tokens", "volume",
        "token_volume", "popularity", "traffic",
    ]
    for k in priority_keys:
        if k in m and m[k]:
            return str(m[k])

    # Any key that contains these substrings (but not "context" which is window size)
    for k, v in m.items():
        kl = k.lower()
        if "context" in kl or "max" in kl or "limit" in kl:
            continue
        if any(w in kl for w in ("token", "usage", "weekly", "processed", "volume", "traffic")):
            return str(v)

    # Large integer that could be a token count (>= 1 million)
    for k, v in m.items():
        if isinstance(v, (int, float)) and v >= 1_000_000:
            return str(int(v))

    return ""


# ---------------------------------------------------------------------------
# Playwright-based collection
# ---------------------------------------------------------------------------

def collect_via_playwright():
    from playwright.sync_api import sync_playwright

    all_json = {}  # url -> body

    def on_response(response):
        try:
            if "openrouter.ai" not in response.url:
                return
            if response.status != 200:
                return
            ct = response.headers.get("content-type", "")
            if "json" not in ct:
                return
            all_json[response.url] = response.json()
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

        print(f"Loading {URL} ...")
        try:
            page.goto(URL, wait_until="networkidle", timeout=90_000)
        except Exception as e:
            print(f"  navigation warning: {e}")
        page.wait_for_timeout(6_000)

        # Scroll to load all models (infinite scroll)
        prev_h = 0
        for _ in range(25):
            page.keyboard.press("End")
            page.wait_for_timeout(1_200)
            h = page.evaluate("document.body.scrollHeight")
            if h == prev_h:
                break
            prev_h = h

        # ----------------------------------------------------------------
        # Strategy A: find an intercepted response that has token data
        # ----------------------------------------------------------------
        best_with_tokens = None
        best_without_tokens = None

        for url, body in all_json.items():
            # Unwrap {"data": [...]} envelope
            items = body.get("data", body) if isinstance(body, dict) else body
            if not isinstance(items, list) or not items:
                continue
            if not isinstance(items[0], dict) or "id" not in items[0]:
                continue
            # Must look like OpenRouter model IDs (provider/model)
            if "/" not in str(items[0].get("id", "")):
                continue

            has_tok = any(token_fields_from_model(m) for m in items[:5])
            print(f"  intercepted {url}: {len(items)} models, token_data={has_tok}")

            if has_tok and best_with_tokens is None:
                best_with_tokens = (items, url)
            elif not has_tok and best_without_tokens is None:
                best_without_tokens = (items, url)

        if best_with_tokens:
            items, url = best_with_tokens
            print(f"Using token-rich API response ({len(items)} models) from {url}")
            browser.close()
            return items, "api_intercept"

        # ----------------------------------------------------------------
        # Strategy B: __NEXT_DATA__ (server-side rendered props)
        # ----------------------------------------------------------------
        next_data = page.evaluate("() => { try { return window.__NEXT_DATA__; } catch { return null; } }")
        if next_data:
            lists = find_model_lists(next_data)
            if lists:
                best = max(lists, key=len)
                print(f"Found {len(best)} models in __NEXT_DATA__")
                has_tok = any(token_fields_from_model(m) for m in best[:5])
                if has_tok:
                    browser.close()
                    return best, "next_data"

        # ----------------------------------------------------------------
        # Strategy C: DOM scraping — extract visible token count text
        # ----------------------------------------------------------------
        print("Falling back to DOM scraping for token counts...")

        # First get model names from best intercepted response
        model_names = {}
        if best_without_tokens:
            for m in best_without_tokens[0]:
                model_names[m.get("id", "")] = m.get("name", m.get("id", ""))

        dom_tokens = page.evaluate(r"""() => {
            const results = [];
            const seen = new Set();

            // Match numbers like 212B, 1.5T, 500M, 300K (with or without "tokens")
            const TOK_RE = /([\d,.]+)\s*([KMBT])\b/i;

            const allLinks = document.querySelectorAll('a[href*="/models/"]');
            for (const a of allLinks) {
                const href = a.getAttribute('href') || '';
                // OpenRouter model IDs: provider/model-name
                const afterModels = href.split('/models/')[1];
                if (!afterModels) continue;
                const modelId = afterModels.split('?')[0];
                if (!modelId || !modelId.includes('/') || seen.has(modelId)) continue;

                const nameEl = a.querySelector('h1,h2,h3,h4,span,p') || a;
                const name = nameEl.innerText.trim().replace(/\s+/g, ' ').slice(0, 120) || modelId;

                // Walk up the DOM to find a sibling/ancestor with a token count
                let tokenText = '';
                let el = a;
                for (let i = 0; i < 12 && !tokenText; i++) {
                    const text = (el.innerText || '').replace(/\n/g, ' ');
                    const m = text.match(TOK_RE);
                    if (m) {
                        // Make sure the number is large enough to be a token count
                        // (skip things like "8B params" that are in the model name)
                        // Look for the token count specifically near "tok" or at end of line
                        const tokMatch = text.match(/([\d,.]+)\s*([KMBT])\s*(tok|token)/i);
                        if (tokMatch) {
                            tokenText = tokMatch[0];
                        } else if (m) {
                            tokenText = m[0];
                        }
                    }
                    if (!el.parentElement) break;
                    el = el.parentElement;
                }

                seen.add(modelId);
                results.push({ id: modelId, name: name, token_text: tokenText });
            }
            return results;
        }""")

        browser.close()

        # Merge DOM token counts with API model names where possible
        if dom_tokens and model_names:
            for item in dom_tokens:
                if not item.get("name") and item["id"] in model_names:
                    item["name"] = model_names[item["id"]]

        return dom_tokens or [], "dom"


# ---------------------------------------------------------------------------
# Record builder
# ---------------------------------------------------------------------------

def build_records(raw, src: str, today: str):
    records = []

    if src in ("api_intercept", "next_data", "requests_api"):
        for m in raw:
            mid = m.get("id", "")
            name = m.get("name", mid)
            tok_raw = token_fields_from_model(m)
            records.append({
                "date": today,
                "model_id": mid,
                "model_name": name,
                "tokens_7d_raw": tok_raw,
                "tokens_7d": parse_tokens(tok_raw) or "",
                "source": src,
            })

    elif src == "dom":
        for m in raw:
            tok_raw = m.get("token_text", "")
            records.append({
                "date": today,
                "model_id": m.get("id", ""),
                "model_name": m.get("name", ""),
                "tokens_7d_raw": tok_raw,
                "tokens_7d": parse_tokens(tok_raw) or "",
                "source": src,
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
    print(f"Collection day {days_so_far + 1} — {today}")

    raw, source = collect_via_playwright()

    if not raw:
        print("ERROR: No model data retrieved.", file=sys.stderr)
        sys.exit(1)

    records = build_records(raw, source, today)
    if not records:
        print("ERROR: Zero records parsed.", file=sys.stderr)
        sys.exit(1)

    # Report token coverage
    with_tokens = sum(1 for r in records if r["tokens_7d"] != "")
    print(f"Models: {len(records)}, with token data: {with_tokens} ({source})")

    append_csv(records)
    print(f"Done. Total days collected: {days_so_far + 1}.")


if __name__ == "__main__":
    main()
