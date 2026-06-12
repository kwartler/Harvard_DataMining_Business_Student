#!/usr/bin/env python3
"""
Daily OpenRouter model token usage + pricing collector.

Two output files:
  model_registry.csv  — one row per model (upserted): provider, context, first/last seen
  model_tokens.csv    — daily append: token usage + input/output prices

Token counts are fetched via the internal /api/frontend/v1/stats/router-activity endpoint
using same-origin Playwright fetches (required — plain HTTP returns 404).

Runs indefinitely. Stop by disabling the workflow or committing tokenomics_data/STOP.
"""

import csv
import re
import sys
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

DATA_DIR     = Path(__file__).parent
TOKENS_CSV   = DATA_DIR / "model_tokens.csv"
REGISTRY_CSV = DATA_DIR / "model_registry.csv"
STOP_FILE    = DATA_DIR / "STOP"
MODELS_URL   = "https://openrouter.ai/models?input_modalities=text"

TOKENS_FIELDS = [
    "date", "model_id", "model_name",
    "tokens_7d_raw", "tokens_7d", "tokens_1d",
    "input_price_per_1m", "output_price_per_1m",
    "source",
]
REGISTRY_FIELDS = [
    "model_id", "model_name", "provider",
    "context_length", "context_display",
    "permaslug", "first_seen", "last_seen",
]

BROWSER_UA = (
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
)
ROUTER_ACTIVITY_PATH = "/api/frontend/v1/stats/router-activity"
CHUNK_SIZE = 40       # models per batch-fetch chunk
FETCH_DELAY_MS = 150  # ms between calls inside JS


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def migrate_tokens_csv():
    """
    Self-heal model_tokens.csv if its header predates a schema change.
    Older runs wrote an 8-column header (no tokens_1d); appending 9-column
    rows under it makes the last field overflow when parsed. Rewrite the
    file with the current header, inserting an empty tokens_1d for old rows.
    """
    if not TOKENS_CSV.exists() or TOKENS_CSV.stat().st_size == 0:
        return
    with open(TOKENS_CSV, newline="") as f:
        rows = list(csv.reader(f))
    if not rows or rows[0] == TOKENS_FIELDS:
        return

    print(f"Migrating model_tokens.csv: header {len(rows[0])} -> "
          f"{len(TOKENS_FIELDS)} columns")
    out_rows = []
    for r in rows[1:]:
        if len(r) == 9:                       # already new schema
            rec = r
        elif len(r) == 8:                     # old: insert empty tokens_1d
            rec = r[:5] + [""] + r[5:]        # after tokens_7d (index 5)
        else:                                 # defensive pad/truncate
            rec = (r + [""] * 9)[:9]
        out_rows.append(rec)

    with open(TOKENS_CSV, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(TOKENS_FIELDS)
        w.writerows(out_rows)
    print(f"  migrated {len(out_rows)} rows to current schema")


def existing_dates():
    if not TOKENS_CSV.exists():
        return set()
    with open(TOKENS_CSV, newline="") as f:
        return {row["date"] for row in csv.DictReader(f) if row.get("date")}


def should_stop(today: str) -> bool:
    if STOP_FILE.exists():
        print("STOP file found — halted.")
        return True
    if today in existing_dates():
        print(f"Data for {today} already collected — skipping.")
        return True
    return False


def parse_tokens(text) -> int | None:
    if text is None:
        return None
    if isinstance(text, (int, float)) and text > 0:
        return int(text)
    s = str(text).strip()
    m = re.search(r"([\d,.]+)\s*([KMBT])", s.upper())
    if m:
        try:
            val = float(m.group(1).replace(",", ""))
            return int(val * {"K": 1e3, "M": 1e6, "B": 1e9, "T": 1e12}[m.group(2)])
        except (ValueError, KeyError):
            pass
    try:
        v = int(s.replace(",", ""))
        return v if v > 1_000_000 else None
    except ValueError:
        return None


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
    return model_id.split("/")[0].replace("-", " ").title()


def pricing_from_model(m: dict):
    p = m.get("pricing", {}) or {}
    try:
        inp = round(float(p.get("prompt", 0) or 0) * 1_000_000, 6)
    except (ValueError, TypeError):
        inp = 0.0
    try:
        out = round(float(p.get("completion", 0) or 0) * 1_000_000, 6)
    except (ValueError, TypeError):
        out = 0.0
    return inp, out


# ---------------------------------------------------------------------------
# Public API — model list + pricing (no token stats here)
# ---------------------------------------------------------------------------

def fetch_api_models():
    try:
        import requests
    except ImportError:
        return None
    try:
        r = requests.get(
            "https://openrouter.ai/api/v1/models",
            headers={"User-Agent": BROWSER_UA, "Accept": "application/json"},
            timeout=30,
        )
        if r.status_code == 200:
            items = r.json().get("data", [])
            if items and "id" in items[0]:
                print(f"API: {len(items)} models")
                return items
        print(f"API HTTP {r.status_code}")
    except Exception as e:
        print(f"API error: {e}")
    return None


# ---------------------------------------------------------------------------
# Permaslug extraction helpers
# ---------------------------------------------------------------------------

def _collect_slug_permaslug(obj, out: dict):
    """
    Recursively walk a JSON structure, recording slug/id -> permaslug for any
    object that carries a permaslug alongside a slug or id. The catalog/models
    and models/find?fmt=cards responses nest model objects, so a flat
    data[0].permaslug assumption misses them — this walks the whole tree.
    """
    if isinstance(obj, dict):
        ps = obj.get("permaslug")
        if isinstance(ps, str) and ps:
            key = obj.get("slug") or obj.get("id")
            if isinstance(key, str) and key:
                out.setdefault(key, ps)
        for v in obj.values():
            _collect_slug_permaslug(v, out)
    elif isinstance(obj, list):
        for v in obj:
            _collect_slug_permaslug(v, out)


def extract_permaslugs_from_json(all_json: dict) -> dict:
    """Recursively scan all intercepted JSON responses for slug->permaslug."""
    slugs: dict = {}
    for body in all_json.values():
        _collect_slug_permaslug(body, slugs)
    if slugs:
        print(f"  permaslugs from intercepted JSON: {len(slugs)} found")
    return slugs


def fetch_permaslugs_via_frontend(page) -> dict:
    """
    Fetch OpenRouter's own frontend model-list endpoints same-origin and walk
    the response for slug->permaslug pairs. These are the real endpoints the
    page uses (discovered via response interception). Recurses the JSON so it
    works regardless of how the model objects are nested.
    """
    candidates = [
        "/api/frontend/v1/models/find?active=true&fmt=cards&input_modalities=text",
        "/api/frontend/v1/catalog/models",
        "/api/frontend/v1/models/find?active=true&fmt=cards",
    ]
    result = page.evaluate(
        """async (paths) => {
            const log = [];
            const out = {};
            const walk = (o) => {
                if (!o || typeof o !== 'object') return;
                if (Array.isArray(o)) { for (const v of o) walk(v); return; }
                const ps = o.permaslug;
                if (typeof ps === 'string' && ps) {
                    const key = o.slug || o.id;
                    if (typeof key === 'string' && key && !(key in out)) {
                        out[key] = ps;
                    }
                }
                for (const k in o) walk(o[k]);
            };
            for (const p of paths) {
                try {
                    const res = await fetch(p, {
                        credentials: 'include',
                        headers: { 'Accept': 'application/json' }
                    });
                    log.push(p + ' -> ' + res.status);
                    if (!res.ok) continue;
                    walk(await res.json());
                    if (Object.keys(out).length) return { path: p, slugs: out, log };
                } catch (e) {
                    log.push(p + ' -> ERR ' + e);
                }
            }
            return { path: null, slugs: out, log };
        }""",
        candidates,
    )
    print(f"  frontend model-list probe: {result.get('log')}")
    slugs = result.get("slugs", {}) or {}
    if slugs:
        print(f"  permaslugs from {result.get('path')}: {len(slugs)} found")
    return slugs


def extract_permaslugs_from_page_html(page) -> dict:
    """
    Extract slug->permaslug pairs from the page HTML, tolerating escaped
    quotes. OpenRouter embeds model data as JSON-inside-JSON (RSC streaming),
    so quotes appear as \\" — the old unescaped-only regex matched nothing.
    """
    matches = page.evaluate(r"""() => {
        // Normalize any run of backslashes before a quote down to a bare quote.
        let html = document.documentElement.innerHTML.replace(/\\+"/g, '"');
        const out = {};
        let m;
        // slug -> permaslug (slug precedes permaslug in a model object)
        const re1 = /"slug":"([^"]+)"[^{}]{0,600}?"permaslug":"([^"]+)"/g;
        while ((m = re1.exec(html)) !== null) out[m[1]] = m[2];
        // permaslug -> slug (reversed field order)
        const re2 = /"permaslug":"([^"]+)"[^{}]{0,600}?"slug":"([^"]+)"/g;
        while ((m = re2.exec(html)) !== null) { if (!out[m[2]]) out[m[2]] = m[1]; }
        // id -> permaslug as a final pairing attempt
        const re3 = /"id":"([^"]+)"[^{}]{0,600}?"permaslug":"([^"]+)"/g;
        while ((m = re3.exec(html)) !== null) { if (!out[m[1]]) out[m[1]] = m[2]; }
        const raw = (html.match(/"permaslug":"/g) || []).length;
        return { pairs: out, raw_count: raw };
    }""")
    out = matches.get("pairs", {}) if isinstance(matches, dict) else {}
    raw = matches.get("raw_count", 0) if isinstance(matches, dict) else 0
    print(f"  page HTML: {raw} raw permaslug occurrences, "
          f"{len(out)} paired to slugs")
    return out or {}


# ---------------------------------------------------------------------------
# Batch same-origin router-activity fetch (runs inside the loaded page)
# ---------------------------------------------------------------------------

def batch_fetch_router_activity(page, permaslugs: list[str]) -> dict:
    """
    permaslugs: list of permaslug strings
    Returns: {permaslug: response_body_dict}
    Runs fetch() inside the loaded openrouter.ai page context so cookies/
    CSRF headers are automatically attached.
    """
    results = {}
    for i in range(0, len(permaslugs), CHUNK_SIZE):
        chunk = permaslugs[i:i + CHUNK_SIZE]
        print(f"  router-activity batch {i // CHUNK_SIZE + 1}/"
              f"{(len(permaslugs) + CHUNK_SIZE - 1) // CHUNK_SIZE} "
              f"({len(chunk)} models)...")
        try:
            chunk_results = page.evaluate(
                """async ([slugs, path, delayMs]) => {
                    const out = {};
                    const sleep = ms => new Promise(r => setTimeout(r, ms));
                    for (const slug of slugs) {
                        try {
                            const url = path + '?permaslug=' + encodeURIComponent(slug);
                            const res = await fetch(url, {
                                credentials: 'include',
                                headers: { 'Accept': 'application/json' }
                            });
                            out[slug] = res.ok
                                ? await res.json()
                                : { _http_error: res.status };
                        } catch(e) {
                            out[slug] = { _js_error: String(e) };
                        }
                        await sleep(delayMs);
                    }
                    return out;
                }""",
                [chunk, ROUTER_ACTIVITY_PATH, FETCH_DELAY_MS],
            )
            results.update(chunk_results or {})
        except Exception as e:
            print(f"  chunk error: {e}")
    return results


# ---------------------------------------------------------------------------
# Parse router-activity analytics → 7-day token total
# ---------------------------------------------------------------------------

_debug_logged = False

def analytics_to_tokens(activity: dict, today_str: str) -> tuple[int | None, int | None]:
    """
    Parse router-activity analytics into two figures:
      tokens_7d  — sum of token counts across the last 7 days (rolling total)
      tokens_1d  — the most recent single day's token count (non-cumulative)

    The analytics array is a per-period, per-series time series. We sum across
    all series within each date, then derive the 7-day rolling total and the
    latest single-day total. Returns (tokens_7d, tokens_1d).
    """
    global _debug_logged

    if not activity or not isinstance(activity, dict):
        return None, None
    if "_http_error" in activity or "_js_error" in activity:
        return None, None

    analytics = activity.get("analytics", [])
    if not isinstance(analytics, list) or not analytics:
        return None, None

    # Log structure of first item once per run to aid debugging
    if not _debug_logged:
        sample = analytics[0] if analytics else {}
        print(f"  analytics[0] sample: {dict(list(sample.items())[:6])}")
        _debug_logged = True

    # Identify the token-count field: the numeric field with the largest sum
    field_totals: dict[str, float] = {}
    for item in analytics:
        if not isinstance(item, dict):
            continue
        for k, v in item.items():
            if isinstance(v, (int, float)) and v > 0:
                field_totals[k] = field_totals.get(k, 0) + v

    if not field_totals:
        return None, None

    token_field = max(field_totals, key=lambda k: field_totals[k])

    # Identify the date/period field
    date_field = None
    for item in analytics[:5]:
        for k, v in item.items():
            if isinstance(v, str) and re.match(r"\d{4}-\d{2}-\d{2}", v):
                date_field = k
                break
        if date_field:
            break

    # Aggregate token counts by date (summing across all series per day)
    by_date: dict[str, float] = {}
    undated_total = 0.0
    for item in analytics:
        if not isinstance(item, dict):
            continue
        tok = item.get(token_field, 0) or 0
        d = None
        if date_field:
            dv = item.get(date_field)
            if isinstance(dv, str):
                d = dv[:10]
        if d:
            by_date[d] = by_date.get(d, 0) + tok
        else:
            undated_total += tok

    # 7-day rolling total
    cutoff = (datetime.fromisoformat(today_str) - timedelta(days=7)).date()
    total_7d = undated_total
    for d, v in by_date.items():
        try:
            if datetime.fromisoformat(d).date() < cutoff:
                continue
        except ValueError:
            pass
        total_7d += v

    # Most recent single day (non-cumulative)
    tokens_1d = None
    dated = sorted((d, v) for d, v in by_date.items() if d)
    if dated:
        tokens_1d = dated[-1][1]

    return (
        int(total_7d) if total_7d > 0 else None,
        int(tokens_1d) if tokens_1d else None,
    )


# ---------------------------------------------------------------------------
# Text-proximity fallback (used if router-activity yields nothing)
# ---------------------------------------------------------------------------

def text_proximity_tokens(page, api_models: list) -> dict:
    """Extract token counts from visible page text as a last resort."""
    name_lookup: dict[str, str] = {}
    for m in api_models:
        mid = m.get("id", "")
        name = m.get("name", "")
        name_lookup[name.lower()] = mid
        if ":" in name:
            name_lookup[name.split(":", 1)[1].strip().lower()] = mid

    pairs = page.evaluate(r"""() => {
        const lines = (document.body.innerText || '')
            .split('\n').map(l => l.trim()).filter(l => l);
        const TOK = /^([\d,.]+)\s*([KMBT])\s*tokens?$/i;
        const SKIP = /^\$|^https?:|^\d+\s*ms$|^\d+\s*[Kk]\/s$|^Context|^Input|^Output/i;
        const DATE_HDR = /^[A-Z][a-z]+ \d{4}$/;
        const results = [];
        for (let i = 0; i < lines.length; i++) {
            if (!TOK.test(lines[i])) continue;
            for (let j = 1; j <= 10; j++) {
                if (i - j < 0) break;
                const prev = lines[i - j];
                if (!prev || SKIP.test(prev) || DATE_HDR.test(prev)) continue;
                if (TOK.test(prev)) break;
                results.push({ name: prev, token_text: lines[i] });
                break;
            }
        }
        return results;
    }""")

    result: dict[str, str] = {}
    for pair in pairs:
        mid = name_lookup.get(pair["name"].lower())
        if not mid:
            # partial match
            pl = pair["name"].lower()
            for m in api_models:
                n = m.get("name", "").lower()
                short = n.split(":", 1)[1].strip() if ":" in n else n
                if short == pl or n == pl:
                    mid = m.get("id")
                    break
        if mid:
            result[mid] = pair["token_text"]

    print(f"  text-proximity fallback: {len(result)} matched")
    return result


# ---------------------------------------------------------------------------
# Main Playwright orchestration
# ---------------------------------------------------------------------------

def fetch_token_counts(api_models: list) -> tuple[dict, dict, str]:
    """
    Returns (token_lookup, daily_lookup, source_label).
      token_lookup — {model_id: 7-day value (int, or raw str from fallback)}
      daily_lookup — {model_id: most-recent single-day token int}
    Tries router-activity first, falls back to text-proximity (7d only).
    """
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        print("playwright not installed")
        return {}, {}, "none"

    all_json: dict = {}

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
        page.set_default_timeout(120_000)

        print(f"Playwright: loading {MODELS_URL}")
        try:
            page.goto(MODELS_URL, wait_until="networkidle", timeout=90_000)
        except Exception as e:
            print(f"  nav warning: {e}")
        page.wait_for_timeout(6_000)

        # Scroll to trigger full model list
        prev_h = 0
        for _ in range(25):
            page.keyboard.press("End")
            page.wait_for_timeout(1_000)
            h = page.evaluate("document.body.scrollHeight")
            if h == prev_h:
                break
            prev_h = h

        # Diagnostic: what JSON did the page fetch on its own?
        print(f"  intercepted {len(all_json)} JSON responses")
        for u in list(all_json)[:25]:
            has_ps = "permaslug" in str(all_json[u])[:8000]
            print(f"    {u[:95]}  permaslug={has_ps}")

        # ---- Step 1: find permaslugs ----
        # The page already fetches catalog/models + models/find (both carry
        # permaslugs), so parse the intercepted JSON first — no extra network.
        permaslugs = extract_permaslugs_from_json(all_json)
        if not permaslugs:
            permaslugs = fetch_permaslugs_via_frontend(page)
        if not permaslugs:
            permaslugs = extract_permaslugs_from_page_html(page)

        if permaslugs:
            print(f"  {len(permaslugs)} permaslugs available, "
                  f"fetching router-activity...")
            slug_list = list(permaslugs.values())
            raw_activity = batch_fetch_router_activity(page, slug_list)

            today_str = date.today().isoformat()
            token_counts: dict[str, int] = {}
            daily_counts: dict[str, int] = {}
            errors = 0
            for mid, ps in permaslugs.items():
                activity = raw_activity.get(ps, {})
                t7, t1 = analytics_to_tokens(activity, today_str)
                if t7 is not None:
                    token_counts[mid] = t7
                if t1 is not None:
                    daily_counts[mid] = t1
                if t7 is None and (
                    "_http_error" in activity or "_js_error" in activity
                ):
                    errors += 1

            coverage = len(token_counts)
            print(f"  router-activity: {coverage}/{len(permaslugs)} models "
                  f"returned data, {errors} errors")

            if coverage > 0:
                # Update registry with permaslugs
                _permaslug_cache.update(permaslugs)
                browser.close()
                return token_counts, daily_counts, "router_activity"
            else:
                print("  router-activity returned 0 counts — trying fallback")

        # ---- Step 2: text-proximity fallback (7-day only) ----
        tok = text_proximity_tokens(page, api_models)
        browser.close()
        return tok, {}, "text_proximity"


# Shared permaslug cache so update_registry can use it
_permaslug_cache: dict[str, str] = {}


# ---------------------------------------------------------------------------
# Registry upsert
# ---------------------------------------------------------------------------

def update_registry(api_models: list, today: str):
    existing: dict = {}
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
        ps = _permaslug_cache.get(mid, existing.get(mid, {}).get("permaslug", ""))

        if mid not in existing:
            existing[mid] = {
                "model_id": mid, "model_name": name, "provider": provider,
                "context_length": ctx, "context_display": format_context(ctx),
                "permaslug": ps,
                "first_seen": today, "last_seen": today,
            }
            changed = True
        else:
            row = existing[mid]
            for k, v in [("model_name", name), ("provider", provider),
                         ("context_length", ctx),
                         ("context_display", format_context(ctx)),
                         ("permaslug", ps),
                         ("last_seen", today)]:
                if str(row.get(k, "")) != str(v):
                    row[k] = v
                    changed = True

    if changed:
        rows = sorted(existing.values(), key=lambda r: r["model_id"])
        with open(REGISTRY_CSV, "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=REGISTRY_FIELDS)
            w.writeheader()
            w.writerows(rows)
        new_today = sum(1 for r in rows if r["first_seen"] == today)
        print(f"Registry: {len(rows)} models, {new_today} new today")
    else:
        print(f"Registry: no changes ({len(existing)} models)")


# ---------------------------------------------------------------------------
# Build + append daily token records
# ---------------------------------------------------------------------------

def build_and_append(api_models: list, token_lookup: dict, daily_lookup: dict,
                     today: str, source: str):
    records = []
    for m in api_models:
        mid = m.get("id", "")
        raw = token_lookup.get(mid)
        tok_int = raw if isinstance(raw, int) else parse_tokens(raw)
        tok_raw = str(raw) if raw is not None else ""
        day_int = daily_lookup.get(mid)
        inp, out = pricing_from_model(m)
        records.append({
            "date": today,
            "model_id": mid,
            "model_name": m.get("name", mid),
            "tokens_7d_raw": tok_raw,
            "tokens_7d": tok_int if tok_int is not None else "",
            "tokens_1d": day_int if day_int is not None else "",
            "input_price_per_1m": inp,
            "output_price_per_1m": out,
            "source": source,
        })

    needs_header = not TOKENS_CSV.exists() or TOKENS_CSV.stat().st_size == 0
    with open(TOKENS_CSV, "a", newline="") as f:
        w = csv.DictWriter(f, fieldnames=TOKENS_FIELDS)
        if needs_header:
            w.writeheader()
        w.writerows(records)

    with_tok = sum(1 for r in records if r["tokens_7d"] != "")
    with_day = sum(1 for r in records if r["tokens_1d"] != "")
    with_price = sum(1 for r in records if r["input_price_per_1m"])
    print(f"Tokens CSV: {len(records)} rows | {with_tok} with 7d | "
          f"{with_day} with 1d | {with_price} with pricing | source={source}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    today = date.today().isoformat()

    migrate_tokens_csv()  # self-heal stale header before anything reads the CSV

    if should_stop(today):
        sys.exit(0)

    days_so_far = len(existing_dates())
    print(f"Collection day {days_so_far + 1} — {today}")

    api_models = fetch_api_models()
    if not api_models:
        print("ERROR: Could not fetch model list", file=sys.stderr)
        sys.exit(1)

    token_lookup, daily_lookup, source = fetch_token_counts(api_models)

    update_registry(api_models, today)
    build_and_append(api_models, token_lookup, daily_lookup, today, source)

    print(f"Done. Total days collected: {days_so_far + 1}.")


if __name__ == "__main__":
    main()
