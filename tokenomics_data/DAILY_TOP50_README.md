# Separate actual-daily top 50 collection

This addition does not change `fetch_tokens.py`, `model_tokens.csv`, the registry, or the original `tokenomics_daily.yml` workflow.

## Daily dataset

`fetch_daily_top50.py` explicitly requests `view=day`, validates that every model row is for the previous UTC date, and ranks by prompt + completion tokens. The approved setting merges all variants into each exact base `model_permaslug`, then saves the top 50 distinct base models in `daily_top50.csv`. `variant_count` records how many source variants were merged. This matches the grouping in the existing collector: it groups by source `date` and exact `model_permaslug`, not by provider, display name, family, or the catalog `model_id` alias. Batch, standard, free, and any other source variants with that same exact permaslug are summed. Different Gemini Pro/Flash releases and different dated releases remain separate. This is different from the public leaderboard's top 50 model variants. The source represents public OpenRouter rankings traffic, not all AI usage.

The new job runs daily at 10:23 UTC, retries collection up to three times with five-minute waits, and retries a rejected push up to three times with fetch/rebase and no force. GitHub may delay or skip scheduled runs; retries cannot guarantee gap-free data. The script deliberately does not retrieve or infer missed historical days. A missing day remains missing.

CSV fields: UTC activity `date`, `rank`, `model_permaslug`, `input_tokens`, `output_tokens`, `total_tokens`, `requests`, `variant_count`, `ranking_unit`, `measurement_window`, `source_url`, `observed_at_utc`, and `raw_sha256`. There are no price estimates presented as actual spend. Raw source JSON is saved by activity date and hash in `daily_top50_raw/` for audit. Reruns preserve an already-complete date unchanged; a partial date fails for manual review rather than being silently treated as complete. The initial CSV is created by the first successful job, not supplied as guessed data.

## Example grouping

For the Nov 9, 2026 daily source capture checked during development:

- `google/gemini-3.8-flash-20260902`, standard: 242,078,901,263 tokens.
- Same `model_permaslug`, batch variant `google/gemini-3.8-flash-20260902:batch`: 298,600,795 tokens.
- Combined daily base-model total: 242,377,502,058 tokens.
- `google/gemini-4-argon-20261013` has a different permaslug and stays separate.

These are dated source observations, not fixed reference totals. Source revisions can change later captures. The first successful committed capture of a complete day is kept unchanged by ordinary reruns.

## Existing broad dataset

The existing `model_tokens.csv` is not rewritten or reclassified by this addition. The current broad collector omits `view=day`; current default-feed comparisons match the trailing seven-day rankings view. An August 22, 2026 code change removed the explicit daily parameter, so this statement should not be applied blindly to every historical row. Older rows and returned date labels have not all been semantically validated. Missing dates are accepted as gaps; the new code does not reconstruct them or insert zeros. The separate daily CSV must not be concatenated into the broad CSV as though both had the same measurement window.

Window-change evidence: https://github.com/kwartler/Harvard_DataMining_Business_Student/commit/a1482283811d07b462fb06c42552856003d0c60b

## Broad collector retry

`tokenomics_retry.yml` listens only for failures of the original "Tokenomics Daily Collection" workflow on the default branch in this repository. It checks out the trusted current default branch, not triggering-run code, then reruns the unchanged broad collector with up to three attempts. It also supports manual dispatch. It does not alter what that collector measures or attempt backfill.

Both new workflows use the existing STOP behavior for the broad collector; the separate daily collector has no STOP file behavior and is stopped by disabling its own workflow. Both additive jobs serialize with each other. The original workflow has not been edited to join that concurrency group. A conflicting original push can therefore occur; the helper handles safe rebases or fails visibly rather than overwriting data.

## Preserving data when a push fails

`push_collected.sh` copies all specified outputs into an immutable runner snapshot and verifies the copies before any rebase. No force push is used. A merge conflict or persistent push failure leaves a failed job, with preserved outputs uploaded as a 30-day GitHub Actions artifact. Artifacts also capture working outputs after a collection failure for diagnosis; those may be partial and are not a success record. Missing output files cause failure rather than a false success. A successful collection with no changes is reported as a no-op.

## Tests

Run `python -m unittest discover -s tokenomics_data -p 'test*.py' -v` from the repository root. Tests use local temporary files and git remotes, not production writes. Covered: model/variant ranking, malformed and stale responses, idempotence, partial-date protection, conflicting remote updates, push races and persistent push failure preservation.

Source semantics: https://openrouter.ai/rankings
Endpoint: https://openrouter.ai/api/frontend/v1/rankings/models?view=day

This frontend endpoint is not a versioned stability guarantee. Unexpected schema/date changes cause collection to fail rather than silently record another window. This implementation needs no OpenRouter API key and makes no historical API calls.
