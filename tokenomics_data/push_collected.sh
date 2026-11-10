#!/usr/bin/env bash
# Run only after successful collection. Preserve immutable outputs before git operations.
set -euo pipefail
snapshot=${RUNNER_TEMP:-/tmp}/tokenomics-preserved
mkdir -p "$snapshot"
for path in "$@"; do
  [[ -e "$path" ]] || { echo "Missing collected output: $path" >&2; exit 1; }
  mkdir -p "$snapshot/$(dirname "$path")"
  cp -a "$path" "$snapshot/$path"
done
# Fail before rebase if any copied output is missing/different.
for path in "$@"; do
  diff -qr "$path" "$snapshot/$path" >/dev/null || { echo 'Snapshot incomplete; no rebase attempted' >&2; exit 1; }
done
git config --local user.email '41898282+github-actions[bot]@users.noreply.github.com'
git config --local user.name 'github-actions[bot]'
git add -- "$@"
if git diff --cached --quiet; then
  echo 'Collection succeeded; no changed outputs to commit.'
  exit 0
fi
git commit -m "data: ${COLLECTION_LABEL:-tokenomics collection} $(date -u +%F)"
for attempt in 1 2 3; do
  git fetch origin "$TARGET_BRANCH"
  if ! git rebase "origin/$TARGET_BRANCH"; then
    git rebase --abort || true
    echo "Rebase conflict. Original collected outputs preserved in $snapshot; not pushed." >&2
    exit 1
  fi
  if git push origin "HEAD:$TARGET_BRANCH"; then
    echo 'Collected outputs pushed successfully.'
    exit 0
  fi
  echo "Push attempt $attempt failed; retrying fetch/rebase without force." >&2
  sleep 5
done
echo "Push failed after 3 attempts. Original collected outputs preserved in $snapshot; not confirmed saved in repo." >&2
exit 1
