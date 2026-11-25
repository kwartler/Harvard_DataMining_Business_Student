#!/usr/bin/env bash
# Writes one combined CSV with a single header: tokenomics_data/hourly_top50_full.csv
cd "$(git rev-parse --show-toplevel)/tokenomics_data"
{ cat hourly_top50.csv | head -1
  { for f in hourly_archive/hourly_top50_*.csv.gz; do [ -e "$f" ] && gzip -dc "$f" | tail -n +2; done
    tail -n +2 hourly_top50.csv; } | sort -u
} > hourly_top50_full.csv
