# Full hourly history = monthly archives + the live 14-day file.
# Run from the repo root.
files <- c(Sys.glob("tokenomics_data/hourly_archive/hourly_top50_*.csv.gz"),
           "tokenomics_data/hourly_top50.csv")
hourly <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))
hourly <- hourly[!duplicated(hourly[, c("fetched_at_utc", "source_date",
                                        "model_permaslug", "variant")]), ]
