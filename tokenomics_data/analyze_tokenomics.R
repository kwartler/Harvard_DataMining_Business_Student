# analyze_tokenomics.R
# Starter analysis for the OpenRouter tokenomics data set.
# Reads the two CSVs produced by fetch_tokens.py and writes a few plots that
# seed a lesson on new-technology diffusion and time-series analysis.
#
# Run from anywhere:  Rscript tokenomics_data/analyze_tokenomics.R
# Needs: tidyverse (install.packages("tidyverse") once).

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

# --- Locate the data folder relative to this script -------------------------
this_dir <- tryCatch(
  dirname(normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(FALSE), value = TRUE)))),
  error = function(e) "tokenomics_data"
)
if (length(this_dir) == 0 || !nzchar(this_dir)) this_dir <- "tokenomics_data"

tokens_path   <- file.path(this_dir, "model_tokens.csv")
registry_path <- file.path(this_dir, "model_registry.csv")
plot_dir      <- file.path(this_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE)

# --- Load --------------------------------------------------------------------
tokens <- read_csv(tokens_path, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
registry <- read_csv(registry_path, show_col_types = FALSE) %>%
  mutate(first_seen = as.Date(first_seen),
         last_seen  = as.Date(last_seen))

n_days <- n_distinct(tokens$date)
cat(sprintf("Loaded %d rows across %d day(s); %d models in the manifest.\n",
            nrow(tokens), n_days, nrow(registry)))

# --- 1. Platform daily totals (time series) ---------------------------------
daily <- tokens %>%
  group_by(date) %>%
  summarise(
    total_tokens = sum(total_tokens, na.rm = TRUE),
    total_cost   = sum(total_cost_usd, na.rm = TRUE),
    models_active = n_distinct(model_permaslug),
    .groups = "drop"
  )
write_csv(daily, file.path(this_dir, "daily_totals.csv"))
print(daily)

ggplot(daily, aes(date, total_tokens / 1e12)) +
  geom_line() + geom_point() +
  labs(title = "OpenRouter daily tokens (all models)",
       x = NULL, y = "Trillions of tokens") +
  theme_minimal()
ggsave(file.path(plot_dir, "daily_tokens.png"), width = 8, height = 4.5, dpi = 120)

ggplot(daily, aes(date, total_cost)) +
  geom_line() + geom_point() +
  labs(title = "Estimated daily spend (all models)",
       x = NULL, y = "USD") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()
ggsave(file.path(plot_dir, "daily_cost.png"), width = 8, height = 4.5, dpi = 120)

# --- 2. Top models on the most recent day -----------------------------------
latest <- max(tokens$date)
top_latest <- tokens %>%
  filter(date == latest) %>%
  slice_max(total_tokens, n = 15)

ggplot(top_latest,
       aes(reorder(model_name, total_tokens), total_tokens / 1e9)) +
  geom_col() + coord_flip() +
  labs(title = sprintf("Top 15 models by tokens on %s", latest),
       x = NULL, y = "Billions of tokens") +
  theme_minimal()
ggsave(file.path(plot_dir, "top_models_latest.png"),
       width = 8, height = 5, dpi = 120)

# --- 3. Technology diffusion: new models entering over time ------------------
diffusion <- registry %>%
  count(first_seen, name = "new_models") %>%
  arrange(first_seen) %>%
  mutate(cumulative_models = cumsum(new_models))

ggplot(diffusion, aes(first_seen, cumulative_models)) +
  geom_step() +
  labs(title = "Cumulative models seen (adoption / diffusion curve)",
       x = NULL, y = "Cumulative model count") +
  theme_minimal()
ggsave(file.path(plot_dir, "diffusion_curve.png"),
       width = 8, height = 4.5, dpi = 120)

# --- 4. Provider share on the most recent day -------------------------------
provider_share <- tokens %>%
  filter(date == latest) %>%
  group_by(provider) %>%
  summarise(tokens = sum(total_tokens, na.rm = TRUE), .groups = "drop") %>%
  slice_max(tokens, n = 10)

ggplot(provider_share,
       aes(reorder(provider, tokens), tokens / 1e12)) +
  geom_col() + coord_flip() +
  labs(title = sprintf("Token share by provider on %s", latest),
       x = NULL, y = "Trillions of tokens") +
  theme_minimal()
ggsave(file.path(plot_dir, "provider_share_latest.png"),
       width = 8, height = 5, dpi = 120)

cat(sprintf("Wrote plots to %s and daily_totals.csv\n", plot_dir))
if (n_days < 2) {
  cat("Note: only one day of data so far. The time-series plots fill in as the\n",
      "daily GitHub Action appends more dates.\n", sep = "")
}
