library(tidyverse)

tokens <- read_csv("tokenomics_data/model_tokens.csv")

# See which models appeared/disappeared
model_activity <- tokens %>%
  group_by(model_id, model_name) %>%
  summarise(first_seen = min(date), last_seen = max(date), days = n())

# Plot token usage over time for top models
tokens %>%
  filter(!is.na(tokens_7d) & tokens_7d > 0) %>%
  ggplot(aes(x = date, y = tokens_7d, color = model_name)) +
  geom_line()

tokens   <- read_csv("tokenomics_data/model_tokens.csv")
registry <- read_csv("tokenomics_data/model_registry.csv")

tokens %>%
  left_join(registry %>% select(model_id, provider, context_display), by = "model_id") %>%
  filter(provider == "Anthropic", context_display == "200K")