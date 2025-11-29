#' Author: Ted Kwartler
#' Apr 28
#' An example for students using lm-studio locally.

# Libraries
library(httr)
library(jsonlite)

# Inputs
prompt <- "Hello." #What is the capital of Brazil?
llmModel <- "qwen2.5:7b"#"qwen2.5:7b" #"llama3.2-1b-instruct"

urlAPI <- "http://127.0.0.1:1337/v1/chat/completions"

#body <- list(
#  messages = list(
#    list(role = "user", content = prompt)), model = llmModel)
body <- list(
  messages = list(
    list(role = "system", content = "You are a helpful and concise assistant."),
    list(role = "user", content = prompt)
  ),
  model = llmModel,
  max_tokens = 512, # Controls the maximum number of tokens in the response
  temperature = 0.7  # Controls the creativity/randomness of the response
)
response <- POST(
  urlAPI,
  body = jsonlite::toJSON(body, auto_unbox = TRUE),
  content_type_json(),
  accept_json()
)
content(response)
httr::status_code(response)
stop_for_status(response)
