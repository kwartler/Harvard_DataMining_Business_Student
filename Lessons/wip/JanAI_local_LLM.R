#' Author: Ted Kwartler
#' Apr 28
#' An example for students using lm-studio locally.

# Libraries
library(httr)
library(jsonlite)

# Inputs
prompt   <- "What is the captial of Brazil?" 
llmModel <- "llama3.2-1b-instruct"

urlAPI <- "http://127.0.0.1:1337/v1/chat/completions"

body <- list(
  messages = list(
    list(role = "system", content = "You are a helpful assistant."),
    list(role = "user", content = prompt)
  ),
  model = llmModel,
  max_tokens = 2048,
  stop = c("hello"),
  frequency_penalty = 0,
  presence_penalty = 0,
  temperature = 0.7,
  top_p = 0.95,
  stream = FALSE
)

response <- POST(
  urlAPI,
  body = jsonlite::toJSON(body, auto_unbox = TRUE),
  content_type_json(),
  accept_json()
)
content(response)
status(response)
stop_for_status(response)
