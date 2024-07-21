#' TK
#' Apr 28
#' An example for students using Ollama locally.
#'

# Libraries
library(httr)
library(jsonlite)

# Inputs
prompt <- "What is the capital of Brazil?" 

# API call inputs
headers <- c(`Content-Type` = "application/json") 
dataLLM    <- list(model = "gemma:2b", # Be sure to change to the model name you're using
                prompt = prompt)

# API Request
res <- httr::POST(
  url = "http://localhost:11434/api/generate", 
  httr::add_headers(.headers=headers), 
  body = jsonlite::toJSON(dataLLM, auto_unbox = TRUE), 
  encode = "json")

# Parse the streaming in JSON
llmResponse <- httr::content(res,as = "text", encoding = "UTF-8")
llmResponse <- strsplit(llmResponse, "\n")[[1]]
llmResponse <- lapply(llmResponse, fromJSON)
llmResponse <- paste(unlist(lapply(llmResponse, '[', 'response')), collapse = '')
llmResponse

# End
