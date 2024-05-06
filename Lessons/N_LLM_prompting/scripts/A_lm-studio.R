#' Author: Ted Kwartler
#' Apr 28
#' An example for students using lm-studio locally.

# Libraries
library(httr)
library(jsonlite)

# Inputs
prompt   <- "What is the capital of Brazil?" 
llmModel <- 'lmstudio-ai/gemma-2b-it-GGUF'

# Organize Request
dataLLM <- list(model = llmModel,
               messages = list(
                 list(role = "system", content = "You are a helpful, smart, kind, and efficient AI assistant. You always fulfill the user's requests to the best of your ability."),
                 list(role = "user", content = prompt)),
               temperature = 0.7,
               max_tokens = 512,
               stream = FALSE)
# Request header
headers <- c(`Content-Type` = "application/json")

# Make the POST request
res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the response
llmResponse <- httr::content(res)$choices[[1]]$message$content
cat(llmResponse)

# End