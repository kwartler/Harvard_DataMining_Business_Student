#' Author: Ted Kwartler
#' Sept 22, 2024
#' Example API Inteface with OpenAI services
# Model Info: https://platform.openai.com/docs/models/gpt-4o
# gpt-4o-mini: small task including vision
# o1-preview: reasoning model designed to solve hard problems across domains
# o1-mini: faster and cheaper reasoning model particularly good at coding,  math, and science.

# Your API Key
OPEN_AI_KEY <- "sk-XXXXXXXXXXXXXXX"

# Load the required libraries
library(httr)
library(jsonlite)

# Define headers
headers <- c(
  `Content-Type` = "application/json",
  Authorization = paste0("Bearer ", OPEN_AI_KEY)
)

# Define model
openAImodel <- "gpt-4o"
userPrompt <- 'Using the R language create a simple guess the number game.'

# Define messages
messages <- list(
  list(role = "system", content = "You are a helpful assistant."),
  list(role = "user", content = userPrompt)
)

# Create data as a list
LLMdata <- list(
  model = openAImodel,
  messages = messages
)

# Convert data to JSON
jsonData <- jsonlite::toJSON(LLMdata, auto_unbox = TRUE)

# Send the POST request
res <- httr::POST(url = "https://api.openai.com/v1/chat/completions", 
                  httr::add_headers(.headers=headers), 
                  body = jsonData)
# Print the response
print(httr::content(res))

# Show response in console
cat(httr::content(res)$choices[[1]]$message$content)

