library(httr)

headers = c(
  `Content-Type` = "application/json"
)

data = '{ \n  "messages": [ \n    { "role": "system", "content": "You are a product owner writing a functional specification.  You write detailed specifications but no code." },\n    { "role": "user", "content": "Write a functional specification for an R function that accepts a number and divides it by Euler\'s number." }\n  ], \n  "temperature": 0.7, \n  "max_tokens": -1,\n  "stream": false\n}'

res <- httr::POST(url = "http://localhost:1234/v1/chat/completions", httr::add_headers(.headers=headers), body = data)

functionalSpec <- httr::content(res)$choices[[1]]$message$content
cat(functionalSpec)


# Data
LLMdata <- list(
  messages = list(
    list(role = "system", content = "You are an expert R programmer.  You are recieving a functional specification from the product owner.  You will write R code to accomplish the functional specification."),
    list(role = "user", content = functionalSpec)
  ),
  temperature = 0.7,
  max_tokens = 512,
  stream = FALSE
)


# Convert list to JSON
jsonData <- jsonlite::toJSON(LLMdata, auto_unbox = TRUE)
res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = jsonData)
llmCoder <- httr::content(res)$choices[[1]]$message$content
cat(llmCoder)

# Quality Control
LLMqc <- list(
  messages = list(
    list(role = "system", content = "You are an expert R programmer with code quality and writing tests.  Write a functional test for the R code below."),
    list(role = "user", content = llmCoder)
  ),
  temperature = 0.7,
  max_tokens = 512,
  stream = FALSE
)
jsonData <- jsonlite::toJSON(LLMqc, auto_unbox = TRUE)
res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = jsonData)
llmQC <- httr::content(res)$choices[[1]]$message$content
cat(llmQC)

# Documentation
LLMdoc <- list(
  messages = list(
    list(role = "system", content = "You are technical writer.  Write a technical documentation for this function and its functional unit tests.  Describe the function technically with outside information and be specific."),
    list(role = "user", content = paste(llmCoder, llmQC, sep ='\n'))
  ),
  temperature = 0.7,
  max_tokens = 512*2,
  stream = FALSE
)
jsonData <- jsonlite::toJSON(LLMdoc, auto_unbox = TRUE)
res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = jsonData)
llmDOC <- httr::content(res)$choices[[1]]$message$content
cat(llmDOC)

# Vs 0-shot
data = '{ \n  "messages": [ \n    { "role": "system", "content": "You are an expert R programmer." },\n    { "role": "user", "content": "Write a functional specification for this function: Write a functional specification for an R function that accepts a number and divides it by the Eulers number. Then write the function.  Then write functional tests to test the function. Lastly write a technical documentation for this function and its functional unit tests.  Describe the function technically with outside information and be specific.  After the technical documentation append the function and functional tests." }\n  ], \n  "temperature": 0.7, \n  "max_tokens": -1,\n  "stream": false\n}'

res <- httr::POST(url = "http://localhost:1234/v1/chat/completions", httr::add_headers(.headers=headers), body = data)

zeroShotLLM <- httr::content(res)$choices[[1]]$message$content
cat(zeroShotLLM)

