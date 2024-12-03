#' Author: Ted Kwartler
#' May 5
#' An Agentic Workflow example
#' Specialized Tasks

# Libraries
library(httr)
library(jsonlite)

# Input
llmModel <- 'meta-llama-3.1-8b-instruct'
headers <- c(`Content-Type` = "application/json")

# Agents
productOwnerSystem   <- "You are a product owner writing a functional specification.  You write detailed specifications but no code."
productOwnerTask <-  "Write a functional specification for an R function that accepts a number and divides it by Euler\'s number."

programmerSystem <- 'You are an expert R programmer.  You are recieving a functional specification from the product owner.  You will write R code to accomplish the functional specification.'

codeQCSystem <- 'You are an expert R programmer with code quality and writing tests.  Write a functional test for the R code below.'

technicalWriterSystem <- "You are technical writer.  Write a technical documentation for this function and its functional unit tests.  Describe the function technically with outside information and be specific."


# Product Owner Agent
prodOwner <- list(model = llmModel,
                messages = list(
                  list(role = "system", content = productOwnerSystem),
                  list(role = "user", content = productOwnerTask)),
                temperature = 0.7,
                max_tokens = 512,
                stream = FALSE)

# PO Functional Spec Creation
POres <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(prodOwner, auto_unbox = TRUE))
functionalSpec <- httr::content(POres)$choices[[1]]$message$content

# Programmer Agent
programmeR <- LLMdata <- list(
  messages = list(
    list(role = "system", content = programmerSystem),
    list(role = "user", content = functionalSpec)
  ),
  temperature = 0.7,
  max_tokens = 512,
  stream = FALSE
)

# Programmer code
progRes <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                    httr::add_headers(.headers = headers),
                    body = toJSON(programmeR, auto_unbox = TRUE))
# Code Creation
Rcode <- httr::content(progRes)$choices[[1]]$message$content

# Quality Control
LLMqc <- list(
  messages = list(
    list(role = "system", content = codeQCSystem),
    list(role = "user", content = Rcode)
  ),
  temperature = 0.7,
  max_tokens = 512,
  stream = FALSE
)

# QC
qcRes <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                      httr::add_headers(.headers = headers),
                      body = toJSON(LLMqc, auto_unbox = TRUE))

# QC Test
qcCode <- httr::content(qcRes)$choices[[1]]$message$content

# Team Responses 
teamEffort <- paste(c(functionalSpec, Rcode, qcCode), collapse ='\\nn')

# technical documentation
LLMdocs <- list(
  messages = list(
    list(role = "system", content = technicalWriterSystem),
    list(role = "user", content = teamEffort)
  ),
  temperature = 0.7,
  max_tokens = 512,
  stream = FALSE
)

# PO Functional Spec Creation
techDoc<- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                    httr::add_headers(.headers = headers),
                    body = toJSON(LLMdocs, auto_unbox = TRUE))
techDoc <- httr::content(techDoc)$choices[[1]]$message$content

# Final output of all agents
sapply(c(functionalSpec, Rcode, qcCode, techDoc), cat)

# End
