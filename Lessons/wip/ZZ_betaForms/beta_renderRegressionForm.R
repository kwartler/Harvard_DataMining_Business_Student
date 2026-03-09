#' @param df dataframe with the following characteristics column one name is "x" consisting of string variable names. Column 2 name is "beta" with corresponding beta coefficient values.  Column 3 is called "type" denoting each variable type as "intercept", "Boolean", "numeric", "character", "dummy".  Finally the data.frame paramater "stringsAsFactors" must be set to FALSE.   See example for clarification.
#' @param fileName character string ending in .html ie. "myForm.html"; NULL will create a default name.

#' @example
#' # Example DF data
#'  df <- data.frame(x = c("(Intercept)", "groupTrt", "variable3", "variable4", "variable5"),
#'                   beta = c(5.032, -0.371, 1.513, 2.145, 0.5),
#'                   type = c("intercept", "Boolean", "numeric", "character", "dummy"),
#'                   stringsAsFactors = FALSE)
#'                   renderRegressionForm(df, 'example.html')

renderRegressionForm <- function(df, fileName = NULL) {
  
  # Add libraries
  library(htmltools)
  library(stringr)
  
  df$x <- gsub("\\W", "", df$x) # Remove non-alphanumeric symbols to create valid JavaScript variable names
  
  # Initialize intercept.
  intercept <- 0
  
  # Get the indices of df's rows for each type of input
  idx_intercept <- which(df$type == "intercept")
  idx_boolean <- which(df$type %in% c("Boolean", "dummy"))
  idx_numeric <- which(df$type == "numeric")
  idx_character <- which(df$type == "character")
  
  # Process intercepts.
  if (length(idx_intercept) != 0) {
    intercept <- sum(df$beta[idx_intercept])
  }
  
  # Initialize the JavaScript function.
  jsfunc <- sprintf("function calculate() {\nlet result = %.2f;\n", intercept)
  
  # Process booleans and dummies.
  for (i in idx_boolean) {
    jsfunc <- paste0(jsfunc, sprintf("if (document.getElementById('%s').checked) { result += %.2f; }\n", df$x[i], df$beta[i]))
  }
  
  # Process numerics.
  for (i in idx_numeric) {
    jsfunc <- paste0(jsfunc, sprintf("result += document.getElementById('%s').value * %.2f;\n", df$x[i], df$beta[i]))
  }
  
  # Warning for character vectors.
  if (length(idx_character) > 0) {
    warning(sprintf("Ignoring character inputs for: %s", paste(df$x[idx_character], collapse = ", ")))
  }
  
  # Finish Javascript function
  jsfunc <- paste0(jsfunc, "document.getElementById('result').innerHTML = 'Result: ' + result;\n", "}")
  
  # Start HTML body with Intercept and Form
  htmlBody <- tags$body(
    tags$div(id = "intercept", tags$p(sprintf("Intercept: %.2f", intercept))),
    tags$form(id = "myForm")
  )
  
  # Add input mechanism according to type
  for(i in 1:nrow(df)) {
    if(df$type[i] == "Boolean" | df$type[i] == "dummy") {
      htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, 
                                                        tags$div(tags$label(df$x[i]), tags$input(type = "checkbox", id = df$x[i], name = df$x[i])))
    } else if(df$type[i] == "numeric") {
      htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, 
                                                        tags$div(tags$label(df$x[i]), tags$input(type = "number", step = "0.01", id = df$x[i], name = df$x[i], value = "0")))
    }
  }
  
  # Add Submit button
  htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, tags$input(type = "button", value = "Submit", onclick = "calculate()"))
  
  # Add result div and script
  htmlBody <- tagAppendChild(htmlBody, tags$div(id = "result"))
  htmlBody <- tagAppendChild(htmlBody, tags$script(HTML(jsfunc, "\n")))
  
  if(is.null(fileName)){
    warning('no fileName added, using default')
    fileName <- paste0(Sys.Date(), '_protypeRegForm.html')
  }
  save_html(htmlBody, fileName)
}

