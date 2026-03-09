
#' @param df dataframe with the following characteristics column one name is "x" consisting of string variable names. Column 2 name is "beta" with corresponding beta coefficient values.  Column 3 is called "type" denoting each variable type as "intercept", "Boolean", "numeric", "character", "dummy".  Finally the data.frame paramater "stringsAsFactors" must be set to FALSE.   See example for clarification.
#' @param fileName character string ending in .html ie. "myForm.html"; NULL will create a default name.
renderLogisticForm <- function(df, backgroundColor = 'black', fontColor = 'white', 
                       submitButtonColor = 'blue', 
                       submitButtonFontColor = 'white', 
                       font = 'arial',
                       fileName = NULL) {
  
  
  # Libs
  library(htmltools)
  library(stringr)
  
  # Remove non-alphanumeric symbols to create valid JavaScript variable names
  df$x <- gsub("\\W", "", df$x)
  
  # Initialize intercept.
  intercept <- 0
  
  idx_intercept <- which(df$type == "intercept")
  idx_boolean <- which(df$type %in% c("Boolean", "dummy"))
  idx_numeric <- which(df$type == "numeric")
  idx_character <- which(df$type == "character")
  
  # Process intercepts.
  if (length(idx_intercept) != 0) {
    intercept <- sum(df$beta[idx_intercept])
  }
  
  jsfunc <- sprintf("function calculate() {\nlet result = %.2f;\n", intercept)
  
  for (i in idx_boolean) {
    jsfunc <- paste0(jsfunc, sprintf("if (document.getElementById('%s').checked) { result += %.2f; }\n", df$x[i], df$beta[i]))
  }
  
  for (i in idx_numeric) {
    jsfunc <- paste0(jsfunc, sprintf("result += document.getElementById('%s').value * %.2f;\n", df$x[i], df$beta[i]))
  }
  
  if (length(idx_character) > 0) {
    warning(sprintf("Ignoring character inputs for: %s", paste(df$x[idx_character], collapse = ", ")))
  }
  
  jsfunc <- paste0(jsfunc,
                   "result = 1 / (1 + Math.exp(-result));\n", 
                   "document.getElementById('result').innerHTML = 'Result: ' + result;\n",
                   "}")
  
  # Define CSS styles
  cssStyles <- paste(
    "body {background-color: ", backgroundColor,
    "; color: ", fontColor,
    "; font-family: ", font, ";}",
    "input[type='button'] {background-color: ", submitButtonColor, 
    "; color: ", submitButtonFontColor, ";}"
  )
  
  htmlBody <- tags$body(
    tags$style(HTML(cssStyles)),
    tags$div(id = "intercept", tags$p(sprintf("Intercept: %.2f", intercept))),
    tags$form(id = "myForm")
  )
  
  for(i in 1:nrow(df)) {
    if(df$type[i] == "Boolean" | df$type[i] == "dummy") {
      htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, 
                                                        tags$div(tags$label(df$x[i]), tags$input(type = "checkbox", id = df$x[i], name = df$x[i])))
    } else if(df$type[i] == "numeric") {
      htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, 
                                                        tags$div(tags$label(df$x[i]), tags$input(type = "number", step = "0.01", id = df$x[i], name = df$x[i], value = "0")))
    }
  }
  
  htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, tags$input(type = "button", value = "Submit", onclick = "calculate()"))
  
  htmlBody <- tagAppendChild(htmlBody, tags$div(id = "result"))
  htmlBody <- tagAppendChild(htmlBody, tags$script(HTML(jsfunc, "\n")))
  
  if(is.null(fileName)){
    warning('no fileName added, using default')
    fileName <- paste0(Sys.Date(), '_protypeLogForm.html')
  }
  save_html(htmlBody, fileName)
}



# fonts can be arial, arial black, helvetics, verdana, tahoma, trebuchet MS, Georgia, Palatino, Times New Roman, Times, Courier
