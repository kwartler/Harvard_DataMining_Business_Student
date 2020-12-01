#' Author: Ted Kwartler
#' Date: 11-30-2010
#' Purpose: Working with new file types
#' 

# libraries
library(docxtractr)
library(xml2)

# wd
setwd("/cloud/project/Lessons/L_DataSources_Reporting/data")

# file
filePath <- 'exampleWord.docx'

# Easiest way to get the table data
info      <- docxtractr::read_docx(filePath)
tableData <- docx_extract_all_tbls(info)
tableData <- as.data.frame(tableData)
tableData

# Easiest way to extract the simple text as a single vector; messy!
library(textreadr)
info <- textreadr::read_docx(filePath)
info
# End
