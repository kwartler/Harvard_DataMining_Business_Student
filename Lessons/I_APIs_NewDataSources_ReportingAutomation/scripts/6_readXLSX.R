#' Author: Ted Kwartler
#' Data: 11-11-18
#' Purpose: Working with new file types

# Libraries
library(readxl)

# wd
setwd("/cloud/project/Lessons/I_APIs_NewDataSources_ReportingAutomation/data")

# File Path
filePath <- 'exampleExcel.xlsx'

# Identify sheets
numSheets <- excel_sheets(filePath)
numSheets

# Get individual sheets
sheetOne   <- read_excel(filePath, 1)
#' Author: Ted Kwartler
#' Data: 11-11-18
#' Purpose: Working with new file types

# Libraries
library(readxl)

# wd
setwd("/cloud/project/lessons/9_Apr3_APIs_ReportingAutomation/wk9_data")

# File Path
filePath <- 'exampleExcel.xlsx'

# Identify sheets
numSheets <- excel_sheets(filePath)
numSheets

# Get individual sheets; remove to save memory in demo
sheetOne   <- read_excel(filePath, 1)
rm(list = 'sheetOne')

sheetTwo   <- read_excel(filePath, 2)
rm(list = 'sheetTwo')

sheetThree <- read_excel(filePath, 3)
head(sheetThree)
rm(list = 'sheetThree')

# End