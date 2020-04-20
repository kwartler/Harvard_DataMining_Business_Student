#' Author: Ted Kwartler
#' Data: 11-11-18
#' Purpose: Working with new file types

# Libraries
library(readxl)

# wd
setwd("/Users/edward.kwartler/Documents/Harvard_DataMining_Business_Admin/lessons/K_DataSources_Reporting/data")

# File Path
filePath <- 'exampleExcel.xlsx'

# Identify sheets
numSheets <- excel_sheets(filePath)
numSheets

# Get individual sheets; remove to save memory in demo
sheetOne   <- read_excel(filePath, 1)
head(sheetOne)
rm(list = 'sheetOne')

sheetTwo   <- read_excel(filePath, 2)
head(sheetTwo)
rm(list = 'sheetTwo')

sheetThree <- read_excel(filePath, 3)
head(sheetThree)
rm(list = 'sheetThree')

# End