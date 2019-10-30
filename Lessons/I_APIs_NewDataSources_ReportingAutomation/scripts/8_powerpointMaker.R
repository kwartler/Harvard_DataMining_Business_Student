#' Author: Ted Kwartler
#' Data: 10-30-19
#' Purpose: Build a powerpoint easily

# libraries
library(officer)
library(ggplot2)
library(flextable)

# 2 decimal places
options(digits=2)

# set wd
setwd("/cloud/project/Lessons/I_APIs_NewDataSources_ReportingAutomation/data")

# Identify the file in the directory
searchPattern <- 'Wk2'
pth <- list.files(pattern = searchPattern, 
                  full.names = T)
pth <- pth[grep('*.csv', pth)]
df  <- read.csv(pth)

# All ppt inputs
author <- 'Dale the DataScientist'
theme  <- '1_Office Theme'

# Create an empty powerpoint
pptx <- read_pptx('exampleTemplate.pptx')

# Let's start with a title slide
titleTxt <- paste(Sys.Date(), 'Patient Review')
pptx     <- add_slide(pptx, 
                      layout = "Title Slide", 
                      master = theme)

pptx <- ph_with_text(pptx, type = "ctrTitle", str = titleTxt ) 
pptx <- ph_with_text(pptx, type = "subTitle",  str = author)

# Add another slide "Two Content"
pptx <- add_slide(pptx, 
                  layout = "Two Content", 
                  master = theme)
# Add a slide title
pptx <- ph_with_text(pptx, 
                     type = "title", 
                     str  = 'Averages & Distributions' )

# Make a simple visual and add it to the first body section
pl   <- ggplot(data = df) + geom_histogram(aes(age))

# Examine the plot
pl

pptx <- ph_with_gg(pptx, value = pl, index = 1)

# 2nd box add table of info
avgLabs <- mean(df$num_lab_procedures)
avgAge  <- mean(df$age) 
females <- as.matrix(table(df$gender))[1,1]
males   <- as.matrix(table(df$gender))[2,1]

highlevelDF <- data.frame(AVGlabs = avgLabs,
                          AVGage  = avgAge,
                          Females = females,
                          Males   = males)

highlevelDF

# Basic table
#pptx <- ph_with_table(pptx,
#                      type = "body", 
#                      value = highlevelDF,
#                      index = 2)

# Flextable
ft   <- flextable(data = highlevelDF)
pptx <- ph_with_flextable(pptx, value = ft, type = "body", index = 2)

# Add another slide
pptx <- add_slide(pptx, 
                  layout = "Comparison", 
                  master = theme)
# Add the title
pptx <- ph_with_text(pptx, 
                     type = "title", 
                     str = 'Readmissions Info' )

# Left side sub title
pptx <- ph_with_text(pptx, 
                     type  = "body", 
                     str   = 'Readmissions in last 30days',
                     index = 1)

# Right side sub title
pptx <- ph_with_text(pptx, 
                     type  = "body", 
                     str   = 'Time in Hospital',
                     index = 5)

# Left side content
jpeg('tmpTimeInHospital.jpg')
hist(df$time_in_hospital)
dev.off()
pptx <- ph_with_img(pptx, 
                    type   = "body",
                    src    = 'tmpTimeInHospital.jpg',
                    height = 4.5,
                    width  = 4,
                    index  = 4)

# Right side content
jpeg('tmpReadmissions.jpg')
barplot(table(df$readmitted))
dev.off()
pptx <- ph_with_img(pptx, 
                    type   = "body",
                    src    = 'tmpReadmissions.jpg',
                    height = 4.5,
                    width  = 4,
                    index  = 2)

fileName <- paste0('diabetesReadout_',searchPattern,'.pptx')
print(pptx, target = fileName)
# End