# Harvard_DataMining_Business_Student
For students of Harvard CSCI E-96 Data Mining for Business

## Syllabus
Fall 2024 University [syllabus](https://harvard.simplesyllabus.com/doc/k6cprbiwb/Fall-Term-2024-Full-Term-CSCI-E-96-1-Data-Mining-for-Business?mode=view)
or [PDF here](https://harvard.simplesyllabus.com/api2/doc-pdf/k6cprbiwb/Fall-Term-2024-Full-Term-CSCI-E-96-1-Data-Mining-for-Business.pdf?locale=en-US)

Spring 2024 University [syllabus](https://harvard.simplesyllabus.com/doc/cs6yu9p2b/Spring-Term-2025-Full-Term-CSCI-E-96-1-Data-Mining-for-Business?mode=view) or 
[PDF here](https://harvard.simplesyllabus.com/api2/doc-pdf/cs6yu9p2b/Spring-Term-2025-Full-Term-CSCI-E-96-1-Data-Mining-for-Business.pdf?locale=en-US)

## Working with R
If you are new to R, please take an online course to get familarity prior to the first session.  We will still cover R basics but students have been aided by spending a few hours taking a free online course at [Youtube](https://www.youtube.com/watch?v=eR-XRSKsuR4) or [DataCamp](https://www.datacamp.com).  The code below should be run in the console to install packages needed for the semester.

## Please install the following packages with this R code.
If you encounter any errors don't worry we will find time to work through them.  The `qdap` library is usually the trickiest because it requires Java and `rJava` and does not work on Mac.  So if you get any errors, try removing that from the code below and rerunning.  This will take **a long time** if you don't already have the packages, so please run prior to class, and at a time you don't need your computer ie *at night*.
```
# Individually you can use 
# install.packages('packageName') such as below:
install.packages('ggplot2')

# or 
install.packages('pacman')
pacman::p_load(ggplot2, ggthemes, ggdark, rbokeh, maps, 
               ggmap, leaflet, radiant.data, DataExplorer,
               vtreat, dplyr, ModelMetrics, pROC,
               MLmetrics, caret, e1071, plyr, 
               rpart.plot, randomForest, forecast, dygraphs,
               lubridate, jsonlite, tseries, ggseas,
               arules,fst, recommenderlab,reshape2,
               TTR,quantmod, htmltools,
               PerformanceAnalytics,rpart, data.table,
               pbapply, rbokeh, stringi, tm, qdap, readr,
               dendextend, wordcloud, RColorBrewer,
               tidytext, radarchart, RCurl, openNLP, xml2, stringr,
               devtools, flexdashboard, rmarkdown, httr)

```

## Class Schedule 

This is tentative and subject to change to maximize learning

### Fall Semester

|Date     | Topic  |
|---------|------------|
| Sept 9  | Intro to R, R-studio & git  |
| Sept 16  | Intro to Data Mining   |
| Sept 23   | More R Practice & EDA  |
| Sept 30  | Data mining workflows  |
| Oct 7   | Regression & Log Regression  |
| Oct 14  | no class (university holiday)  |
| Oct 21   | Decision Tree & Random Forest  |
| Oct 28  | Time Series Data  |
| Nov 4  | Equities  |
| Nov 11   | Predicting Risk & non-traditional investing  |
| Nov 18   | Text analysis & NLP  |
| Nov 25  | Text analysis & NLP, continued  |
| Dec 2  | Chat GPT basics  |
| Dec 9  | Responsible AI & tech ethics  |
| Dec 16   | Optional Class Lab  |

### Spring Semester

|Date     | Topic  |
|---------|------------|
| Jan 27  | Intro to R, R-studio & git  |
| Feb 3   | Intro to Data Mining   |
| Feb 10  | Chat GPT basics  |
| Feb 17  | No class - President's Day  |
| Feb 24  | More R Practice & EDA |
| Mar 3   | Data mining workflows   |
| Mar 10  | Regression & Log Regression   |
| Mar 17  | No class - Spring Break   |
| Mar 24  | Decision Tree & Random Forest  |
| Mar 31  | Time Series Data  |
| Apr 7   | Equities  |
| Apr 14  | Predicting Risk & non-traditional investing |
| Apr 21  | Text analysis & NLP   |
| Apr 28  | Text analysis & NLP, continued  |
| May 5   | APIs, Novel & Advanced LLM workflows  |
| May 12  | Responsible AI & tech ethics  |