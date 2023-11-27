# Harvard_DataMining_Business_Student
For students of Harvard CSCI E-96 Data Mining for Business

## Syllabus
Fall 2023 University [syllabus](https://harvard.simplesyllabus.com/doc/jff3zh7o0/Fall-Term-2023-Full-Term-CSCI-E-96-1-Data-Mining-for-Business?mode=view)
or [PDF here](https://harvard.simplesyllabus.com/api2/doc-pdf/jff3zh7o0/Fall-Term-2023-Full-Term-CSCI-E-96-1-Data-Mining-for-Business.pdf?locale=en-US)

Spring 2024 University [syllabus](https://harvard.simplesyllabus.com/doc/lb0nf6j56/Spring-Term-2024-Full-Term-CSCI-E-96-1-Data-Mining-for-Business?mode=view) or 
[PDF here](https://harvard.simplesyllabus.com/api2/doc-pdf/lb0nf6j56/Spring-Term-2024-Full-Term-CSCI-E-96-1-Data-Mining-for-Business.pdf?locale=en-US)

## Working with R
If you are new to R, please take an online course to get familarity prior to the first session.  We will still cover R basics but students have been aided by spending a few hours taking a free online course at [DataQuest](https://www.dataquest.io/) or [DataCamp](https://www.datacamp.com).  The code below should be run in the console to install packages needed for the semester.

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
