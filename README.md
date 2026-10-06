<!-- AI assisted with Claude -->
# Harvard_DataMining_Business_Student
For students of Harvard CSCI E-96 Data Mining for Business.

**Next offering: Spring 2027.**

## Syllabus

The Spring 2027 syllabus will be linked here once Harvard Extension publishes it. The official syllabus always has the most up to date topics, dates and grading details.

Earlier offerings, for reference only:

* Spring 2026: [syllabus](https://harvard.simplesyllabus.com/doc/67luwcix3/Spring-Term-2026-Full-Term-CSCI-E-96-1-Data-Mining-for-Business?mode=view) or [PDF here](https://harvard.simplesyllabus.com/api2/doc-pdf/67luwcix3/Spring-Term-2026-Full-Term-CSCI-E-96-1-Data-Mining-for-Business.pdf?locale=en-US). A copy of the PDF is also in this repo.
* Fall 2024: [syllabus](https://harvard.simplesyllabus.com/doc/k6cprbiwb/Fall-Term-2024-Full-Term-CSCI-E-96-1-Data-Mining-for-Business?mode=view) or [PDF here](https://harvard.simplesyllabus.com/api2/doc-pdf/k6cprbiwb/Fall-Term-2024-Full-Term-CSCI-E-96-1-Data-Mining-for-Business.pdf?locale=en-US)
* Spring 2025: [syllabus](https://harvard.simplesyllabus.com/doc/cs6yu9p2b/Spring-Term-2025-Full-Term-CSCI-E-96-1-Data-Mining-for-Business?mode=view) or [PDF here](https://harvard.simplesyllabus.com/api2/doc-pdf/cs6yu9p2b/Spring-Term-2025-Full-Term-CSCI-E-96-1-Data-Mining-for-Business.pdf?locale=en-US)

## How this repo is organized

| Folder | What is in it |
|--------|---------------|
| `Lessons/` | One folder per class session (A through N). Each has slides, R scripts, data, and usually a `challenge!` folder. |
| `HW/` | Homework starter files. |
| `Cases/` | Case studies (Word documents) and their data. |
| `EthicsArticles/` | Readings for the responsible AI and technology ethics sessions. |
| `BookDataSets/` | Datasets that go with the course textbook. |

### Challenges and answer keys
Each `challenge!` folder holds a STUDENT script to work from. Some also include a KEY script. The challenges are ungraded practice, and the KEY files are there as a backup so you can check your work. Please attempt the challenge first, then compare.

### Data files and the `.DELETED.txt` placeholders
You will see many files ending in `.DELETED.txt`. Each one stands in for a data file that is no longer stored in this repo, which keeps the download small and keeps licensed data out of git. Open the placeholder to see where the file now lives. For most of them, that is the instructor's `teaching-datasets` repo, and you can read the data directly in R, for example:

```
dat <- read.csv(url("https://raw.githubusercontent.com/kwartler/teaching-datasets/main/fileName.csv"))
```

Replace `fileName.csv` with the file named in the placeholder. If a placeholder has no link, check the course site or ask in class.

Case data is for your individual work in this course and is not to be redistributed.

## Tools for the course

* **R and RStudio** for all data mining work.
* **GitHub** to get the course materials and keep your own work organized.
* **Generative AI tools**, including Gemini, Google AI Studio and NotebookLM, which we will use for prompting, coding help and working with documents. The first LLM session covers how to use them well.

## Working with R
If you are new to R, please take an online course to get familiar with it prior to the first session. We will still cover R basics, but students have been aided by spending a few hours taking a free online course at [YouTube](https://www.youtube.com/watch?v=eR-XRSKsuR4) or [DataCamp](https://www.datacamp.com). The code below should be run in the console to install the packages needed for the semester.

## Please install the following packages with this R code
If you encounter any errors, don't worry, we will find time to work through them. The `qdap` library is usually the trickiest because it requires Java and `rJava`, and it does not work on Mac. If you get any errors, try removing it from the code below and rerunning. This will take **a long time** if you don't already have the packages, so please run it prior to class, at a time you don't need your computer, such as *at night*.
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
               arules, fst, recommenderlab, reshape2,
               TTR, quantmod, htmltools,
               PerformanceAnalytics, rpart, data.table,
               pbapply, stringi, tm, qdap, readr,
               dendextend, wordcloud, RColorBrewer,
               tidytext, radarchart, RCurl, openNLP, xml2, stringr,
               devtools, flexdashboard, rmarkdown, httr)

```

## Class Schedule

This is tentative and subject to change to maximize learning. The course syllabus has the exact dates and the most up to date topics. Holiday weeks (Presidents' Day and Spring Break) have no class.

### Spring 2027

| Session | Topic |
|---------|-------|
| 1  | Intro to R, RStudio and git |
| 2  | Intro to Data Mining |
| 3  | LLM basics and prompting |
| 4  | More R practice and EDA |
| 5  | Data mining workflows |
| 6  | Regression and logistic regression |
| 7  | Decision trees and random forests |
| 8  | Time series data |
| 9  | Equities |
| 10 | Predicting risk and non-traditional investing |
| 11 | Text analysis and NLP |
| 12 | Text analysis and NLP, continued |
| 13 | APIs, novel and advanced LLM workflows |
| 14 | Responsible AI and technology ethics |
