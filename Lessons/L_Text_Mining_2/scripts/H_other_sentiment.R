#' Purpose: Other sentiment libraries
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: Apr 28, 2025
#'

# Libs
library(sentimentr)
library(lexicon) #lexicon::available_data()
library(dplyr)
library(SentimentAnalysis)
library(ggplot2)


# Read in data
txt <- read.csv("https://github.com/kwartler/Harvard_DataMining_Business_Student/raw/refs/heads/master/Lessons/L_Text_Mining_2/data/new_orleans_airbnb_listings.csv")

# Explore
head(txt)
summary(txt)
barplot(table(txt$neighbourhood_cleansed), las = 2)

# this is a non-qdap wrapper for sentiment from sentimentr, with come minor differences around dictionary and negations
?sentiment
sentResults <- sentiment(text.var = get_sentences(txt$description),
                         polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                         valence_shifters_dt = lexicon::hash_valence_shifters,
                         amplifier.weight = 0.8, n.before = 5, n.after = 2)
head(sentResults)

# Let's aggregate by word count
totalWords <- aggregate(word_count ~ element_id, sentResults, sum)

# Since sentiment is adjusted for doc & sentiment length simple sum should be fine
totalSent <- aggregate(sentiment ~ element_id, sentResults, sum)

results <- left_join(totalWords, totalSent, by = join_by(element_id))
head(results)

# sometimes author effort and conviction can be seen with longer documents and the result is a "barbell" i.e. super bad and super positive reviews are often longer w/determined authors
plot(results$word_count, results$sentiment)
cor(results$word_count, results$sentiment, use = 'complete.obs')

# What about by a grouping variable like author or meta information
sentGrp <- sentiment_by(text.var = get_sentences(txt$description),
                        by = txt$neighbourhood_cleansed,
                        polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                        valence_shifters_dt = lexicon::hash_valence_shifters,
                        amplifier.weight = 0.8, n.before = 5, n.after = 2)
head(sentGrp)

# Let's make a cleveland dot plot of the top 15 neighborhoods
ggplot(sentGrp[1:15,], aes(x = word_count, y = reorder(neighbourhood_cleansed, word_count))) +
  geom_segment(aes(yend = reorder(neighbourhood_cleansed, ave_sentiment)),
               xend = 0, colour = "darkgrey") +
  geom_point(aes(x = word_count, y = reorder(neighbourhood_cleansed, ave_sentiment), size = ave_sentiment)) +
  theme_bw()


#### Takes a long time; so save a copy of results
# Now let's try SentimentAnalysis, which can also accept a corpus directly, again some calc differences
# This function has an aggregate parameter but its unclear whether it does it by factor order thus
nReviews <- 100
multipleMethods <- analyzeSentiment(txt$description[1:nReviews],
                                    language = "english",
                                    aggregate = NULL,
                                    removeStopwords = TRUE,
                                    stemming = TRUE)
#saveRDS(multipleMethods,
#        '~/Desktop/ICPSR/personalFiles/multipleMethods.rds')
#multipleMethods <- readRDS('~/Desktop/ICPSR/personalFiles/multipleMethods.rds')
head(multipleMethods)

# Append the grp - we are only doing a few for expediency
results <- cbind(multipleMethods, neighbourhood_cleansed = txt$neighbourhood_cleansed[1:nReviews])
head(results)
# Depending on the method you may want to aggregate up now
neighborhoodResults <- results %>%
  group_by(neighbourhood_cleansed) %>%
  summarise_at(vars(SentimentLM), list(name = mean)) %>% as.data.frame()
head(neighborhoodResults[order(neighborhoodResults$name, decreasing = T),],15)

# End
