#' Extra TS data practice
#' Load and convert the CVS data to TS
#' Using TSD understand the impact of Covid on retail sales at CVS. Compare the shape of this revenue to NIKE and reflect on the impact of covid for the two companies.
#' Thinking of  naive methods, and visualising the TS select the best method among the 4, then make a prediction for the next 4 quarters.  

cvsDF <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/G_TimeSeries_Equities/data/CVS_quarterlyRev.csv')
head(cvsDF)
plot(cvsDF$Value, type = 'l')


#' See if you can understand the impact of TSA screenings due to coronavirus using TSD
#' Using HW make a prediction for the next days, evaluate the model using out of time sampling
#' Once satisfied with HW (or another from the book) rerun with all the data making a prediction for the next month and compare to the data here:
#' https://www.tsa.gov/coronavirus/passenger-throughput
#' 

tsaDF <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/G_TimeSeries_Equities/data/TSA_screened_passengers.csv')
head(tsaDF)
plot(tsaDF$TSAscreenings, type = 'l')

# Convert to a TS for the TSA_screenings, then partition with windo(), then model, evaluate, and predict!
# End