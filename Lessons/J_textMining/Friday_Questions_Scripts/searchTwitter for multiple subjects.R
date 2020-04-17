#' TK
#' 10-1
#' NBA Team Twitter Mention Data Collection
#'

# lib
print(paste('started:', Sys.time()))
library(twitteR)
#https://developer.twitter.com/en

# Inputs
consumerKey        <- "XXXXXXXXXXXXXXXXXX"
consumerSecret     <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
accessToken        <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
accessTokenSecret  <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

# Teams
nbaTeams <- c('Atlanta Hawks',
              'Boston Celtics',
              'Brooklyn Nets',
              'Charlotte Hornets',
              'Chicago Bulls',
              'Cleveland Cavaliers',
              'Dallas Mavericks',
              'Denver Nuggets',
              'Detroit Pistons',
              'Golden State Warriors',
              'Houston Rockets',
              'Indiana Pacers',
              'LA Clippers',
              'LA Lakers',
              'Memphis Grizzlies',
              'Miami Heat',
              'Milwaukee Bucks',
              'Minnesota Timberwolves',
              'New Orleans Pelicans',
              'New York Knicks',
              'Oklahoma City Thunder',
              'Orlando Magic',
              'Philadelphia Sixers',
              'Phoenix Suns',
              'Portland Trail Blazers',
              'Sacramento Kings',
              'San Antonio Spurs', 
              'Toronto Raptors',
              'Utah Jazz',
              'Washington Wizards')
# Auth
setup_twitter_oauth(consumerKey,consumerSecret, accessToken, accessTokenSecret)

# Get tweets by search term (can use userTimeline)
allTeams <- list()
for (i in 1:length(nbaTeams)){
  print(nbaTeams[i])
  # Get
  subjectA <- searchTwitter(nbaTeams[i], n=1000, lang = 'en')
  # Organize
  subjectA      <- twListToDF(subjectA)
  subjectA$team <- nbaTeams[i]
  # List
  allTeams[[nbaTeams[i]]] <- subjectA
  
}

allTweets <- do.call(rbind, allTeams)
write.csv(allTweets, paste0('~/nbaTweetData/',Sys.Date(),'_NBATweets.csv'), row.names = F)
print(paste('completed:', Sys.time()))
# End