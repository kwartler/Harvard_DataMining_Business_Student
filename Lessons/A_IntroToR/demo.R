
# Lecture 1 - demo 
# 
# Work on your homework in an R file like this,
# but you need to submit the Rmd and HTML files (instead of this R file).


# What is the name of this course?
# Answer:

2*8


# In the lower right pane, move to the directory (folder) 
# containing the file ToyotaCorolla.csv .

# Click "More" and select "Set As Working Directory", then
# the command setwd() will be executed and shown in Console (lower left pane).

# Copy the whole command and paste it to here (upper left pane)
# setwd()

# Execute the following
df <- read.csv('ToyotaCorolla.csv')

dim(df)

head(df)

pairs(df[c('Price','KM')])
?pairs