#' Author: Ted Kwartler
#' Data: 5-3-2020
#' Purpose: Susceptible Infected Removed SIR modeling
#' Some additional resources
#'https://alhill.shinyapps.io/COVID19seir/
#'http://www.public.asu.edu/~hnesse/classes/sir.html
#'https://www.who.int/bulletin/online_first/20-255158.pdf
#'https://www.who.int/bulletin/online_first/20-255695.pdf
#'https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(20)30074-7/fulltext
#'https://arxiv.org/pdf/2002.06563.pdf
#'
#'https://www.r-bloggers.com/sir-model-with-desolve-ggplot2/
#'https://www.r-bloggers.com/covid-19-in-belgium/
#'http://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html
#'https://rpubs.com/choisy/sir

# libs
library(EpiDynamics)

# options
options(scipen = 999)

# Starting inputs
susceptible <- .999999
infected    <- .000001
removed     <- 1-susceptible-infected #sometimes Recovered
totalDays   <- 70

# Virus Behavior
betaVal  <- 1.4247 #infection rate; how many people are infected for each person
gammaVal <- 0.14286 #recovery rate; rate an infected recovers & moves to removed

# Organize into named vectors
initials   <- c(S = susceptible, 
                I = infected, 
                R = removed)
parameters <- c(beta = betaVal, 
                gamma = gammaVal)

# Solve and plot.
sir <- SIR(pars = parameters, init = initials, time = 0:totalDays)

# Examine
head(sir$results)

# Plot the curves
plot(sir$results$S, type='l')
lines(sir$results$I, col='red')
lines(sir$results$R, col='blue')
#PlotMods(sir)

# Using Covid infection & recovery from papers
betaVal  <- 1.75
gammaVal <- .5 #R0 = beta/gamma; 3.5 = 1.75/gamma;3.5g =1.75
# China recovered minus death div infected =(47367-2945)/80151  

sirC <- SIR(pars = c(beta = betaVal, gamma = gammaVal), 
           init = initials, time = 0:totalDays)
# Plot the curves
plot(sirC$results$S, type = 'l')
lines(sirC$results$I, col='red')
lines(sirC$results$R, col='blue')

# Let's apply some social distancing to decrease the infection rate
betaVal  <- 1.1
gammaVal <- .5 
sirD <- SIR(pars = c(beta = betaVal, gamma = gammaVal), 
           init = initials, time = 0:totalDays)
PlotMods(sirD)

# Plot the infection curves next to each other
plot(sirC$results$I, col='red', type ='l')
lines(sirD$results$I, col='red')

# End