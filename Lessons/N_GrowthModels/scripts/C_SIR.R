#' Author: Ted Kwartler
#' Data: Dec 13-2021
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
susceptible <- .999999 #basically everyone is susceptible
infected    <- .000001 #basically no one is infected
removed     <- 1-susceptible-infected #sometimes Recovered, these are people that will not be reinfected again, i.e. who is left for the virus to infect
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
plot(sir$results$S, type='l') #at any given point in time who is left to infect
plot(sir$results$I, col='red', type ='l') #at any given point in time how many are infected
plot(sir$results$R, col='blue', type='l') #at any given point how many are recovered or died ie. removed state

# Another view
PlotMods(sir)

# Using Covid infection & recovery from papers as of May 2020
betaVal  <- 1.75 #10 infected people will infect 17.5 people "how much movement is going on" & "how virulent"
gammaVal <- .5 #R0 (Rate of recovery) = beta/gamma 
# A paper said China recovered minus death div infected =(47367-2945)/80151 cited in May21 = 0.5542289; 
## Gamma of 0.5 means 1/2 of an infected person is recovering each day
#R0 was said to be ~3.5 in papers and means
# beta/gamma = R0
# 1.75 / gamma = 3.5
# 1.75 = 3.5 * gamma
# 1.75 / 3.5 = gamma
# 0.5 = gamma

sirC <- SIR(pars = c(beta = betaVal, gamma = gammaVal), 
           init = initials, time = 0:totalDays)
# Plot the actual covid curves
PlotMods(sirC)

# Let's apply some social distancing to decrease the infection rate
betaVal  <- 1.1 #when a person is infected they will infect 1.1 more people instead of 1.75
gammaVal <- .5 #recovery is the same, but in reality new treatments improve this over time
sirD <- SIR(pars = c(beta = betaVal, gamma = gammaVal), 
           init = initials, time = 0:totalDays)
PlotMods(sirD)

# Plot the infection curves next to each other, the social distanced one is lower meaning hospitals aren't overwhelmed and more people will recover vs die in the removed state.
# Recovery line is stretched out on x-axis
plot(sirC$results$R, col='darkred', type ='l',  lwd = 5)
lines(sirD$results$R, col='darkgreen',  lwd = 5)

# Infected line is not as high, meaning the capacity for care is less strained
plot(sirC$results$I, col='darkred', type ='l',  lwd = 5)
lines(sirD$results$I, col='darkgreen',  lwd = 5)

# Conclusion
# Keep in mind this a toy example to show how these factors interact.
# Issues of personal behavior like distancing, masking and not traveling/going to gatherings, spreading in different locations (coastal, south, upper midwest basically starts the process over in three+ waves), uneven access to healthcare in rural vs urban and systemic issues related to race, income and access to healthcare all impact these curves. 

# End