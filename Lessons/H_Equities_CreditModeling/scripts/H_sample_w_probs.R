#' Author: Ted Kwartler
#' Date: April 4, 2022
#' Purpose: See sample w/probability
#' 

idx <- 1:10

# Equal Probs
set.seed(123)
sample(idx,3)

# Example Probs
set.seed(123)
sample(idx, 3, replace = T, prob = c(1,0,0,0,0,0,0,0,0,0))


# Example Probs 2
set.seed(123)
sample(idx, 3, replace = T, prob = c(.5,.5,0,0,0,0,0,0,0,0))


# Example Probs 2
set.seed(123)
sample(idx, 3, replace = T, prob = c(.33,.33,0,0,0,0,.33,0,0,0))

set.seed(1234)
sample(idx, 3, replace = T, prob = c(.33,.33,0,0,0,0,.33,0,0,0))

# End
