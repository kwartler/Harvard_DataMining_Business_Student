#' TK
#' Nov 9 2020
#' Compare the historical notes to my portfolio

# Library
library(jsonlite)
library(rbokeh)

# Grab historical data from site
notes      <- as.data.frame(fromJSON('similarNoteReturns.json')$bmDataArraySimilar)
notes$type <- rep('historical', nrow(notes))
myPort     <- data.frame(V1 = 36, V2 = .0832, type = 'actual')

# Combine
notes        <- rbind(myPort, notes)
names(notes) <- c('month', 'rate','type')

# plot
p <- figure() %>%
  ly_points(month, rate, data = notes,
            color = type, glyph = type,
            hover = list(month, rate, type)) %>%
  ly_abline(a      = .0832, 
            b      = 0, 
            type   = 2, 
            color  = 'blue', 
            legend = "my portfolio results")
p

# End