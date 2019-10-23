crackPack <- function (spoiler, packsPerMythic = 8, 
                       packsPerFoil = 6, foilsInSet = T) {
    mythics   <- subset(spoiler, spoiler$R == "M")
    rares     <- subset(spoiler, spoiler$R == "R")
    uncommons <- subset(spoiler, spoiler$R == "U")
    commons   <- subset(spoiler, spoiler$R == "C")
    spoiler   <- spoiler[!(grepl("T", spoiler$R)), ]
    spoiler   <- spoiler[!(grepl("L", spoiler$R)), ]
    commons   <- spoiler %>% filter(R == "C") %>% sample_n(11)
    uncommons <- spoiler %>% filter(R == "U") %>% sample_n(3)
    if (nrow(mythics) > 0) {
      mythicCheck <- sample(c(0, 1), 1, 
                            prob = c(1 - (1/packsPerMythic), 
                                                 1/packsPerMythic))
      if (mythicCheck == 0) {
        premium <- spoiler %>% filter(R == "R") %>% sample_n(1)
      } else {
        premium <- spoiler %>% filter(R == "M") %>% sample_n(1)
      }
    } else {
      premium <- spoiler %>% filter(R == "R") %>% sample_n(1)
    }
    if (foilsInSet == T) {
      foilCheck <- sample(c(0, 1), 1, 
                          prob = c(1 - (1/packsPerFoil), 
                                               1/packsPerFoil))
      if (foilCheck == 1) {
        foil             <- spoiler %>% sample_n(1)
        commons          <- commons %>% sample_n(10)
        boosterPack      <- rbind(commons, uncommons, premium, foil)
        boosterPack$foil <- c(rep("FALSE", 14), "TRUE")
      } else {
        boosterPack      <- rbind(commons, uncommons, premium)
        boosterPack$foil <- c(rep("FALSE", 15))
      }
    } else {
      boosterPack      <- rbind(commons, uncommons, premium)
      boosterPack$foil <- c(rep("FALSE", 15))
    }
    return(boosterPack)
  }