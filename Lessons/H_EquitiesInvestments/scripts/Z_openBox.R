openBox <- function(spoiler, numPacks = 36, 
                    packsPerMythic = 8, 
                    packsPerFoil = 6, foilsInSet = T) {
    boosterBox <- list()
    for (i in 1:numPacks){
      cat(paste("opening pack", i, "\n"))
      pack <- crackPack(spoiler, 
                        packsPerMythic, 
                        packsPerFoil, 
                        foilsInSet)
      nam <- i
      boosterBox[[nam]] <- pack
    }
    boosterBox <- do.call(rbind, boosterBox)
    return(boosterBox)
  }