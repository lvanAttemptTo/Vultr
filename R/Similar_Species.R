library("rebird")
library("dplyr")
library("dataset")

# this is a tibble that has all the species in it
speciesTibble <- ebirdtaxonomy()


getSimilarSpecies(species = NA, amount = 5)
{
    similarSpecies <- c()
    
    # algorithm
    temp1 <- arrange(speciesTibble, desc(taxonOrder))
    find
    
    # returns the vector
    
    return(similarSpecies)
}