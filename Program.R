library("tidyverse")
library("rebird")

APIkey <- "vmgu1o6c6ihc"
speciesTibble <- rebird:::tax
species <- tolower(readline("Species Name?\n"))

searchSpeciesTable <- function(columnSearch, columnOutput, commonName)
{
  for (i in 1:16743)
  {
    if (tolower(speciesTibble[i, columnSearch]) == commonName)
    {
      code <- toString(speciesTibble[i, columnOutput])
      break
    }
  }
  return(code)
}

ebirdCode <- searchSpeciesTable(2,3,species)
print(ebirdCode)
print(birdIndex)
print(ebirdgeo(species = ebirdCode, lat = 45.5, lng = -122.6, key = APIkey))
