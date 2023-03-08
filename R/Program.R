library("tidyverse")
library("rebird")

APIkey <- "vmgu1o6c6ihc"
consoleWrtieString <- "Species Name?
"
speciesTibble <- rebird:::tax
species <- tolower(readline(consoleWrtieString))
lati <- 45.5
long <- -122.6

searchSpeciesTable <- function(columnSearch, columnOutput, commonName, indexOut)
{
  code <- ""
  place <- NaN
  
  for (i in 1:16743)
  {
    if (tolower(speciesTibble[i, columnSearch]) == commonName)
    {
      code <- toString(speciesTibble[i, columnOutput])
      place <- i
      break
    }
  }
  
  if(indexOut == FALSE)
  {
    if (code != "")
    {
      return(code)
    }
    else
    {
      species <- tolower(readline(consoleWrtieString))
      searchSpeciesTable(2, 3, species, FALSE)
    }
  }
  else
  {
    if (!is.nan(place))
    {
      return(place)
    }
    else
    {
      species <- tolower(readline(consoleWrtieString))
      searchSpeciesTable(2, 3, species, TRUE)
    }
  }
}


ebirdCode <- searchSpeciesTable(2,3,species, FALSE)
birdIndex <- searchSpeciesTable(2,3, species, TRUE)
print(speciesTibble[birdIndex, 1:15])
closestSighting <- nearestobs(species = ebirdCode, lat = lati, lng = long, key = APIkey)[1,1:13]
R <- 6371
phi1 <- lati * pi/180
phi2 <- closestSighting[1,8] * pi/180
deltaPhi <- abs(closestSighting[1,8]-lati) * pi/180
deltaLamda <- abs(closestSighting[1,9]-long) * pi /180

a <- (sin(deltaPhi/2)*sin(deltaPhi/2)) + (cos(phi1) * cos(phi2) * sin(deltaLamda/2)* sin(deltaLamda/2))
c <- 2 * atan(sqrt(a)/sqrt(1-a))



distanceOfClosestSighting = R * c
print(closestSighting)
print("/n/n/n")
print(c("distance: ",distanceOfClosestSighting," km"))

