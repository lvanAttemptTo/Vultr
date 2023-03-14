library("tidyverse")
library("rebird")
library("shiny")


# Vultr is a program that helps people identify birds
APIkey <- "vmgu1o6c6ihc"

# this is a tibble that has all the species in it
speciesTibble <- rebird:::tax
# long and lat for finding nearby birds
lati <- 45.5
long <- -122.6

# function that searches the species tibble and returns the indexes of occurrences
searchSpeciesTibble <- function(columnSearch, term)
{
  places <- list()
  
  for (i in 1:nrow(speciesTibble))
  {
    if (tolower(speciesTibble[i, columnSearch]) == tolower(term))
    {
      append(places, i)
    }
  }
  
  if (length(places))
  {
    return(places)
  }
  else
  {
    return(FALSE)
  }
}



birdIndex <- searchSpeciesTibble(2, species)[1]
ebirdCode <- speciesTibble[birdIndex, 3]

findClosestSighting <- function(speciesCode, radius)
{
  closestSighting <- nearestobs(species = speciesCode, lat = lati, lng = long, key = APIkey, dist = radius)
  print(closestSighting)
  if (ncol(closestSighting) != 0){
    R <- 6371
    phi1 <- lati * pi/180
    phi2 <- closestSighting[1,8] * pi/180
    deltaPhi <- abs(closestSighting[1,8]-lati) * pi/180
    deltaLamda <- abs(closestSighting[1,9]-long) * pi /180
    
    a <- (sin(deltaPhi/2)*sin(deltaPhi/2)) + (cos(phi1) * cos(phi2) * sin(deltaLamda/2)* sin(deltaLamda/2))
    c <- 2 * atan(sqrt(a)/sqrt(1-a))
    distanceOfClosestSighting <- NaN
    distanceOfClosestSighting <- R * c
    return(c(distanceOfClosestSighting, closestSighting[1, 5]))
  }
  return("Outside Search Distance")
}

findNumOfObs <- function(speciesCode, radius)
{
  numSightingsTibble <- ebirdgeo(species = speciesCode, key = APIkey, lat = lati, lng = long, dist = radius)
  numSightings <- nrow(numSightingsTibble)
  return(numSightings)
}



ui <- fluidPage(
  
  fluidRow(
    sidebarPanel(
      textInput("species", h3("Species Input"), value = ""),
      textInput("radius", h3("Search Radius(km)"), value = 25),

    ),
    sidebarPanel(
      htmlOutput("SpeciesInfoOut"),
      tags$head(tags$style("#SpeciesInfoOut{color: black; font-size: 18px"))
    ),
    sidebarPanel(
      h3("Similar Species")
    )

  )
)
server <- function(input, output)
{
  output$SpeciesInfoOut <- renderText({ 
    inputText <- input$species
    searchRad <- strtoi(input$radius)
    index <- searchSpeciesTibble(2,inputText)
    print(index)
    if (index != FALSE & !is.na(searchRad) & searchRad != 0)
    {
      speciesCode <- speciesTibble[index, 3]
      HTML(paste(sep = " ",
        "Common Name:", speciesTibble[index, 2],
        "<br/>Scientific Name:", speciesTibble[index, 1],
        "<br/>Family:", speciesTibble[index, 11],
        "<br/>Closest Sighting Distance (km):", findClosestSighting(speciesCode, searchRad)[1],
        "<br/>Location of Closest Sighting:", findClosestSighting(speciesCode, searchRad)[2],
        "<br/>Number of Sightings in", searchRad,"km:", findNumOfObs(speciesCode, searchRad)))
    }
    else
    {
      paste("Invalid")
    }
  })
}

shinyApp(ui = ui, server = server)
