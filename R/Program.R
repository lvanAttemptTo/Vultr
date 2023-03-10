library("tidyverse")
library("rebird")
library("shiny")




APIkey <- "vmgu1o6c6ihc"

consoleWriteString <- ""

speciesTibble <- rebird:::tax
lati <- 45.5
long <- -122.6
species <- "house sparrow"
searchSpeciesTibble <- function(columnSearch, columnOutput, commonName, indexOut)
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

    }
  }
}


ebirdCode <- searchSpeciesTibble(2,3,species, FALSE)
birdIndex <- searchSpeciesTibble(2,3, species, TRUE)
print(speciesTibble[birdIndex, 1:15])

findClosestSighting <- function(speciesCode)
{
  closestSighting <- nearestobs(species = speciesCode, lat = lati, lng = long, key = APIkey)[1,1:13]
  R <- 6371
  phi1 <- lati * pi/180
  phi2 <- closestSighting[1,8] * pi/18
  deltaPhi <- abs(closestSighting[1,8]-lati) * pi/180
  deltaLamda <- abs(closestSighting[1,9]-long) * pi /180
  
  a <- (sin(deltaPhi/2)*sin(deltaPhi/2)) + (cos(phi1) * cos(phi2) * sin(deltaLamda/2)* sin(deltaLamda/2))
  c <- 2 * atan(sqrt(a)/sqrt(1-a))
  distanceOfClosestSighting = R * c
  return(distanceOfClosestSighting)
}





print("/n/n/n")

ui <- fluidPage(
  textInput("text", h3("TextInput"), value = "Enter Species Name"),
  textOutput("Hello")
)
server <- function(input, output)
{
  output$Hello <- renderText({ 
    paste(findClosestSighting(searchSpeciesTibble(2,3,input$text,FALSE)))
  })
}

shinyApp(ui = ui, server = server)
