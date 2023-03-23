library("tidyverse")
library("rebird")
library("shiny")
library("shinyWidgets")
library("shinydashboard")

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
  # checks if the term is NA and if it is it returns FALSE
  if (is.na(term))
  {
    return(FALSE)
  }
  # creates a list "places" that holds all of the indexes with occurrences of the 
  # search term
  places <- list()
  checkTerm <- tolower(term)
  
  # starts a for loop that iterates through all rows in the tibble
  for (i in 1:nrow(speciesTibble))
  {
    # checks if the column is not NA
    if (!is.na(speciesTibble[i, columnSearch]))
    {
      # checks if the term and the value in the tibble match
      if (tolower(speciesTibble[i, columnSearch]) == checkTerm)
      {
        # adds the index to the places list
        places <- append(places, i)
      }
    }
  }
  # checks if any occurrences were found
  if (length(places) != 0)
  { # returns the list
    return(places)
  }
  else
  {
    # returns FALSE if no occurrences were found
    return(FALSE)
  }
}


# finds the closest sighting of a species in a given radius, place, and number
findClosestSighting <- function(speciesCode, radius, ApiKey)
{
  # polls the API and gets all of the closest sightings
  closestSighting <- nearestobs(species = speciesCode, lat = lati, lng = long, key = ApiKey, dist = radius)
  # makes sure one was found
  if (ncol(closestSighting) != 0){
    # calculates the distance
    R <- 6371
    phi1 <- lati * pi/180
    phi2 <- closestSighting[1,8] * pi/180
    deltaPhi <- (closestSighting[1,8]-lati) * pi/180
    deltaLamda <- (closestSighting[1,9]-long) * pi /180
    
    a <- (sin(deltaPhi/2)*sin(deltaPhi/2)) + (cos(phi1) * cos(phi2) * sin(deltaLamda/2)* sin(deltaLamda/2))
    c <- 2 * atan(sqrt(a)/sqrt(1-a))
    

    distanceOfClosestSighting <- R * c
    
    # returns the distance of the closest sighting, the location of the
    # closest sighting, and the number of sightings
    return(c(distanceOfClosestSighting, closestSighting[1, 5], nrow(closestSighting)))
  }
  # returns "Outside of Search Distance" if no occurrences were found
  return(list("Outside of Search Distance"))
}



# ui <- fluidPage(
#   setBackgroundColor(color ="darkgrey"),
#   
#   fluidRow(
#     # left sidebar for inputting information
#     sidebarPanel(
#       textInput("species", h3("Species Input"), value = ""),
#       sliderInput(inputId = "radius", "Search Radius(km)",min = 1, max = 50, value = 25)
# 
#     ),
#     # middle sidebar for showing species information
#     sidebarPanel(
#       htmlOutput("SpeciesInfoOut"),
#       tags$head(tags$style("#SpeciesInfoOut{color: black; font-size: 18px"))
#     ),
#     # right sidebar for showing similar species
#     sidebarPanel(
#       h3("Similar Species"),
#       uiOutput("SimilarSpeciesOut")
#     )
# 
#   ),
#   fluidRow(
#     sidebarPanel(
#       h3("API Key"),
#       uiOutput("APILink"),
#       textInput("apikey", "", value = "vmgu1o6c6ihc")
#     )
#   )
# )

colorScheme <- "blue"
ui <- dashboardPage(
  skin = colorScheme,
  dashboardHeader(title = "Vultr"),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "species", buttonId = "speciesSearchButton", label = "Species"),
      menuItem("Species Search", tabName = "1", icon = icon("magnifying-glass", lib = "font-awesome")),
      menuItem("Settings", tabName = "2", icon = icon("gear", lib = "font-awesome"))
    )
  ),
  dashboardBody(
    tabItems(
      #Species Search tab
      tabItem(
        tabName = "1",
        
        fluidRow(

          
          # middle sidebar for showing species information
          box(
            title = "Species Info",
            background = "black",
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            
            htmlOutput("SpeciesInfoOut")
          ),
          
          # right sidebar for showing similar species
          box(
            
            title = "Similar Species",
            background = "black",
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            
            uiOutput("SimilarSpeciesOut")
          )
          
          
        )
      
      ),
      
      
      tabItem(
        tabName = "2",
        box(
          title = "API key",
          background = "black",
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "primary",
          
          
          uiOutput("APILink"),
          textInput("apikey", "", value = "vmgu1o6c6ihc")
        ),
        
        box(
          title = "Search Radius(km)",
          background = "black",
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "primary",
          
          sliderInput(inputId = "radius", label = "", min = 1, max = 50, value = 25)
        ),
        
        box(
          title = "Style",
          background = "black",
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "primary",
          
          drop
        )
      )
      
    )
    
  )
    
)
  
server <- function(input, output)
{

 output$APILink <- renderUI({
   url <- a("Get API Key", href="https://ebird.org/api/keygen")
   tagList("To obtain a API key go to", url,".")
 }) 
  
  # output for species info
  searchButtonPressed <- FALSE
  observeEvent(input$speciesSearchButton,{
    searchButtonPressed <- TRUE
    print(searchButtonPressed)

  })
  output$SpeciesInfoOut <- renderText({ 
    
    inputText <- input$species
    print(inputText)
    searchRad <- input$radius
    key <- input$apikey
    index <- searchSpeciesTibble(2,inputText)[[1]]
    print(searchButtonPressed)
    if (index != FALSE)
    {
      speciesCode <- speciesTibble[index, 3]
      cSightingData <- findClosestSighting(speciesCode, searchRad, key)
      HTML(paste(sep = " ",
                 "Common Name:", speciesTibble[index, 2],
                 "<br/>Scientific Name:", speciesTibble[index, 1],
                 "<br/>Family:", speciesTibble[index, 11],
                 "<br/>Closest Sighting Distance (km):", cSightingData[1],
                 "<br/>Location of Closest Sighting:", cSightingData[2],
                 "<br/>Number of Sightings in", searchRad,"km:", cSightingData[3]))

    }
    else
    {
      paste("Invalid")
    }
    
  })
  
  output$SimilarSpeciesOut <- renderUI({
    
    inputText <- input$species
    searchRad <- strtoi(input$radius)
    key <- input$apikey
    speciesIndex <- searchSpeciesTibble(2,inputText)[[1]]
    if(speciesIndex != FALSE)
    {
      
      speciesFamily <- speciesTibble[speciesIndex, 10][[1,1]]
      birdFamily <- searchSpeciesTibble(10, speciesFamily)
      if(length(birdFamily) != 0)
      {
        birdFamilyName <- list()
        for(i in 1:length(birdFamily))
        {
          birdName <- speciesTibble[birdFamily[[i]],2][[1,1]]
          birdCode <- speciesTibble[birdFamily[[i]],3][[1,1]]
          if (findClosestSighting(birdCode, searchRad, key)[[1]] != "Outside of Search Distance")
          {
            birdFamilyName <- append(birdFamilyName, birdName)
          }
        }
        
          HTML(
            paste(
              birdFamilyName
            )
          )
      
      }

    }
    
  })
}

shinyApp(ui = ui, server = server)
