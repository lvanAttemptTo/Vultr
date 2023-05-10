# installs packages that are not installed

# list of required packages
packageList <- c("tidyverse", "rebird", "shiny", "shinyWidgets", "shinydashboard",
                 "shinydashboardPlus", "auk", "dynutils", "shinyalert", "contactdata",
                 "countrycode", "geosphere", "shinyjs", "fresh", "leaflet", "dplyr",
                 "shinyBS", "maps", "FlickrAPI", "shinymanager", "scrypt")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]

if(length(newPackages)) install.packages(newPackages)

library("tidyverse")
library("rebird")
library("shiny")
library("shinyWidgets")
library("shinydashboard")
library("shinydashboardPlus")
library("auk")
library("dynutils")
library("shinyalert")
library("contactdata")
library("countrycode")
library("geosphere")
library("shinyjs")
library("fresh")
library("leaflet")
library("dplyr")
library("shinyBS")
library("maps")
library("FlickrAPI")
library("shinyauthr")
library("RSQLite")
library("glue")
library("DBI")
library("lubridate")



flickerAPIkey <- "282bedc95f24d8bb2638d1c9f6c7a7fa"

currentDate <- Sys.Date()

speciesCodeList <- list()

# Vultr is a program that helps people identify birds
APIkey <- "vmgu1o6c6ihc"

# this is a tibble that has all the species in it
speciesTibble <- ebirdtaxonomy()
# long and lat for finding nearby birds

searchTibble <- function(inTibble, term)
{
    df <- as.data.frame(inTibble)
    occurences <- which(df == term, arr.ind = TRUE)
    return(occurences)
}

print(as.integer(searchTibble(speciesTibble, "House Sparrow")[1]))

countryList <- as.list(ebirdsubregionlist(regionType = "country", key = APIkey)$name)

codeCommonNameDict <- c()
commonNameIndexDict <- c()

for (i in 1:nrow(speciesTibble))
{
    codeCommonNameDict[speciesTibble[[i,3]]] <- speciesTibble[[i,2]]
    commonNameIndexDict[tolower(speciesTibble[[i,2]])] <- i
}


# function that converts species code to common name
ebirdCodeToCommon <- function(code)
{
    if (is.na(code))
    {
        return(FALSE)
    }
    commonName <- as.character(codeCommonNameDict[code])
    return(commonName)
}

commonToIndex <- function(name)
{
    if (is.na(name))
    {
        return(FALSE)
    }
    index <- as.integer(commonNameIndexDict[name])
    return(index)
}

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
    {
        # returns the list
        return(places)
    }
    else
    {
        # returns FALSE if no occurrences were found
        return(FALSE)
    }
}


# finds the closest sighting of a species in a given radius, place, and number
findClosestSighting <- function(speciesCode, radius, ApiKey, lati, long, daysBack)
{
    # polls the API and gets all of the closest sightings
    closestSightings <- nearestobs(species = speciesCode, lat = lati, lng = long, key = ApiKey, dist = radius, back = daysBack)
    # makes sure one was found
    if (ncol(closestSightings) != 0){
        # calculates the distance
        distanceOfclosestSighting <- 50
        indexOfClosestSighting <- NaN
        latVec <- c()
        lngVec <- c()
        for (i in 1:nrow(closestSightings))
        {
            lng <- closestSightings[[i,"lng"]]
            lat <- closestSightings[[i,"lat"]]
            distance <- distHaversine(c(long, lati), c(lng, lat))
            distance <- distance/1000
            if (distance < distanceOfclosestSighting)
            {
                distanceOfclosestSighting <- distance
                indexOfClosestSighting <- i
            }
            lngVec <- append(lngVec, lng)
            latVec <- append(latVec, lat)
        }
        
        
        # returns the distance of the closest sighting, the location of the
        # closest sighting, and the number of sightings
        return(list(distanceOfclosestSighting, closestSightings[[indexOfClosestSighting, 5]], nrow(closestSightings), closestSightings[1:nrow(closestSightings), 5]
                    ,lngVec, latVec))
    }
    # returns "Outside of Search Distance" if no occurrences were found
    return(list("Outside of Search Distance"))
}

existsInTibble <- function(tibbleIn, column, term)
{
    if (is.na(term))
    {
        return(FALSE)
    }
    found <- FALSE
    for (i in 1:nrow(tibbleIn))
    {
        if (!is.na(tibbleIn[i,column]))
        {
            if (tibbleIn[i, column] == term)
            {
                found <- TRUE
            }
        }
    }
    return(found)
}


# user Info
source("UserInfo/credentials.R", local = TRUE)

# ui script
source("Ui/Ui_Shell.R", local = TRUE)
# server script


correct <- 0
incorrect <- 0
quizSubmit <- 0
searchVar <- 0
signedInFlag <- FALSE
source("Server/Server_Shell.R", local = TRUE)

# creates the app
shinyApp(ui = ui, server = server)