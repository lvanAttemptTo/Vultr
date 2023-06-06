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
library("remotes")
library("colormod")
# custom ebird API library
source("Pelican/Pelican.R")
markerColor <- colorNumeric(palette = c("white", 'lightgray', "beige", "lightgreen", "green", "darkgreen", "lightblue", "cadetblue", "blue", "darkblue", "pink", "purple", "orange", "red", "darkred"), 0:500)
markerColorDate <- function(daysback)
{
    return(colorNumeric(palette = c("white", 'lightgray', "beige", "lightgreen", "green", "darkgreen", "lightblue", "cadetblue", "blue", "darkblue", "pink", "purple", "orange", "red", "darkred"), 0:daysback))
}
print(markerColor(500))
# reverse legend
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
    position <- match.arg(position)
    type <- "unknown"
    na.color <- NULL
    extra <- NULL
    if (!missing(pal)) {
        if (!missing(colors)) 
            stop("You must provide either 'pal' or 'colors' (not both)")
        if (missing(title) && inherits(values, "formula")) 
            title <- deparse(values[[2]])
        values <- evalFormula(values, data)
        type <- attr(pal, "colorType", exact = TRUE)
        args <- attr(pal, "colorArgs", exact = TRUE)
        na.color <- args$na.color
        if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
            0) {
            na.color <- NULL
        }
        if (type != "numeric" && !missing(bins)) 
            warning("'bins' is ignored because the palette type is not numeric")
        if (type == "numeric") {
            cuts <- if (length(bins) == 1) 
                pretty(values, bins)
            else bins	
            
            if (length(bins) > 2) 
                if (!all(abs(diff(bins, differences = 2)) <= 
                         sqrt(.Machine$double.eps))) 
                    stop("The vector of breaks 'bins' must be equally spaced")
            n <- length(cuts)
            r <- range(values, na.rm = TRUE)
            cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
            n <- length(cuts)
            p <- (cuts - r[1])/(r[2] - r[1])
            extra <- list(p_1 = p[1], p_n = p[n])
            p <- c("", paste0(100 * p, "%"), "")
            if (decreasing == TRUE){
                colors <- pal(rev(c(r[1], cuts, r[2])))
                labels <- rev(labFormat(type = "numeric", cuts))
            }else{
                colors <- pal(c(r[1], cuts, r[2]))
                labels <- rev(labFormat(type = "numeric", cuts))
            }
            colors <- paste(colors, p, sep = " ", collapse = ", ")
            
        }
        else if (type == "bin") {
            cuts <- args$bins
            n <- length(cuts)
            mids <- (cuts[-1] + cuts[-n])/2
            if (decreasing == TRUE){
                colors <- pal(rev(mids))
                labels <- rev(labFormat(type = "bin", cuts))
            }else{
                colors <- pal(mids)
                labels <- labFormat(type = "bin", cuts)
            }
            
        }
        else if (type == "quantile") {
            p <- args$probs
            n <- length(p)
            cuts <- quantile(values, probs = p, na.rm = TRUE)
            mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                             na.rm = TRUE)
            if (decreasing == TRUE){
                colors <- pal(rev(mids))
                labels <- rev(labFormat(type = "quantile", cuts, p))
            }else{
                colors <- pal(mids)
                labels <- labFormat(type = "quantile", cuts, p)
            }
        }
        else if (type == "factor") {
            v <- sort(unique(na.omit(values)))
            colors <- pal(v)
            labels <- labFormat(type = "factor", v)
            if (decreasing == TRUE){
                colors <- pal(rev(v))
                labels <- rev(labFormat(type = "factor", v))
            }else{
                colors <- pal(v)
                labels <- labFormat(type = "factor", v)
            }
        }
        else stop("Palette function not supported")
        if (!any(is.na(values))) 
            na.color <- NULL
    }
    else {
        if (length(colors) != length(labels)) 
            stop("'colors' and 'labels' must be of the same length")
    }
    legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                   na_color = na.color, na_label = na.label, opacity = opacity, 
                   position = position, type = type, title = title, extra = extra, 
                   layerId = layerId, className = className, group = group)
    invokeMethod(map, data, "addLegend", legend)
}
# import for ebird life list

# ebirdLifeList <- read.csv("ebird_world_life_list.csv")$Common.Name
# tempStr <- ""
# for (i in ebirdLifeList)
# {
#     tempStr <- paste0(tempStr, ";", i)
# }
# print(tempStr)
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
speciesPhotoIndex <- 1
searchedSpecies <- ""
hotspotMapSettings <- c("speciesCount", "Hybrid")
source("Server/Server_Shell.R", local = TRUE)

# creates the app
shinyApp(ui = ui, server = server)