
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


# custom ebird API library
library("httr")
library("stringr")


nearbyObs <- function(key = NA, lat = NA, lng = NA, dist = 25, back = 14, cat = "all", hotspot = FALSE, includeProvisional = FALSE, sort = "date", sppLocale = "en", maxResults = "all")
{
    if (!is.na(key) & !is.na(lat) & !is.na(lng))
    {
        headers = c(
            
            'X-eBirdApiToken' = key
        )
        if (cat == "all" & maxResults == "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/geo/recent?lat=", lat, "&lng=", lng, "&sort=", sort, "&dist=", dist, "&back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale), add_headers(headers))
        }
        else if (cat != "all" & maxResults != "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/geo/recent?lat=", lat, "&lng=", lng, "&sort=", sort, "&dist=", dist, "&back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&cat=", cat, "&maxResults=", maxResults), add_headers(headers))
        }
        else if (cat != "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/geo/recent?lat=", lat, "&lng=", lng, "&sort=", sort, "&dist=", dist, "&back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&cat=", cat), add_headers(headers))
        }
        else
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/geo/recent?lat=", lat, "&lng=", lng, "&sort=", sort, "&dist=", dist, "&back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&maxResults=", maxResults), add_headers(headers))
        }
        catRes <- content(res, 'text')
        parsedSightings <- unlist(str_split(catRes[[1]], "[}]"))
        parsedSightings <- parsedSightings[!duplicated(parsedSightings)]
        parsedSightingInformation <- data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        if (length(parsedSightings) > 1)
        {
            for (i in 1:(length(parsedSightings)-1))
            {
                testParse1 <- unlist(str_split(parsedSightings[i], "[{]"))[2]
                testParse2 <- unlist(str_split(testParse1, ',\"'))
                testParse3 <- list()
                for (j in testParse2)
                {
                    p <- unlist(str_split(j, "[:]"))[2]
                    
                    p <- gsub('"', "", p)
                    
                    if (p == "true")
                    {
                        p <- TRUE
                    }
                    else if (p == "false")
                    {
                        p <- FALSE
                    }
                    suppressWarnings(
                        expr = {
                            if (!is.na(as.numeric(p)))
                            {
                                p <- as.numeric(p)
                            }
                        }
                        
                        
                    )
                    
                    
                    testParse3 <- append(testParse3, p)
                }
                if (length(testParse3) == 12)
                {
                    temp <- testParse3[1:6]
                    temp <- append(temp, NaN)
                    temp <- append(temp, testParse3[7:12])
                    testParse3 <- temp
                }
                else if(length(testParse3) == 14)
                {
                    testParse3 <- testParse3[1:13]
                }
                parsedSightingInformation[i,] <- testParse3
            }
        }
        # df <- data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        # for (i in 1:length(catRes))
        # {
        #     
        #     df[i,] <- catRes[[i]]
        # }
        # return(df)
        return(parsedSightingInformation)
    }
    else if (is.na(key))
    {
        warning("No key set")
    }
    else if (is.na(lat))
    {
        warning("No latitude set")
    }
    else if (is.na(lng))
    {
        warning("No longitude set")
    }
}


regionObs <- function(key = NA, regionCode = NA, back = 14, cat = "all", hotspot = FALSE, includeProvisional = FALSE, sppLocale = "en", maxResults = "all")
{
    if (!is.na(key) & !is.na(regionCode))
    {
        headers = c(
            
            'X-eBirdApiToken' = key
        )
        
        if (cat == "all" & maxResults == "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/", regionCode, "/recent", "?back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale), add_headers(headers))
        }
        else if (cat != "all" & maxResults != "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/", regionCode, "/recent", "?back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&cat=", cat, "&maxResults=", maxResults), add_headers(headers))
        }
        else if (cat != "all")
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/", regionCode, "/recent", "?back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&cat=", cat), add_headers(headers))
        }
        else
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/data/obs/", regionCode, "/recent", "?back=", back, "&hotspot=", hotspot, "&includeProvisional=", includeProvisional, "&sppLocal=", sppLocale, "&maxResults=", maxResults), add_headers(headers))
        }
        
        catRes <- content(res, 'text')
        parsedSightings <- unlist(str_split(catRes[[1]], "[}]"))
        parsedSightings <- parsedSightings[!duplicated(parsedSightings)]
        parsedSightingInformation <-data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        if (length(parsedSightings) > 1)
        {
            for (i in 1:(length(parsedSightings)-1))
            {
                testParse1 <- unlist(str_split(parsedSightings[i], "[{]"))[2]
                testParse2 <- unlist(str_split(testParse1, ',\"'))
                testParse3 <- list()
                for (j in testParse2)
                {
                    p <- unlist(str_split(j, "[:]"))[2]
                    
                    p <- gsub('"', "", p)
                    
                    if (p == "true")
                    {
                        p <- TRUE
                    }
                    else if (p == "false")
                    {
                        p <- FALSE
                    }
                    suppressWarnings(
                        expr = {
                            if (!is.na(as.numeric(p)))
                            {
                                p <- as.numeric(p)
                            }
                        }
                        
                        
                    )
                    
                    testParse3 <- append(testParse3, p)
                }
                if (length(testParse3) == 12)
                {
                    temp <- testParse3[1:6]
                    temp <- append(temp, NaN)
                    temp <- append(temp, testParse3[7:12])
                    testParse3 <- temp
                }
                else if(length(testParse3) == 14)
                {
                    testParse3 <- testParse3[1:13]
                }
                parsedSightingInformation[i,] <- testParse3
            }
        }
        # df <- data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
        # for (i in 1:length(catRes))
        # {
        #     
        #     df[i,] <- catRes[[i]]
        # }
        # return(df)
        return(parsedSightingInformation)
    }
    else if (is.na(key))
    {
        warning("No key set")
    }
    else if (is.na(lat))
    {
        warning("No latitude set")
    }
    else if (is.na(lng))
    {
        warning("No longitude set")
    }
}

nearbyHotspots <- function(key = NA, lat = NA, lng = NA, dist = 25, back = NA)
{
    if (!is.na(key) & !is.na(lat) & !is.na(lng))
    {
        headers = c(
            
            'X-eBirdApiToken' = key
        )
        if (is.na(back))
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/ref/hotspot/geo?lat=", lat, "&lng=", lng, "&dist=", dist, "&fmt=json"), add_headers(headers))
        }
        else
        {
            res <- VERB("GET", url = paste0("https://api.ebird.org/v2/ref/hotspot/geo?lat=", lat, "&lng=", lng, "&dist=", dist, "&back=", back, "&fmt=json"), add_headers(headers))
        }
        catRes <- content(res, "text")
        parsedSightings <- unlist(str_split(catRes[[1]], "[}]"))
        parsedSightings <- parsedSightings[!duplicated(parsedSightings)]
        parsedSightingInformation <-data.frame(locId = NA, locName = NA, country = NA, subregion1 = NA, subregion2 = NA, lat = NA, lng = NA,  lastSighting = NA, speciesCount = NA)
        if (length(parsedSightings) > 1)
        {
            for (i in 1:(length(parsedSightings)-1))
            {
                testParse1 <- unlist(str_split(parsedSightings[i], "[{]"))[2]
                testParse2 <- unlist(str_split(testParse1, ',\"'))
                testParse3 <- list()
                for (j in testParse2)
                {
                    p <- unlist(str_split(j, "[:]"))[2]
                    
                    p <- gsub('"', "", p)
                    
                    if (p == "true")
                    {
                        p <- TRUE
                    }
                    else if (p == "false")
                    {
                        p <- FALSE
                    }
                    suppressWarnings(
                        expr = {
                            if (!is.na(as.numeric(p)))
                            {
                                p <- as.numeric(p)
                            }
                        }
                        
                        
                    )
                    
                    testParse3 <- append(testParse3, p)
                }
                
                parsedSightingInformation[i,] <- testParse3
            }
        }
        parsedSightingInformation[i,] <- testParse3
    }
    return(parsedSightingInformation)
}

nearbyHotspots(key = "vmgu1o6c6ihc", lat = 45.5896568645855, lng = -122.738592624664)

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
cookie_expiry <- 7

# This function must return a data.frame with columns user and sessionid.  Other columns are also okay
# and will be made available to the app after log in.

get_sessions_from_db <- function(conn = db, expiry = cookie_expiry) {
    dbReadTable(conn, "sessions") %>%
        mutate(login_time = ymd_hms(login_time)) %>%
        as_tibble() %>%
        filter(login_time > now() - days(expiry))
}

# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.

add_session_to_db <- function(user, sessionid, conn = db) {
    tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
        dbWriteTable(conn, "sessions", ., append = TRUE)
}

db <- dbConnect(SQLite(), ":memory:")
dbCreateTable(db, "sessions", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))

user_base <- tibble(
    user = c("lvan", "abc"),
    password = c("test", "pass"),
    password_hash = sapply(c("test", "pass"), sodium::password_store),
    permissions = c("admin", "standard"),
    name = c("Luke VanDeGrift", "ABC"),
    country = c("United States", "United Kingdom"),
    state = c("Oregon", "England"),
    county = c("Multnomah", "Bedfordshire"),
    ebirdKey = c ("vmgu1o6c6ihc", "vmgu1o6c6ihc"),
    specificLocation = c(TRUE, FALSE),
    specificLatitude = c("45.5896568645855", ""),
    specificLongitude = c("-122.738592624664", ""),
    radius = c(25, 10),
    daysBack = c(7, 30),
    lifeList = c("Merlin;Rufous Hummingbird;Hutton's Vireo;Lesser Yellowlegs;Rhinoceros Auklet;Dunlin;Sanderling;Black Turnstone;Brant;White-winged Scoter;Surf Scoter;Long-tailed Duck;Pigeon Guillemot;Red-necked Grebe;Barrow's Goldeneye;Pileated Woodpecker;Rough-legged Hawk;Short-eared Owl;Hooded Merganser;Western Bluebird;Red-crowned Parrot;Acorn Woodpecker;Blue-gray Gnatcatcher;Sharp-shinned Hawk;Black-necked Stilt;Greater Roadrunner;Clark's Grebe;Yellow-crowned Night-Heron;Ridgway's Rail;Lincoln's Sparrow;Cassin's Kingbird;American Pipit;Whimbrel;Wrentit;Say's Phoebe;Royal Tern;Forster's Tern;Caspian Tern;Short-billed Gull;Willet;Long-billed Curlew;White-faced Ibis;Common Gallinule;Hermit Thrush;House Wren;Red-shouldered Hawk;Orange-crowned Warbler;California Towhee;Black Phoebe;Allen's Hummingbird;Snowy Egret;Pelagic Cormorant;Brandt's Cormorant;Heermann's Gull;Common Murre;Harlequin Duck;Brown Pelican;Purple Finch;Band-tailed Pigeon;Western Gull;Glaucous-winged Gull;Pacific Wren;Pacific-slope Flycatcher;Lazuli Bunting;Yellow-breasted Chat;Swainson's Thrush;Spotted Sandpiper;Horned Lark;Cassin's Finch;Trumpeter Swan;Broad-tailed Hummingbird;MacGillivray's Warbler;Dusky Flycatcher;Willow Flycatcher;Western Wood-Pewee;Yellow Warbler;Mountain Bluebird;Mountain Chickadee;Clark's Nutcracker;White-throated Swift;Eared Grebe;Great Horned Owl;Gray Catbird;Brown Thrasher;Green Heron;Herring Gull;Ring-billed Gull;Blue Jay;Bobolink;Cooper's Hawk;Baltimore Oriole;Dickcissel;Northern Cardinal;Chipping Sparrow;Chimney Swift;Common Grackle;Brown-headed Cowbird;Western Meadowlark;Common Nighthawk;Common Raven;Western Kingbird;Black-billed Magpie;Swainson's Hawk;Eastern Kingbird;Prairie Falcon;Ferruginous Hawk;California Gull;Eurasian Collared-Dove;California Quail;Warbling Vireo;Black-headed Grosbeak;Vaux's Swift;Wilson's Warbler;Western Tanager;Yellow-headed Blackbird;Savannah Sparrow;Marsh Wren;Northern Rough-winged Swallow;Blue-winged Teal;Yellow-rumped Warbler;Common Yellowthroat;Greater Yellowlegs;Pied-billed Grebe;Cliff Swallow;Barn Swallow;Purple Martin;Violet-green Swallow;Turkey Vulture;Peregrine Falcon;Osprey;Golden Eagle;Red-throated Loon;Greater Scaup;Tufted Duck;Harris's Sparrow;Ring-necked Duck;Canvasback;Snow Goose;Sandhill Crane;Killdeer;Western Grebe;Horned Grebe;Common Goldeneye;Lesser Scaup;Steller's Jay;White-throated Sparrow;Brown Booby;Red Junglefowl;Pacific Golden-Plover;Warbling White-eye;Red-vented Bulbul;Rose-ringed Parakeet;Black-crowned Night-Heron;Zebra Dove;Yellow-fronted Canary;Cattle Egret;Common Waxbill;White Tern;Red-crested Cardinal;Java Sparrow;Rock Pigeon;Common Myna;Spotted Dove;American White Pelican;European Starling;Black-capped Chickadee;American Kestrel;Belted Kingfisher;Wilson's Snipe;Long-billed Dowitcher;American Coot;Ruddy Duck;Common Merganser;Bufflehead;Green-winged Teal;Northern Pintail;Mallard;American Wigeon;Gadwall;Northern Shoveler;Tundra Swan;Brewer's Blackbird;American Robin;Spotted Towhee;Fox Sparrow;American Bittern;Red-breasted Merganser;Eurasian Wigeon;Wood Duck;Pine Siskin;American Crow;Bushtit;Double-crested Cormorant;Cedar Waxwing;Bald Eagle;Cackling Goose;Canada Goose;Dark-eyed Junco;Bewick's Wren;Ruby-crowned Kinglet;Red-breasted Sapsucker;Anna's Hummingbird;Chestnut-backed Chickadee;Northern Flicker;White-breasted Nuthatch;Downy Woodpecker;Golden-crowned Sparrow;White-crowned Sparrow;American Goldfinch;Lesser Goldfinch;House Finch;California Scrub-Jay;Mourning Dove;Golden-crowned Kinglet;Brown Creeper;House Sparrow;Red-winged Blackbird;Bank Swallow;Tree Swallow;Hairy Woodpecker;Great Gray Owl;Red-breasted Nuthatch;Canada Jay;Ruffed Grouse;Song Sparrow;Varied Thrush;Northern Harrier;Great Egret;Great Blue Heron;Cinnamon Teal;Red-tailed Hawk;Indian Peafowl;Mandarin Duck;cormorant sp.;swallow sp.;goose sp.;new world sparrow sp.;Buteo sp.;gull sp.", "House Sparrow")
)



# ui script
# custom color scheme 
colorScheme <- create_theme(
    adminlte_color(
        light_blue = "#004475",
        
    ),
    adminlte_sidebar(
        dark_bg = "#121212",
        dark_hover_bg = "#002b4a",
        dark_color = "#2E3440"
    ),
    adminlte_global(
        content_bg = "#2e2e2e",
        box_bg = "#121212", 
        info_box_bg = "#121212"
    )
)
ebirdRegionName <- "Multnomah County"
ebirdRegionCode <- "US-OR-051"
SpeciesInfoTab <- tabItem(
    tabName = "speciesSearch", # tab ID
    
    # fluid row for boxes
    fluidRow(
        
        # column for sighting locations
        column(
            width = 6, # covers half the width
            # box that has a list of all the locations that the species has been sighted at
            box(
                # box appearance settings 
                width = 12,
                title = "Sightings Locations",
                background = "black",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                
                # output for the locations list
                uiOutput("sightingLocations") 
            )
            # end of column for sighting locations
        ),
        # column for similar species and species info
        column(
            width = 6, # column covers half the width
            box(
                # box appearance settings
                width = 12,
                title = "Photo",
                background = "black",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                
                # image for bird
                htmlOutput("SpeciesPhoto"),
                actionBttn("previousphotosearch", "Previous Photo", icon = icon("arrow-left", lib = "font-awesome"), size = "xs"),
                actionBttn("nextphotosearch", "next Photo", icon = icon("arrow-right", lib = "font-awesome"), size = "xs")
            ),
            # box for species info
            box(
                # box appearance settings
                width = 12,
                title = "Species Info",
                background = "black",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                
                # output for the species info text
                htmlOutput("SpeciesInfoOut")
            ),
            
            
            # box for similar species output
            box(
                # box appearance settings
                width = 12,
                title = uiOutput("SimilarSpeciesTitle"),
                background = "black",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                
                # output for the similar species list
                uiOutput("SimilarSpeciesOut")
            )
            # end of column for species info and simlar species
        )
        # end of fluid row
    )
    # end of tab for species search
)
SettingsTab <- tabItem(
    tabName = "settings", # tab ID
    # modal for setting specific location
    bsModal("locationInModal", "", "InputMapOpen", size = "large", tags$span(
        leafletOutput("locationInMap"),
        style = "color: #121212")),
    
    shinyauthr::loginUI((id = "login")),
    # fluid row for settings boxes
    fluidRow(
        # column for the location
        column(
            width = 6,
            # box for setting the location information
            box(
                # box appearance settings
                width = 12,
                title = "Location",
                background = "black",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                
                # drop-down menus for the country, state, and county
                selectInput("country","Country", countryList),
                selectInput("state", "State", c("None")),
                selectInput("county", "County", c("None")),
                
                # box for specific location
                box(
                    # box appearance settings
                    width = 12,
                    background = "black",
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    status = "primary",
                    # switch for enabling specific location
                    title = switchInput("specificlocationtoggle", "Specific Location", size = 'mini', onStatus = "danger", onLabel = "YES", offLabel = "NO"),
                    
                    # switchInput color while on
                    tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-danger,
                                           .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-danger {
                                            background: #004475;
                                            color: white;
                                            }'))),
                    # box for setting the specific location
                    box(
                        # box appearance settings
                        id = "specificLocalBox",
                        background = "black",
                        width = 12,
                        collapsible = TRUE,
                        closable = TRUE,
                        solidHeader = TRUE,
                        status = "primary",
                        title = "Specific Location",
                        
                        # button for opening modal with map to set location
                        actionBttn("InputMapOpen", "Set Location With Map", icon = icon("map"), size = "sm"),
                        # numeric inputs for lat and lng
                        numericInput("latiudeinput", "Latitude", value = 0, min = -90, max = 90, step = .000001),
                        numericInput("longitudeinput", "Longitude", value = 0, min = -180, max = 180, step = .0000001)
                        
                        
                        # end of specific location input box
                    )
                    # end of specific location box
                )
                
                
                # end of main location box
            )
            # end of column for location
        ),
        # column for API key input, radius input, and days back input
        column(
            width = 6,
            
            # box for API key input
            box(
                # box appearance settings
                width = 12,
                title = "API key",
                background = "black",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                
                # text with a link to where you can get a key
                uiOutput("APILink"),
                # text box for inputting key
                textInput("apikey", value = APIkey, label = "")
                # end of API key box
            ),
            
            # box for radius input
            box(
                # box appearance settings
                width = 12,
                title = "Radius",
                background = "black",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                
                # slider for inputting search radius
                sliderInput("radius", "", min = 1, max = 50, value = 25, step = 1)
                # end of radius box
            ),
            
            # box for days back
            box(
                # box appearance settings 
                width = 12,
                title = "Days Back",
                background = "black",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                
                # slider for inputting the amount of days back
                sliderInput("daysback", "", min = 1, max = 30, value = 14, step = 1)
                # end of days back box
            )
            
            # end of column
        )
        # end of fluid row
    )
    #end of settings tab
)
SpeciesListTab <- tabItem(
    tabName = "species", # tab ID
    # box for the species list output
    box(
        # box appearance settings
        title = uiOutput("regionSightingsTitle"),
        background = "black",
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        status = "primary",
        # drop down menu for setting area to get list for
        selectInput("speciesListArea", "", c("County", "State", "Country")),
        # switch for days back on (not used yet)
        switchInput("speciesDateSwitch", "Time Range", size = 'mini', onStatus = "danger"),
        # switchInput color while on
        tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-danger,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-danger {
                                        background: #004475;
                                        color: white;
                                        }'))),
        
        # output for list of species sighted in region
        htmlOutput(outputId = "speciesList")
        # end of species list box
    )
    # end of species tab
)
SpeciesMapTab <- tabItem(
    tabName = "speciesMap", # tab ID
    # box for the map
    box(
        # box appearance settings 
        title = "Sightings",
        background = "black",
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        height = 24,
        status = "primary",
        
        # sets map height to height of window
        tags$style(type = "text/css", "#speciesMap {height: calc(100vh - 80px) !important;}"),
        # map for displaying locations the species been sighted at
        leafletOutput("speciesMap")
        # end of map box
    )
    # end of map tab
)


QuizTab <- tabItem(
    tabName = "quiz",
    
    fluidRow(
        column(
            width = 12,
            box(
                # box appearance settings 
                width = 12,
                title = "What Bird Is This?",
                background = "black",
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                # image of bird
                htmlOutput("quizImage"),
                textOutput("quizScore"),
                radioButtons("guess", "", choices = c(1,2,3,4,5)),
                
                actionButton("quizSubmit", label = "Submit"),
                actionButton("resetQuiz", label = "Reset")
                
            )
        )
    )
)
ui <- function()
{
    dashboardPage(
        
        header = dashboardHeader(title = "Vultr"), 
        #title = tags$a(href='http://ebird.org',
        #tags$img(src='DSC06705.jpg'))
        
        sidebar = dashboardSidebar(
            sidebarMenu(
                id = "tabs",
                
                sidebarSearchForm(textId = "species", buttonId = "speciesSearchButton", label = "Search Species"),
                menuItem(
                    "Account",
                    menuItemOutput("loginName"),
                    
                    menuSubItem("Settings", tabName = "settings", icon = icon("gear", lib = "font-awesome")),
                    shinyauthr::logoutUI("logout")
                ),
                #menuItem("Home", tabName = "home", icon = icon("house", lib = "font-awesome")),
                
                menuItem("Species Search", icon = icon("magnifying-glass", lib = "font-awesome"),
                         menuSubItem("Species Information", tabName = "speciesSearch", icon = icon("info", lib = "font-awesome")),
                         menuSubItem("Map", tabName = "speciesMap", icon = icon("map", lib = "font-awesome"))
                ),
                menuItem("Species", icon = icon("feather", lib = "font-awesome"),
                         menuSubItem("Species List", tabName = "species", icon = icon("feather", lib = "font-awesome")),
                         menuSubItem("Notable sightings", tabName = "notableMapTab", icon = icon("circle-exclamation", lib = "font-awesome")),
                         menuSubItem("Target Map", tabName = "targetMapTab", icon = icon("plus", lib = "font-awesome"))
                ),
                menuItem("Quiz", tabName = "quiz", icon = icon("question", lib = "font-awesome")),
                menuItem("Hotspots", icon = icon("fire", lib = "font-awesome"),
                         menuSubItem("Hotspot Information", tabName = "hotspotInfo", icon = icon("info", lib = "font-awesome")),
                         menuSubItem("Hotspot Map", tabName = "hotspotMap", icon = icon("map", lib = "font-awesome"))
                )
                
                
                
                # end of sidebar menu
            )
            # end of dashboard sidebar
        ),
        
        body = dashboardBody(
            use_theme(colorScheme),
            
            
            
            # holds all the tabs
            tabItems(
                
                tabItem(
                    tabName = "login",
                    shinyauthr::loginUI(
                        "login", 
                        cookie_expiry = cookie_expiry
                    )
                ),
                
                #Species Search tab
                SpeciesInfoTab,
                
                # settings tab
                SettingsTab,
                
                SpeciesListTab,
                
                # tab for map of sighting locations
                SpeciesMapTab,
                
                QuizTab,
                
                tabItem(
                    tabName = "notableMapTab",
                    box(
                        # box appearance settings 
                        title = "Notable Sightings",
                        background = "black",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        width = 12,
                        height = 24,
                        status = "primary",
                        
                        # sets map height to height of window
                        tags$style(type = "text/css", "#notableMap {height: calc(100vh - 80px) !important;}"),
                        # map for displaying locations the species been sighted at
                        actionButton("notableMapReload", label = "Reload"),
                        leafletOutput("notableMap"), 
                        uiOutput("notableList")
                        # end of map box
                    )
                    # end of map tab
                ),
                
                tabItem(
                    tabName = "targetMapTab",
                    box(
                        # box appearance settings 
                        title = "Target Birds",
                        background = "black",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        width = 12,
                        height = 24,
                        status = "primary",
                        
                        # sets map height to height of window
                        tags$style(type = "text/css", "#targetMap {height: calc(100vh - 80px) !important;}"),
                        # map for displaying locations the species been sighted at
                        actionButton("targetMapReload", label = "Reload"),
                        leafletOutput("targetMap"), 
                        uiOutput("targetList")
                        # end of map box
                    )
                    # end of map tab
                ),
                
                tabItem(
                    tabName = "hotspotMap",
                    box(
                        # box appearance settings 
                        title = "",
                        background = "black",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        width = 12,
                        height = 24,
                        status = "primary",
                        
                        # sets map height to height of window
                        tags$style(type = "text/css", "#targetMap {height: calc(100vh - 80px) !important;}"),
                        # map for displaying locations the species been sighted at
                        actionButton("hotspotMapReload", label = "Reload"),
                        numericInput("speciesLimit", "Minimum Number of Species", value = 0),
                        dropdownMenu = boxDropdown(
                            boxDropdownItem("Number of Species Sighted", id = "speciesCountHotspotMap"),
                            boxDropdownItem("Number of New Species", id = "newSpeciesCountHotspotMap"),

                            
                            
                        ),
                        leafletOutput("hotspotMap"), 
                        uiOutput("hotspotList")
                    )
                ),
                
                tabItem(
                    tabName = "hotspotInfo",
                    box(
                        # box appearance settings 
                        title = "",
                        background = "black",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        width = 12,
                        height = 24,
                        status = "primary"
                        
                        
                    )
                )
                
                
                
                # end of tab items
            )
            
            # end of dashboard body
        )
        
        
        # end of dashboard page
    )
    # end of ui
}

# server script


correct <- 0
incorrect <- 0
quizSubmit <- 0
searchVar <- 0
signedInFlag <- FALSE
speciesPhotoIndex <- 1
searchedSpecies <- ""
hotspotMapSettings <- c("speciesCount", "Hybrid")
server <- function(input, output, session)
{
    # log in code
    credentials <- shinyauthr::loginServer(
        id = "login",
        data = user_base,
        user_col = user,
        pwd_col = password_hash,
        sodium_hashed = TRUE,
        cookie_logins = TRUE,
        sessionid_col = sessionid,
        cookie_getter = get_sessions_from_db,
        cookie_setter = add_session_to_db,
        log_out = reactive(logout_init())
    )
    
    # call the logout module with reactive trigger to hide/show
    logout_init <- shinyauthr::logoutServer(
        id = "logout",
        active = reactive(credentials()$user_auth)
        
        
    )
    
    user_info <- reactive({
        credentials()$info
    })
    
    
    
    output$loginName <- renderMenu({
        userdata <- user_info()
        signedInFlag <<-TRUE
        if (!is.null(userdata$name))
        {
            key <- userdata$ebirdKey
            updateTextInput(inputId = "apikey", value = key)
            updateSelectInput(inputId = "country", choices = countryList, selected = userdata$country)
            countryCode <- countrycode(userdata$country,origin = 'country.name', destination = 'iso2c')
            subregion1Tibble <- ebirdsubregionlist(regionType = "subnational1", parentRegionCode = countryCode, key = key)
            statesList <- as.list(subregion1Tibble$name)
            state = userdata$state
            if (length(statesList) > 0)
            {
                updateSelectInput(
                    inputId = "state",
                    label = "State",
                    choices = statesList,
                    selected = state
                )
                stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
                county <- userdata$county
                if (county != "None")
                {
                    countyList <- as.list(ebirdsubregionlist("subnational2", stateCode, key = key)$name)
                    updateSelectInput(
                        inputId = "county",
                        label = "County",
                        choices = countyList,
                        selected = county
                    )
                }
                else
                {
                    updateSelectInput(
                        inputId = "county",
                        label = "County",
                        choices = c("None"),
                        selected = "None"
                    )
                }
            }
            else
            {
                updateSelectInput(
                    inputId = "state",
                    label = "State",
                    choices = c("None"),
                    selected = "None"
                )
            }
            print(userdata$specificLocation)
            # updates the log in tab to be the users name
            
            updateSwitchInput(inputId = "specificlocationtoggle", value = userdata$specificLocation)
            if (userdata$specificLocation)
            {
                updateNumericInput(inputId = "latiudeinput", value = userdata$specificLatitude) # numeric input for lat
                updateNumericInput(inputId = "longitudeinput", value = userdata$specificLongitude) # numeric input for lng
            }
            else
            {
                updateNumericInput(inputId = "latiudeinput", value = NULL) # numeric input for lat
                updateNumericInput(inputId = "longitudeinput", value = NULL) # numeric input for lng
            }
            updateSliderInput(inputId = "radius", value = userdata$radius)
            updateSliderInput(inputId = "daysback", value = userdata$daysBack)
            
            # keep update for menu Item at the very end
            menuSubItem(userdata$name, icon = icon("user", lib = "font-awesome"))
            
        }
        else
        {
            menuSubItem("Log In", tabName = "login", icon = icon("user", lib = "font-awesome"), selected = TRUE)
        }
        
    })
    
    # settings tab
    # map for selecting specific location
    output$locationInMap <- renderLeaflet({
        leaflet() %>% # creates map
            addProviderTiles(providers$Esri.WorldImagery)%>% # adds satellite tiles
            addProviderTiles(providers$Stamen.TonerLabels) # adds labels
    })
    
    # event for setting the specific location when the map is clicked
    observeEvent(input$locationInMap_click, {
        clickInfo <- input$locationInMap_click # object with the click's info
        lat <- clickInfo$lat # lat of click
        lng <- clickInfo$lng # lng of click
        # these while loops normalize the lng to be within the -180 to 180 range
        # ths is run when the user selects the the location too far to the left or right
        while (lng < -180)
        {
            lng <- lng + 360
        }
        while (lng > 180)
        {
            lng <- lng - 360
            
        }
        
        # updates the ui
        updateSelectInput(inputId = "specificlocationtoggle", selected = TRUE) # toggle for specific location setting
        updateNumericInput(inputId = "latiudeinput", value = lat) # numeric input for lat
        updateNumericInput(inputId = "longitudeinput", value = lng) # numeric input for lng
        toggleModal(session = session, "locationInModal") # hides the modal
        
        country1 <- map.where(x = lng, y = lat)
        print(country1)
        if (grepl(":",country1))
        {
            country1 <- unlist(strsplit(country1,":"))[1]
        }
        print(country1)
        pos <- which(iso3166 == country1, arr.ind = TRUE)[1,1]
        country2 <- iso3166[pos, "ISOname"]
        
        updateSelectInput(
            inputId = "country",
            label = "Country",
            choices = countryList,
            selected = country2
        )
        
    })
    
    
    # code to show/hide the specific location box
    observeEvent(input$specificlocationtoggle, {
        selectedState <- input$specificlocationtoggle # gets state of setting
        if (selectedState == FALSE)
        {
            updateBox("specificLocalBox", action = "remove") # hides box
        }
        else if (selectedState == TRUE)
        {
            updateBox("specificLocalBox", action = "restore") # shows box
        }
    })
    
    observeEvent(input$specificLocalBox$visible, {
        boxState <- input$specificLocalBox$visible
        selectedState <- input$specificlocationtoggle # gets state of setting
        if (!boxState & selectedState)
        {
            updateBox("specificLocalBox", action = "restore") # shows box
        }
    })
    
    
    # code to render a hyperlink to the API key website
    output$APILink <- renderUI({
        url <- a("Get API Key", href="https://ebird.org/api/keygen")
        tagList("To obtain a API key go to", url,".")
    }) 
    
    
    
    # code that updates when a new country is selected 
    # it is used to set the list of states in the country and add them to the
    # drop down menu
    observeEvent(c(input$country, input$longitudeinput), {
        key <- input$apikey # key for the ebird API
        if (key != "" & !signedInFlag)
        {
            country <- input$country # full name of country selected
            # 2 char code for the country that ebird uses to look stuff up
            countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c')
            # tibble that rebird returns that is 2 x n (state code, full name)
            statesTibble <- ebirdsubregionlist("subnational1", countryCode, key = key)
            # list that will store all the full names of the states
            statesList <- list()
            # checks to make sure that the tibble retreval worked
            if (nrow(statesTibble) > 0)
            {
                # loops through every row of the tibble and adds the full name to "statesList"
                statesList <- as.list(statesTibble$name)
                
                # updates the SelectInput for the state with the list of states
                
                if (countryCode == "US" & input$specificlocationtoggle == TRUE)
                {
                    state <- str_to_title(map.where(x = input$longitudeinput, y = input$latiudeinput, database = "state"))
                    print(state)
                    if (grepl(":", state))
                        print(state)
                    {
                        if (unlist(strsplit(state, ":"))[1] %in% statesList)
                        {
                            state <- unlist(strsplit(state, ":"))[1]
                        }
                        else
                        {
                            state <- unlist(strsplit(state, ":"))[2]
                        }
                        }
                    print(state)
                    updateSelectInput(
                        inputId = "state",
                        label = "State",
                        choices = statesList,
                        selected = state
                    )
                }
                else 
                {
                    updateSelectInput(
                        inputId = "state",
                        label = "State",
                        choices = statesList,
                        selected = NULL
                    )
                }
                # end of tibble check
            }
            else
            {
                updateSelectInput(
                    inputId = "state",
                    label = "State",
                    choices = c("None"),
                    selected = "None"
                )
                updateSelectInput(
                    inputId = "county",
                    label = "County",
                    choices = c("None"),
                    selected = "None"
                )
            }
        }
        signedInFlag <<- FALSE
        # end of observe event for country selection
    })
    
    
    # code that updates when a new state is selected
    # it is used to generate the list of counties or set them to "None"
    observeEvent(input$state, ignoreInit = TRUE, {
        key <- input$apikey # key for ebird API
        if (key != "" & !signedInFlag)
        {
            state <- input$state # full name of the state
            country <- input$country # full name of the country
            # 2 character country code
            countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c')
            # tibble that rebird returns that is 2 x n (state code, full name)
            subregion1Tibble <- ebirdsubregionlist("subnational1", countryCode, key = key)
            # checks if state is not an empty string
            if (state != "None")
            {
                
                stateCode <- "" # state code
                # goes through the subregion1 tibble to find what the ebird code is
                # for the state
                # this block of code finds the ebird code for the state
                stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
                
                # tibble the rebird that contains the ebird code for the county and 
                # full names of the counties 
                countyTibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
                countyList <- list() # list that will store the full names of the counties
                # checks if there are counties for the state
                if (nrow(countyTibble) > 0)
                {
                    # goes through all the counties and add the full name to the list
                    countyList <- as.list(countyTibble$name)
                    
                    # updates the selectInput for county with the list of counties
                    if (countryCode == "US" & input$specificlocationtoggle == TRUE)
                    {
                        county <- str_to_title(map.where(x = input$longitudeinput, y = input$latiudeinput, database = "county"))
                        print(county)
                        county <- unlist(strsplit(county, ","))[2]
                        
                        updateSelectInput(
                            inputId = "county",
                            label = "County",
                            choices = countyList,
                            selected = county
                        )
                    }
                    else
                    {
                        updateSelectInput(
                            inputId = "county",
                            label = "County",
                            choices = countyList,
                            selected = NULL
                        )
                    }
                    # end of county check
                }
                else # if there are no counties set the choices to "None"
                {
                    updateSelectInput(
                        inputId = "county",
                        label = "County",
                        choices = c("None"),
                        selected = "None"
                        
                    )
                    # end of county check else
                }
                # end of state check
            }
        }
        signedInFlag <<- FALSE
        # end of observe event for state select
    })
    
    # reactive titles
    # code that generates the title of the similar species box
    output$SimilarSpeciesTitle <- renderUI({
        country <- input$country # full name of country
        state <- input$state # full name of state
        county <- input$county # full name of county
        if(county != "None") # if there is a county
        {
            # set location to the county
            locationName <- paste(county, "", sep = "")
        }
        else if(state != "None") # if there is a state
        {
            # set location to state
            locationName <- state
        }
        else 
        {
            # set location to country
            locationName <- country
        }
        # outputs the text in form:
        # Similar Species In {location}
        paste(
            "Similar Species In ",
            locationName,
            sep = ""
        )
        # end of code that generates title for similar species
    })
    
    # code that generates title for  region sightings box
    output$regionSightingsTitle <- renderUI({
        country <- input$country # full name of country
        state <- input$state # full name of state
        county <- input$county # full name of county
        area <- input$speciesListArea # input for country, state, or county
        if(area == "County") # if there is a county and it is selected
        {
            if (county != "None")
            {
                #location is the county
                locationName <- paste(county, "", sep = "")
            }
            else
            {
                locationName <- "ERROR: No County"
            }
        }
        else if(area == "State") # if the state is set and selected
        {
            if (county != "None")
            {
                #location is the state
                locationName <- paste(state, "", sep = "")
            }
            else
            {
                locationName <- "ERROR: No State"
            }
        }
        else if(area == "Country") # if country is selected
        {
            # location is the country
            locationName <- country
        }
        else # this triggers for the case where there is not county but is selected
        {
            # sets location to error for no county
            locationName <- "ERROR"
        }
        # sets the string for the title
        paste(
            "Species Sighted In ",
            locationName,
            sep = ""
        )
        # end of similar species title code
    })
    
    
    
    # reactive script
    observeEvent(ignoreInit = TRUE, c(input$speciesSearchButton), {
        shinyalert("Searching...", "It will take a second.") # searching pop-up
        locationSet <- input$specificlocationtoggle # setting for if the user has set a specific location
        print(as.integer(input$speciesSearchButton))
        speciesInput <- input$species # species input
        searchRad <- input$radius # radius for the search
        amountDaysBack <- input$daysback # amount of days back the api will look back
        key <- input$apikey # key for ebird API
        if (key != "")
        {
            index <- commonToIndex(tolower(speciesInput)) # index for the species in the speciesTibble
            country <- input$country # full name of country
            countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
            state <- input$state # full name of the state
            stateCode <- "" # ebird state code
            county <- input$county # full name of county
            countyCode <- "" # ebird code for the county
            # tibble that contains the codes for the states and the full names
            subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
            
            
            if(state != "None")
            {
                stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
                subregion2Tibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
            }
            else
            {
                subregion2Tibble <- tibble()
            }
            # tibble that has all the codes and names for counties in the state
            # but it will be 0x0 if there are none
            
            # checks if there are counties for the given state
            if (nrow(subregion2Tibble) > 0)
            {
                # if there were counties it goes through and finds the county code
                # for the given input
                
                countyCode <- subregion2Tibble[[as.integer(searchTibble(subregion2Tibble,county)[1]), 1]]
                
                
                if (!locationSet)  # if the user has not set a specific location
                {
                    # this gets the information on the county from rebird
                    countyInfoTibble <- ebirdregioninfo(countyCode, key = key)
                    # calculates the average latitude and longitude for the county
                    latitude <- (countyInfoTibble[[4]] + countyInfoTibble[[5]])/2
                    longitude <- (countyInfoTibble[[2]] + countyInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            else if (state != "None")# if there is not a county
            {
                if (!locationSet)  # if the user has not set a specific location
                {
                    # gets the information on the state from rebird
                    stateInfoTibble <- ebirdregioninfo(stateCode, key = key)
                    # calculates the average latitude and longitude for the state
                    latitude <- (stateInfoTibble[[4]] + stateInfoTibble[[5]])/2
                    longitude <- (stateInfoTibble[[2]] + stateInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            else
            {
                if (!locationSet)  # if the user has not set a specific location
                {
                    # gets the information on the state from rebird
                    countryInfoTibble <- ebirdregioninfo(countryCode, key = key)
                    # calculates the average latitude and longitude for the state
                    latitude <- (countryInfoTibble[[4]] + countryInfoTibble[[5]])/2
                    longitude <- (countryInfoTibble[[2]] + countryInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            
            if (!is.na(index))
            {
                searchedSpecies <<- speciesInput
                # output for species information
                output$SpeciesPhoto <- renderText({
                    photo <- getPhotoSearch(tags = c(speciesInput), per_page = 50, sort = "relevance", api_key = flickerAPIkey, img_size = "w")
                    speciesSearchPhoto <<- photo
                    
                    if (nrow(photo) > 0)
                    {
                        speciesPhotoIndex <<- 1
                        photoIndex <- speciesPhotoIndex
                        
                        serverID <- photo$server[photoIndex]
                        ID <- photo$id[photoIndex]
                        secret <- photo$secret[photoIndex]
                        size = "w"
                        format = "jpg"
                        
                        src= paste(
                            sep = "",
                            "https://live.staticflickr.com/",
                            serverID,
                            "/",
                            ID,
                            "_",
                            secret,
                            "_",
                            size,
                            ".",
                            format
                        )
                        cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
                        print(cite)
                        author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
                        print(author)
                        if (author == "")
                        {
                            author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
                        }
                        c('<img src="', src, '", width="100%">', '<br/>
                  <p>Photo property of 
                  <a href="', cite, '" target="_blank">' ,author, '</a>
                  and API services via
                  <a href="https://www.flickr.com" target="_blank">Flickr</a>
                  </p>')
                    }
                    else
                    {
                        shinyalert("No Photos.")
                    }
                })
                
                output$SpeciesInfoOut <- renderText({ 
                    
                    # checks if the species if valid by making sure that it was found in the species index
                    if (!is.na(index))
                    {
                        # temporary tibble that has the information of the bird
                        tempTibble <- speciesTibble[index, 1:15]
                        # species code that ebird use for look-ups
                        speciesCode <- tempTibble[[1, 3]]
                        # a list with 3 items on the closest sighting, and sightings in the radius
                        cSightingData <- findClosestSighting(speciesCode, searchRad, key, latitude, longitude, amountDaysBack)
                        # HTML code that is use to render the information box body
                        HTML(
                            # turns all the items into one string
                            paste(
                                sep = " ",
                                "Common Name:", tempTibble[1, 2],
                                "<br/>Scientific Name:", tempTibble[1, 1],
                                "<br/>Family:", tempTibble[1, 11],
                                "<br/>Closest Sighting Distance (km):", cSightingData[1],
                                "<br/>Location of Closest Sighting:", cSightingData[2],
                                "<br/>Number of Sightings in", searchRad,"km:", cSightingData[3]
                                # end of paste
                            )
                            # end of HTML
                        )
                        # end of code that runs if the index is NOT FALSE
                    }
                    else
                    {
                        paste("Invalid")
                    }
                    # end of species information
                })
                
                # output for similar species
                if (index != FALSE)
                {
                    if (as.integer(input$speciesSearchButton) != searchVar)
                    {
                        output$SimilarSpeciesOut <- renderUI({
                            speciesInput <- input$species # species input
                            searchRad <- input$radius # radius input
                            key <- input$apikey # key for ebird API
                            
                            # checks if the species is valid by checking if it exists in the species tibble
                            if(index != FALSE)
                            {
                                
                                speciesFamily <- speciesTibble[index, 10][[1,1]] # ebird family code
                                # gets the indices for the birds that are in the family
                                birdFamilyIndices  <- searchSpeciesTibble(10, speciesFamily)
                                # checks to make sure that it worked
                                if(length(birdFamilyIndices) != 0)
                                {
                                    # list that will contain all of the common names for the birds in the same family
                                    birdFamilyName <- list() 
                                    
                                    if (county != "None") # checks if there is a county selected
                                    {
                                        # if there is get the a tibble with all the ebird codes of birds seen in that county
                                        birdsSightedTibble <- ebirdregionspecies(countyCode, key = key)
                                    }
                                    else if (state != "None")# if a county is not selected use the state instead
                                    {
                                        # get the a tibble with all the ebird codes of birds seen in that state
                                        birdsSightedTibble <- ebirdregionspecies(stateCode, key = key)
                                    }
                                    else
                                    {
                                        birdsSightedTibble <- ebirdregionspecies(countryCode, key = key)
                                    }
                                    # list that contains all the codes for birds that have sighted in the region
                                    birdsSightedList <- birdsSightedTibble[[1]]
                                    
                                    # list for the the codes of the birds in the family
                                    familyCodesList <- list()
                                    # adds the code for each bird to the list of codes
                                    for(i in 1:length(birdFamilyIndices))
                                    { 
                                        birdCode <- speciesTibble[birdFamilyIndices [[i]],3][[1,1]]
                                        familyCodesList <- append(familyCodesList, birdCode)
                                        
                                        # end of for loop
                                    }
                                    
                                    # makes a new list of ones that overlap
                                    intersectionCodes <- intersect(birdsSightedList, familyCodesList)
                                    # converts each code in the overlap to the common name
                                    for(i in 1:length(intersectionCodes))
                                    {
                                        birdFamilyName <- append(birdFamilyName, ebirdCodeToCommon(intersectionCodes[[i]]))
                                    }
                                    # HTML code for the body of the similar species box
                                    HTML(
                                        # converts the entire list to one string with a separator
                                        paste(
                                            birdFamilyName,
                                            collapse = "<br/>",
                                            sep = " "
                                            # end of paste
                                        )
                                        # end of HTML
                                    )
                                    # end of code that runs if bird family length is not 0
                                }
                                
                                # end of code that runs if species index is not FALSE
                            }
                        })
                    }
                    
                    # output for sighting locations
                    output$sightingLocations <-renderUI({
                        # temporary tibble that has the information of the bird
                        tempTibble <- speciesTibble[index, 1:15]
                        # species code that ebird use for look-ups
                        speciesCode <- tempTibble[[1, 3]]
                        # list of the locations it has been sighted at
                        sightingLocationsTibble <- findClosestSighting(speciesCode, searchRad, key, latitude, longitude, amountDaysBack)
                        if (length(sightingLocationsTibble) >= 4)
                        {
                            sightingLocationsTibble <- sightingLocationsTibble[[4]]
                            sightingLocationsList <- list()
                            for (i in 1:nrow(sightingLocationsTibble))
                            {
                                sightingLocationsList <- append(sightingLocationsList, sightingLocationsTibble[[i,1]])
                            }
                            
                            HTML(
                                paste(
                                    sightingLocationsList,
                                    collapse = "<br/>"
                                )
                            )
                        }
                    })
                    
                    # map code
                    output$speciesMap <- renderLeaflet({
                        # temporary tibble that has the information of the bird
                        tempTibble <- speciesTibble[index, 1:15]
                        # species code that ebird use for look-ups
                        speciesCode <- tempTibble[[1, 3]]
                        latList <- c(latitude)
                        lngList <- c(longitude)
                        locationNames <- c("User")
                        typeVector <- c("user")
                        data <- findClosestSighting(speciesCode, searchRad, key, latitude, longitude, amountDaysBack)
                        if (length(data) >= 4)
                        {
                            latList <- append(latList, data[[6]])
                            lngList <- append(lngList, data[[5]])
                            locationTibble <- data[[4]]
                            for (i in 1:nrow(locationTibble))
                            {
                                locationNames <- append(locationNames, locationTibble[[i,1]])
                            }
                            
                            for (i in 2:length(lngList))
                            {
                                typeVector <- append(typeVector, "sighting")
                            }
                            dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
                            icons <- awesomeIconList(
                                user = makeAwesomeIcon(
                                    icon = "user",
                                    iconColor = "black",
                                    library = "fa",
                                    markerColor = "darkblue"
                                ),
                                sighting = makeAwesomeIcon(
                                    icon = "binoculars",
                                    iconColor = "black",
                                    library = "fa",
                                    markerColor = "blue",
                                    
                                )
                            )
                            leaflet(data = dataFrame) %>%
                                addProviderTiles(providers$Esri.WorldImagery)%>%
                                addProviderTiles(providers$Stamen.TonerLabels) %>%
                                addAwesomeMarkers(~long, ~lat, icon = ~icons[type], label = ~label)
                        }
                    })
                    
                    # end of reactive search
                }
            }
            else
            {
                shinyalert("Invalid Species", "")
            }
        }
        else
        {
            shinyalert("No API Key Set", "Please Set It In Settings")
        }
        searchVar <<- as.integer(input$speciesSearchButton)
    })
    
    # species tab
    output$speciesList <- renderUI({
        key <- input$apikey # key for the ebird API
        if (key != "")
        {
            country <- input$country # full name of the country
            countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
            state <- input$state # full name of the state
            stateCode <- "" # ebird state code
            county <- input$county # full name of county
            countyCode <- "" # ebird county code
            # tibble that contains all the state codes and full names for the country
            subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
            
            # this block of code finds the ebird code for the state
            if (state != "None")
            {
                stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
                subregion2Tibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
            }
            else 
            {
                subregion2Tibble <- tibble()
            }
            # tibble that contains all the county codes and full names for the state
            
            # this block of code finds the ebird code for the county if there are any counties in the state
            if (nrow(subregion2Tibble) != 0)
            {
                # if there were counties it goes through and finds the county code
                # for the given input
                countyCode <- subregion2Tibble[[as.integer(searchTibble(subregion2Tibble,county)[1]), 1]]
                
            }
            
            
            selection <- input$speciesListArea # selection for the area(County, State, Country)
            
            if (input$speciesDateSwitch == FALSE)
            {
                speciesCodeList <<- c()
                if(county != "None" & selection == "County") # if there is a county and it is selected
                {
                    # set the code list to the species in the county
                    speciesCodeList <<- ebirdregionspecies(countyCode, key = key)[[1]]
                }
                else if(state != "None" & selection == "State") # if there is a state and it is selected
                {
                    # set the code list to the species in the state
                    speciesCodeList <<- ebirdregionspecies(stateCode, key = key)[[1]]
                }
                else # if the selection is country
                {
                    # set the code list to the species in the country
                    speciesCodeList <<- ebirdregionspecies(countryCode, key = key)[[1]]
                }
                
                # converts all of the codes to common names
                if (length(speciesCodeList) != 0)
                {
                    for (i in 1:length(speciesCodeList))
                    {
                        speciesCodeList[i] <<- ebirdCodeToCommon(speciesCodeList[[i]])
                    }
                }
                speciesCodeList <<- sort(speciesCodeList)
            }
            else
            {
                daysBack <- input$daysback
                speciesCodeList <<- list()
                
                if(county != "None" & selection == "County") # if there is a county and it is selected
                {
                    
                    speciesCodeList <<- regionObs(key = key, regionCode = countyCode, back = daysBack)$comName
                }
                else if(state != "None" & selection == "State") # if there is a state and it is selected
                {
                    speciesCodeList <<- regionObs(key = key, regionCode = stateCode, back = daysBack)$comName
                    
                }
                else # if the selection is country
                {
                    speciesCodeList <<- regionObs(key = key, regionCode = countryCode, back = daysBack, )$comName
                    
                }
                
                speciesCodeList <<- sort(unlist(speciesCodeList))
                
            }
            
            
            
            # HTML code for the body of the species list box
            HTML(
                # turns the entire list into one string with a separator
                paste(
                    speciesCodeList,
                    colapase = "<br/>"
                )
            )
        }
    })
    
    # quiz tab
    observeEvent(input$quizSubmit,
                 {
                     if(as.integer(input$quizSubmit) == 1)
                     {
                         correct <<- 0
                         incorrect <<- 0
                     }
                     key <- input$apikey # key for the ebird API
                     if (key != "")
                     {
                         output$quizScore <- renderText({
                             # check previous answer
                             if (as.integer(input$quizSubmit) != quizSubmit)
                             {
                                 quizSubmit <<- as.integer(input$quizSubmit)
                                 guess <- input$guess
                                 
                                 if(guess == TRUE)
                                 {
                                     correct <<- correct + 1
                                 }
                                 else
                                 {
                                     incorrect <<- incorrect + 1
                                 }
                                 
                                 # display score
                                 c("Correct: ", correct, " Incorrect: ", incorrect)
                             }
                             else
                             {
                                 c("Correct: ", correct, " Incorrect: ", incorrect)
                                 
                             }
                         })
                     }
                     
                     output$quizImage <- renderText({
                         
                         
                         
                         # change bird
                         if (length(speciesCodeList) == 0)
                         {
                             
                             if (key != "")
                             {
                                 
                                 country <- input$country # full name of the country
                                 countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
                                 state <- input$state # full name of the state
                                 stateCode <- "" # ebird state code
                                 county <- input$county # full name of county
                                 countyCode <- "" # ebird county code
                                 # tibble that contains all the state codes and full names for the country
                                 subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
                                 
                                 # this block of code finds the ebird code for the state
                                 if (state != "None")
                                 {
                                     stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
                                     subregion2Tibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
                                 }
                                 else 
                                 {
                                     subregion2Tibble <- tibble()
                                 }
                                 # tibble that contains all the county codes and full names for the state
                                 
                                 # this block of code finds the ebird code for the county if there are any counties in the state
                                 if (nrow(subregion2Tibble) != 0)
                                 {
                                     # if there were counties it goes through and finds the county code
                                     # for the given input
                                     countyCode <- subregion2Tibble[[as.integer(searchTibble(subregion2Tibble,county)[1]), 1]]
                                     
                                 }
                                 
                                 
                                 selection <- input$speciesListArea # selection for the area(County, State, Country)
                                 
                                 if (input$speciesDateSwitch == FALSE)
                                 {
                                     speciesCodeList <<- c()
                                     if(county != "None" & selection == "County") # if there is a county and it is selected
                                     {
                                         # set the code list to the species in the county
                                         speciesCodeList <<- ebirdregionspecies(countyCode, key = key)[[1]]
                                     }
                                     else if(state != "None" & selection == "State") # if there is a state and it is selected
                                     {
                                         # set the code list to the species in the state
                                         speciesCodeList <<- ebirdregionspecies(stateCode, key = key)[[1]]
                                     }
                                     else # if the selection is country
                                     {
                                         # set the code list to the species in the country
                                         speciesCodeList <<- ebirdregionspecies(countryCode, key = key)[[1]]
                                     }
                                     
                                     # converts all of the codes to common names
                                     if (length(speciesCodeList) != 0)
                                     {
                                         for (i in 1:length(speciesCodeList))
                                         {
                                             speciesCodeList[i] <<- ebirdCodeToCommon(speciesCodeList[[i]])
                                         }
                                     }
                                     speciesCodeList <<- sort(speciesCodeList)
                                 }
                                 else
                                 {
                                     daysBack <- input$daysback
                                     speciesCodeList <<- list()
                                     
                                     if(county != "None" & selection == "County") # if there is a county and it is selected
                                     {
                                         
                                         speciesCodeList <<- regionObs(key = key, regionCode = countyCode, back = daysBack)$comName
                                     }
                                     else if(state != "None" & selection == "State") # if there is a state and it is selected
                                     {
                                         speciesCodeList <<- regionObs(key = key, regionCode = stateCode, back = daysBack)$comName
                                         
                                     }
                                     else # if the selection is country
                                     {
                                         speciesCodeList <<- regionObs(key = key, regionCode = countryCode, back = daysBack)$comName
                                         
                                     }
                                     
                                     speciesCodeList <<- sort(unlist(speciesCodeList))
                                     
                                 }
                             }
                         }
                         if (key != "")
                         {
                             speciesIndex <- round(runif(1, 1, length(speciesCodeList)))
                             choices <- c(speciesCodeList[speciesIndex], speciesCodeList[round(runif(4, 1, length(speciesCodeList)))])
                             choices <- sample(choices)
                             values <- c()
                             for (i in 1:5)
                             {
                                 if (choices[i] == speciesCodeList[speciesIndex])
                                 {
                                     values <- append(values, TRUE)
                                 }
                                 else
                                 {
                                     values <- append(values, FALSE)
                                 }
                             }
                             updateRadioButtons(inputId = "guess", choiceNames = choices, choiceValues = values)
                             photo <- getPhotoSearch(tags = c(speciesCodeList[speciesIndex]), per_page = 50, sort = "relevance", api_key = flickerAPIkey, img_size = "w")
                             photoIndex <- round(runif(min = 1, max = 3, n = 1))
                             
                             serverID <- photo$server[photoIndex]
                             ID <- photo$id[photoIndex]
                             secret <- photo$secret[photoIndex]
                             size = "w"
                             format = "jpg"
                             
                             src= paste(
                                 sep = "",
                                 "https://live.staticflickr.com/",
                                 serverID,
                                 "/",
                                 ID,
                                 "_",
                                 secret,
                                 "_",
                                 size,
                                 ".",
                                 format
                             )
                             cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
                             print(cite)
                             author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
                             print(author)
                             if (author == "")
                             {
                                 author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
                             }
                             c('<img src="', src, '">', '<br/>
              <p>Photo property of 
              <a href="', cite, '" target="_blank">' ,author, '</a>
              and API services via
              <a href="https://www.flickr.com" target="_blank">Flickr</a>
              </p>')
                         }
                     })
                 })
    
    observeEvent(c(input$resetQuiz),
                 {
                     print(input$tabs)
                     correct <<- 0
                     incorrect <<- 0
                     output$quizScore <- renderText({
                         c("Correct: ", correct, " Incorrect: ", incorrect)
                         
                     })
                     output$quizImage <- renderText({
                         key <- input$apikey # key for the ebird API
                         
                         
                         # change bird
                         if (length(speciesCodeList) == 0)
                         {
                             
                             
                             if (key != "")
                             {
                                 country <- input$country # full name of the country
                                 countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
                                 state <- input$state # full name of the state
                                 stateCode <- "" # ebird state code
                                 county <- input$county # full name of county
                                 countyCode <- "" # ebird county code
                                 # tibble that contains all the state codes and full names for the country
                                 subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
                                 
                                 # this block of code finds the ebird code for the state
                                 if (state != "None")
                                 {
                                     stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
                                     subregion2Tibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
                                 }
                                 else 
                                 {
                                     subregion2Tibble <- tibble()
                                 }
                                 # tibble that contains all the county codes and full names for the state
                                 
                                 # this block of code finds the ebird code for the county if there are any counties in the state
                                 if (nrow(subregion2Tibble) != 0)
                                 {
                                     # if there were counties it goes through and finds the county code
                                     # for the given input
                                     countyCode <- subregion2Tibble[[as.integer(searchTibble(subregion2Tibble,county)[1]), 1]]
                                     
                                 }
                                 
                                 
                                 selection <- input$speciesListArea # selection for the area(County, State, Country)
                                 
                                 if (input$speciesDateSwitch == FALSE)
                                 {
                                     speciesCodeList <<- c()
                                     if(county != "None" & selection == "County") # if there is a county and it is selected
                                     {
                                         # set the code list to the species in the county
                                         speciesCodeList <<- ebirdregionspecies(countyCode, key = key)[[1]]
                                     }
                                     else if(state != "None" & selection == "State") # if there is a state and it is selected
                                     {
                                         # set the code list to the species in the state
                                         speciesCodeList <<- ebirdregionspecies(stateCode, key = key)[[1]]
                                     }
                                     else # if the selection is country
                                     {
                                         # set the code list to the species in the country
                                         speciesCodeList <<- ebirdregionspecies(countryCode, key = key)[[1]]
                                     }
                                     
                                     # converts all of the codes to common names
                                     if (length(speciesCodeList) != 0)
                                     {
                                         for (i in 1:length(speciesCodeList))
                                         {
                                             speciesCodeList[i] <<- ebirdCodeToCommon(speciesCodeList[[i]])
                                         }
                                     }
                                     speciesCodeList <<- sort(speciesCodeList)
                                 }
                                 else
                                 {
                                     daysBack <- input$daysback
                                     speciesCodeList <<- list()
                                     
                                     if(county != "None" & selection == "County") # if there is a county and it is selected
                                     {
                                         
                                         speciesCodeList <<- regionObs(key = key, regionCode = countyCode, back = daysBack)$comName
                                     }
                                     else if(state != "None" & selection == "State") # if there is a state and it is selected
                                     {
                                         speciesCodeList <<- regionObs(key = key, regionCode = stateCode, back = daysBack)$comName
                                         
                                     }
                                     else # if the selection is country
                                     {
                                         speciesCodeList <<- regionObs(key = key, regionCode = countryCode, back = daysBack)$comName
                                         
                                     }
                                     
                                     speciesCodeList <<- sort(unlist(speciesCodeList))
                                     
                                 }
                             }
                         }
                         if (key != "")
                         {
                             speciesIndex <- round(runif(1, 1, length(speciesCodeList)))
                             choices <- c(speciesCodeList[speciesIndex], speciesCodeList[round(runif(4, 1, length(speciesCodeList)))])
                             choices <- sample(choices)
                             values <- c()
                             for (i in 1:5)
                             {
                                 if (choices[i] == speciesCodeList[speciesIndex])
                                 {
                                     values <- append(values, TRUE)
                                 }
                                 else
                                 {
                                     values <- append(values, FALSE)
                                 }
                             }
                             updateRadioButtons(inputId = "guess", choiceNames = choices, choiceValues = values)
                             photo <- getPhotoSearch(tags = c(speciesCodeList[speciesIndex]), per_page = 3, sort = "relevance", api_key = flickerAPIkey, img_size = "w")
                             photoIndex <- round(runif(min = 1, max = 3, n = 1))
                             
                             serverID <- photo$server[photoIndex]
                             ID <- photo$id[photoIndex]
                             secret <- photo$secret[photoIndex]
                             size = "w"
                             format = "jpg"
                             
                             src= paste(
                                 sep = "",
                                 "https://live.staticflickr.com/",
                                 serverID,
                                 "/",
                                 ID,
                                 "_",
                                 secret,
                                 "_",
                                 size,
                                 ".",
                                 format
                             )
                             
                             cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
                             print(cite)
                             author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
                             print(author)
                             if (author == "")
                             {
                                 author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
                             }
                             c('<img src="', src, '">', '<br/>
              <p>Photo property of 
              <a href="', cite, '" target="_blank">' ,author, '</a>
              and API services via
              <a href="flickr.com" target="_blank">Flickr</a>
              </p>')
                         }
                     })
                 })
    
    # notable sightings map
    observeEvent(input$notableMapReload, {
        
        key <- input$apikey
        
        if (key != "")
        {
            locationSet <- input$specificlocationtoggle # setting for if the user has set a specific location
            country <- input$country # full name of country
            countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
            state <- input$state # full name of the state
            stateCode <- "" # ebird state code
            county <- input$county # full name of county
            countyCode <- "" # ebird code for the county
            # tibble that contains the codes for the states and the full names
            subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
            
            
            if(state != "None")
            {
                stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
                subregion2Tibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
            }
            else
            {
                subregion2Tibble <- tibble()
            }
            # tibble that has all the codes and names for counties in the state
            # but it will be 0x0 if there are none
            
            # checks if there are counties for the given state
            if (nrow(subregion2Tibble) > 0)
            {
                # if there were counties it goes through and finds the county code
                # for the given input
                
                countyCode <- subregion2Tibble[[as.integer(searchTibble(subregion2Tibble,county)[1]), 1]]
                
                
                if (!locationSet)  # if the user has not set a specific location
                {
                    # this gets the information on the county from rebird
                    countyInfoTibble <- ebirdregioninfo(countyCode, key = key)
                    # calculates the average latitude and longitude for the county
                    latitude <- (countyInfoTibble[[4]] + countyInfoTibble[[5]])/2
                    longitude <- (countyInfoTibble[[2]] + countyInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            else if (state != "None")# if there is not a county
            {
                if (!locationSet)  # if the user has not set a specific location
                {
                    # gets the information on the state from rebird
                    stateInfoTibble <- ebirdregioninfo(stateCode, key = key)
                    # calculates the average latitude and longitude for the state
                    latitude <- (stateInfoTibble[[4]] + stateInfoTibble[[5]])/2
                    longitude <- (stateInfoTibble[[2]] + stateInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            else
            {
                if (!locationSet)  # if the user has not set a specific location
                {
                    # gets the information on the state from rebird
                    countryInfoTibble <- ebirdregioninfo(countryCode, key = key)
                    # calculates the average latitude and longitude for the state
                    latitude <- (countryInfoTibble[[4]] + countryInfoTibble[[5]])/2
                    longitude <- (countryInfoTibble[[2]] + countryInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            
            
            notableTibble <- ebirdnotable(lat = latitude, lng = longitude, key = key, dist = input$radius, back = input$daysBack)
            print(notableTibble)
            if (nrow(notableTibble) > 0)
            {
                latList <- c(latitude)
                lngList <- c(longitude)
                latList <- append(latList, notableTibble$lat + runif(1, -0.002, 0.002))
                lngList <- append(lngList, notableTibble$lng + runif(1, -0.002, 0.002))
                locationNames <- c("User")
                locationNames <- append(locationNames, paste0(notableTibble$comName, " : ", notableTibble$locName))
                typeVector <- "user"
                
                for (i in 2:length(lngList))
                {
                    typeVector <- append(typeVector, "sighting")
                }
                print(latList)
                print(lngList)
                # data frame with the information
                dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
                icons <- awesomeIconList(
                    user = makeAwesomeIcon(
                        icon = "user",
                        iconColor = "black",
                        library = "fa",
                        markerColor = "darkblue"
                    ),
                    sighting = makeAwesomeIcon(
                        icon = "binoculars",
                        iconColor = "black",
                        library = "fa",
                        markerColor = "blue",
                        
                    )
                )
                output$notableMap <- renderLeaflet({
                    leaflet(data = dataFrame) %>%
                        addProviderTiles(providers$Esri.WorldImagery)%>%
                        addProviderTiles(providers$Stamen.TonerLabels) %>%
                        addAwesomeMarkers(~long, ~lat, icon = ~icons[type], label = ~label)
                    
                })
                output$notableList <- renderUI({
                    
                    notableNames <- as.list(sort(locationNames[2:length(locationNames)]))
                    HTML(
                        # converts the entire list to one string with a separator
                        paste(
                            notableNames,
                            collapse = "<br/>",
                            sep = " "
                            # end of paste
                        )
                        # end of HTML
                    )
                })
            }
        }
        
    })
    
    
    
    
    # target map
    observeEvent(input$targetMapReload, {
        
        key <- input$apikey
        
        if (key != "")
        {
            locationSet <- input$specificlocationtoggle # setting for if the user has set a specific location
            country <- input$country # full name of country
            countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
            state <- input$state # full name of the state
            stateCode <- "" # ebird state code
            county <- input$county # full name of county
            countyCode <- "" # ebird code for the county
            # tibble that contains the codes for the states and the full names
            subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
            
            
            if(state != "None")
            {
                stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
                subregion2Tibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
            }
            else
            {
                subregion2Tibble <- tibble()
            }
            # tibble that has all the codes and names for counties in the state
            # but it will be 0x0 if there are none
            
            # checks if there are counties for the given state
            if (nrow(subregion2Tibble) > 0)
            {
                # if there were counties it goes through and finds the county code
                # for the given input
                
                countyCode <- subregion2Tibble[[as.integer(searchTibble(subregion2Tibble,county)[1]), 1]]
                
                
                if (!locationSet)  # if the user has not set a specific location
                {
                    # this gets the information on the county from rebird
                    countyInfoTibble <- ebirdregioninfo(countyCode, key = key)
                    # calculates the average latitude and longitude for the county
                    latitude <- (countyInfoTibble[[4]] + countyInfoTibble[[5]])/2
                    longitude <- (countyInfoTibble[[2]] + countyInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            else if (state != "None")# if there is not a county
            {
                if (!locationSet)  # if the user has not set a specific location
                {
                    # gets the information on the state from rebird
                    stateInfoTibble <- ebirdregioninfo(stateCode, key = key)
                    # calculates the average latitude and longitude for the state
                    latitude <- (stateInfoTibble[[4]] + stateInfoTibble[[5]])/2
                    longitude <- (stateInfoTibble[[2]] + stateInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            else
            {
                if (!locationSet)  # if the user has not set a specific location
                {
                    # gets the information on the state from rebird
                    countryInfoTibble <- ebirdregioninfo(countryCode, key = key)
                    # calculates the average latitude and longitude for the state
                    latitude <- (countryInfoTibble[[4]] + countryInfoTibble[[5]])/2
                    longitude <- (countryInfoTibble[[2]] + countryInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            
            
            nearbySightingsDF <- nearbyObs(key = key, lat = latitude, lng = longitude, dist = input$radius, back = input$daysback)
            if (nrow(nearbySightingsDF) > 0)
            {
                lifeList <- unlist(str_split(user_info()$lifeList, "[;]"))
                alreadySighted <- nearbySightingsDF$comName %in% lifeList
                print(alreadySighted)
                targetDF <- data.frame(speciesCode = NA, comName = NA, sciName = NA, locId = NA, locName = NA, obsDt = NA, howMany = NA, lat = NA, lng = NA, obsValid = NA, obsReviewed = NA, locationPrivate = NA, subId = NA)
                for (i in 1:nrow(nearbySightingsDF))
                {
                    if (!alreadySighted[i])
                    {
                        targetDF <- rbind(targetDF, nearbySightingsDF[i,])
                    }
                }
                print(targetDF)
                
                latList <- c(latitude)
                lngList <- c(longitude)
                latList <- append(latList, targetDF$lat + runif(1, -0.002, 0.002))
                lngList <- append(lngList, targetDF$lng + runif(1, -0.002, 0.002))
                locationNames <- c("User")
                locationNames <- append(locationNames, paste0(targetDF$comName, " : ", targetDF$locName))
                typeVector <- "user"
                print(latList)
                
                for (i in 2:length(lngList))
                {
                    typeVector <- append(typeVector, "sighting")
                }
                # data frame with the information
                dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
                icons <- awesomeIconList(
                    user = makeAwesomeIcon(
                        icon = "user",
                        iconColor = "black",
                        library = "fa",
                        markerColor = "darkblue"
                    ),
                    sighting = makeAwesomeIcon(
                        icon = "binoculars",
                        iconColor = "black",
                        library = "fa",
                        markerColor = "blue",
                        
                    )
                )
                
                output$targetMap <- renderLeaflet({
                    leaflet(data = dataFrame) %>%
                        addProviderTiles(providers$Esri.WorldImagery)%>%
                        addProviderTiles(providers$Stamen.TonerLabels) %>%
                        addAwesomeMarkers(~long, ~lat, icon = ~icons[type], label = ~label)
                    
                })
                output$targetList <- renderUI({
                    
                    notableNames <- as.list(sort(locationNames[2:length(locationNames)]))
                    HTML(
                        # converts the entire list to one string with a separator
                        paste(
                            notableNames,
                            collapse = "<br/>",
                            sep = " "
                            # end of paste
                        )
                        # end of HTML
                    )
                })
            }
        }
    })
    
    # change species photo
    observeEvent(c(input$previousphotosearch), {
        
        speciesInput <- searchedSpecies
        outHTML <- NA
        
        if (speciesInput != "")
        {
            photo <- speciesSearchPhoto
            if (nrow(photo) > 0)
            {
                if (speciesPhotoIndex > 1)
                {
                    speciesPhotoIndex <<- speciesPhotoIndex - 1
                    
                    photoIndex <- speciesPhotoIndex
                    
                    serverID <- photo$server[photoIndex]
                    ID <- photo$id[photoIndex]
                    secret <- photo$secret[photoIndex]
                    size = "w"
                    format = "jpg"
                    
                    src= paste(
                        sep = "",
                        "https://live.staticflickr.com/",
                        serverID,
                        "/",
                        ID,
                        "_",
                        secret,
                        "_",
                        size,
                        ".",
                        format
                    )
                    cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
                    print(cite)
                    author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
                    print(author)
                    if (author == "")
                    {
                        author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
                    }
                    outHTML <- c('<img src="', src, '", width="100%">', '<br/>
                      <p>Photo property of 
                      <a href="', cite, '" target="_blank">' ,author, '</a>
                      and API services via
                      <a href="https://www.flickr.com" target="_blank">Flickr</a>
                      </p>')
                }
                else
                {
                    shinyalert("First Image.")
                    outHTML <- NA
                }
            }
        }
        if (!is.na(outHTML[[1]]))
        {
            output$SpeciesPhoto <- renderText({
                outHTML
            })
        }
        
    })
    
    observeEvent(c(input$nextphotosearch), {
        
        speciesInput <- searchedSpecies
        outHTML <- NA
        print("1")
        if (speciesInput != "")
        {
            print("2")
            
            photo <- speciesSearchPhoto
            if (nrow(photo) > 0)
            {
                print("3")
                
                if (speciesPhotoIndex < nrow(photo))
                {
                    print("4")
                    
                    speciesPhotoIndex <<- speciesPhotoIndex + 1
                    
                    photoIndex <- speciesPhotoIndex
                    
                    serverID <- photo$server[photoIndex]
                    ID <- photo$id[photoIndex]
                    secret <- photo$secret[photoIndex]
                    size = "w"
                    format = "jpg"
                    
                    src= paste(
                        sep = "",
                        "https://live.staticflickr.com/",
                        serverID,
                        "/",
                        ID,
                        "_",
                        secret,
                        "_",
                        size,
                        ".",
                        format
                    )
                    cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
                    print(cite)
                    author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
                    print(author)
                    if (author == "")
                    {
                        author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
                    }
                    outHTML <- c('<img src="', src, '", width="100%">', '<br/>
                      <p>Photo property of 
                      <a href="', cite, '" target="_blank">' ,author, '</a>
                      and API services via
                      <a href="https://www.flickr.com" target="_blank">Flickr</a>
                      </p>')
                }
                else
                {
                    shinyalert("No More Images.")
                    outHTML <- NA
                }
            }
            
        }
        print("5")
        
        if (!is.na(outHTML[[1]]))
        {
            output$SpeciesPhoto <- renderText({
                outHTML
            })
        }
    })
    
    # hotspot map
    observeEvent(input$speciesCountHotspotMap, {
        hotspotMapSettings[1] <<- "speciesCount"
        print(hotspotMapSettings)
    })
    observeEvent(input$newSpeciesCountHotspotMap, {
        hotspotMapSettings[1] <<- "newSpeciesCount"
        print(hotspotMapSettings)
    })
    observeEvent(input$obsDateHotspotMap, {
        hotspotMapSettings[1] <<- "obsDate"
        print(hotspotMapSettings)
    })
    
    observeEvent(input$hotspotMapReload, {
        print(hotspotMapSettings)
        key <- input$apikey
        
        if (key != "")
        {
            locationSet <- input$specificlocationtoggle # setting for if the user has set a specific location
            country <- input$country # full name of country
            countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
            state <- input$state # full name of the state
            stateCode <- "" # ebird state code
            county <- input$county # full name of county
            countyCode <- "" # ebird code for the county
            # tibble that contains the codes for the states and the full names
            subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
            
            
            if(state != "None")
            {
                stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
                subregion2Tibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
            }
            else
            {
                subregion2Tibble <- tibble()
            }
            # tibble that has all the codes and names for counties in the state
            # but it will be 0x0 if there are none
            
            # checks if there are counties for the given state
            if (nrow(subregion2Tibble) > 0)
            {
                # if there were counties it goes through and finds the county code
                # for the given input
                
                countyCode <- subregion2Tibble[[as.integer(searchTibble(subregion2Tibble,county)[1]), 1]]
                
                
                if (!locationSet)  # if the user has not set a specific location
                {
                    # this gets the information on the county from rebird
                    countyInfoTibble <- ebirdregioninfo(countyCode, key = key)
                    # calculates the average latitude and longitude for the county
                    latitude <- (countyInfoTibble[[4]] + countyInfoTibble[[5]])/2
                    longitude <- (countyInfoTibble[[2]] + countyInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            else if (state != "None")# if there is not a county
            {
                if (!locationSet)  # if the user has not set a specific location
                {
                    # gets the information on the state from rebird
                    stateInfoTibble <- ebirdregioninfo(stateCode, key = key)
                    # calculates the average latitude and longitude for the state
                    latitude <- (stateInfoTibble[[4]] + stateInfoTibble[[5]])/2
                    longitude <- (stateInfoTibble[[2]] + stateInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            else
            {
                if (!locationSet)  # if the user has not set a specific location
                {
                    # gets the information on the state from rebird
                    countryInfoTibble <- ebirdregioninfo(countryCode, key = key)
                    # calculates the average latitude and longitude for the state
                    latitude <- (countryInfoTibble[[4]] + countryInfoTibble[[5]])/2
                    longitude <- (countryInfoTibble[[2]] + countryInfoTibble[[3]])/2
                }
                else # if the user has set a specific location
                {
                    # sets the lat and long to the users input
                    latitude <- input$latiudeinput
                    longitude <- input$longitudeinput
                }
            }
            
            
            hotspotDF <- nearbyHotspots(key = key, lat = latitude, lng = longitude, dist = input$radius)
            print(hotspotDF)
            if (hotspotMapSettings[1] == "speciesCount")
            {
                
                
                if (nrow(hotspotDF) > 0)
                {
                    
                    minSpecies <- input$speciesLimit
                    minSpeciesVec <- c()
                    for (i in 1:nrow(hotspotDF))
                    {
                        if (hotspotDF$speciesCount[i] < minSpecies)
                        {
                            minSpeciesVec <- append(minSpeciesVec, i)
                        }
                    }
                    print(minSpeciesVec)
                    if (length(minSpeciesVec) > 0)
                    {
                        hotspotDF <- hotspotDF[-minSpeciesVec, ]
                    }
                    
                    latList <- c(latitude)
                    lngList <- c(longitude)
                    latList <- append(latList, hotspotDF$lat)
                    lngList <- append(lngList, hotspotDF$lng)
                    locationNames <- c("User")
                    locationNames <- append(locationNames, paste0(hotspotDF$locName))
                    typeVector <- "user"
                    for (i in 1:nrow(hotspotDF))
                    {
                        if (hotspotDF$speciesCount[i] < 20)
                        {
                            typeVector <- append(typeVector, "sighting1") # white
                        }
                        else if (hotspotDF$speciesCount[i] < 40)
                        {
                            typeVector <- append(typeVector, "sighting2") # lightgray
                        }
                        else if (hotspotDF$speciesCount[i] < 60)
                        {
                            typeVector <- append(typeVector, "sighting3") # beige
                        }
                        else if (hotspotDF$speciesCount[i] < 80)
                        {
                            typeVector <- append(typeVector, "sighting4") # lightgreen
                        }
                        else if (hotspotDF$speciesCount[i] < 100)
                        {
                            typeVector <- append(typeVector, "sighting5") # green
                        }
                        else if (hotspotDF$speciesCount[i] < 120)
                        {
                            typeVector <- append(typeVector, "sighting6") # darkgreen
                        }
                        else if (hotspotDF$speciesCount[i] < 140)
                        {
                            typeVector <- append(typeVector, "sighting7") # cadetblue
                        }
                        else if (hotspotDF$speciesCount[i] < 160)
                        {
                            typeVector <- append(typeVector, "sighting8") # lightblue
                        }
                        else if (hotspotDF$speciesCount[i] < 180)
                        {
                            typeVector <- append(typeVector, "sighting9") # blue
                        }
                        else if (hotspotDF$speciesCount[i] < 200)
                        {
                            typeVector <- append(typeVector, "sighting10") # darkblue
                        }
                        else if (hotspotDF$speciesCount[i] < 220)
                        {
                            typeVector <- append(typeVector, "sighting11") # pink
                        }
                        else if (hotspotDF$speciesCount[i] < 240)
                        {
                            typeVector <- append(typeVector, "sighting12") # purple
                        }
                        else if (hotspotDF$speciesCount[i] < 260)
                        {
                            typeVector <- append(typeVector, "sighting13") # darkpurple
                        }
                        else if (hotspotDF$speciesCount[i] < 280)
                        {
                            typeVector <- append(typeVector, "sighting14") # orange
                        }
                        else if (hotspotDF$speciesCount[i] < 300)
                        {
                            typeVector <- append(typeVector, "sighting15") # lightred
                        }
                        else if (hotspotDF$speciesCount[i] < 350)
                        {
                            typeVector <- append(typeVector, "sighting16") # red
                        }
                        else
                        {
                            typeVector <- append(typeVector, "sighting17") # darkred
                        }
                        print(i)
                    }
                    print(latList)
                    print(lngList)
                    # data frame with the information
                    dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
                    icons <- awesomeIconList(
                        user = makeAwesomeIcon(
                            icon = "user",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "darkblue"
                        ),
                        sighting1 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "white",
                            
                        ),
                        sighting2 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "lightgray",
                            
                        ),
                        sighting3 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "beige",
                            
                        ),
                        sighting4 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "lightgreen",
                            
                        ),
                        sighting5 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "green",
                            
                        ),
                        sighting6 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "darkgreen",
                            
                        ),
                        sighting7 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "cadetblue",
                            
                        ),
                        sighting8 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "lightblue",
                            
                        ),
                        sighting9 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "blue",
                            
                        ),
                        sighting10 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "darkblue",
                            
                        ),
                        sighting11 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "pink",
                            
                        ),
                        sighting12 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "purple",
                            
                        ),
                        sighting13 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "darkpurple",
                            
                        ),
                        sighting14 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "orange",
                            
                        ),
                        sighting15 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "lightred",
                            
                        ),
                        sighting16 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "red",
                            
                        ),
                        sighting17 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "darkred",
                            
                        )
                    )
                }
            }
            
            
            
            if (hotspotMapSettings[1] == "newSpeciesCount")
            {
                
                
                if (nrow(hotspotDF) > 0)
                {
                    
                    shinyalert(title = "Loading This Can Take A Bit.") 
                    minSpecies <- input$speciesLimit
                    minSpeciesVec <- c()
                    for (i in 1:nrow(hotspotDF))
                    {
                        if (hotspotDF$speciesCount[i] < minSpecies)
                        {
                            minSpeciesVec <- append(minSpeciesVec, i)
                        }
                    }
                    hotspotDF <- hotspotDF[-minSpeciesVec, ]
                    if (length(minSpeciesVec) > 0)
                    {
                        hotspotDF <- hotspotDF[-minSpeciesVec, ]
                    }
                    latList <- c(latitude)
                    lngList <- c(longitude)
                    latList <- append(latList, hotspotDF$lat)
                    lngList <- append(lngList, hotspotDF$lng)
                    locationNames <- c("User")
                    locationNames <- append(locationNames, paste0(hotspotDF$locName))
                    typeVector <- "user"
                    hotspotNewSpeciesCount <- c()
                    for (i in 1:nrow(hotspotDF))
                    {
                        suppressWarnings({
                            temp <- ebirdregion(loc = hotspotDF$locId[i], key = key, back = input$daysback)
                            if (nrow(temp) > 0)
                            {
                                lifeList <- unlist(str_split(user_info()$lifeList, "[;]"))
                                hotspotNewSpeciesCount <- append(hotspotNewSpeciesCount, length(setdiff(temp$comName, lifeList)))
                            }
                            else
                            {
                                hotspotNewSpeciesCount <- append(hotspotNewSpeciesCount, 0)
                            }
                        })
                    }
                    for (i in 1:nrow(hotspotDF))
                    {
                        if (hotspotNewSpeciesCount[i] == 0)
                        {
                            typeVector <- append(typeVector, "sighting1") # white
                        }
                        else if (hotspotNewSpeciesCount[i] <= 1)
                        {
                            typeVector <- append(typeVector, "sighting2") # lightgray
                        }
                        else if (hotspotNewSpeciesCount[i] <= 3)
                        {
                            typeVector <- append(typeVector, "sighting3") # beige
                        }
                        else if (hotspotNewSpeciesCount[i] <= 5)
                        {
                            typeVector <- append(typeVector, "sighting4") # lightgreen
                        }
                        else if (hotspotNewSpeciesCount[i] <= 10)
                        {
                            typeVector <- append(typeVector, "sighting5") # green
                        }
                        else if (hotspotNewSpeciesCount[i] <= 15)
                        {
                            typeVector <- append(typeVector, "sighting6") # darkgreen
                        }
                        else if (hotspotNewSpeciesCount[i] <= 20)
                        {
                            typeVector <- append(typeVector, "sighting7") # cadetblue
                        }
                        else if (hotspotNewSpeciesCount[i] <= 25)
                        {
                            typeVector <- append(typeVector, "sighting8") # lightblue
                        }
                        else if (hotspotNewSpeciesCount[i] <= 30)
                        {
                            typeVector <- append(typeVector, "sighting9") # blue
                        }
                        else if (hotspotNewSpeciesCount[i] <= 40)
                        {
                            typeVector <- append(typeVector, "sighting10") # darkblue
                        }
                        else if (hotspotNewSpeciesCount[i] <= 50)
                        {
                            typeVector <- append(typeVector, "sighting11") # pink
                        }
                        else if (hotspotNewSpeciesCount[i] <= 60)
                        {
                            typeVector <- append(typeVector, "sighting12") # purple
                        }
                        else if (hotspotNewSpeciesCount[i] <= 70)
                        {
                            typeVector <- append(typeVector, "sighting13") # darkpurple
                        }
                        else if (hotspotNewSpeciesCount[i] <= 80)
                        {
                            typeVector <- append(typeVector, "sighting14") # orange
                        }
                        else if (hotspotNewSpeciesCount[i] <= 100)
                        {
                            typeVector <- append(typeVector, "sighting15") # lightred
                        }
                        else if (hotspotNewSpeciesCount[i] <= 150)
                        {
                            typeVector <- append(typeVector, "sighting16") # red
                        }
                        else
                        {
                            typeVector <- append(typeVector, "sighting17") # darkred
                        }
                    }
                    
                    # data frame with the information
                    dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
                    print(dataFrame[511,])
                    hotspotsWithNone <- c()
                    for (i in 1:nrow(dataFrame))
                    {
                        print(i)
                        if (dataFrame$type[i] == "sighting1")
                        {
                            hotspotsWithNone <- append(hotspotsWithNone, i)
                        }
                    }
                    print(hotspotsWithNone)
                    dataFrame <- dataFrame[-hotspotsWithNone, ]
                    icons <- awesomeIconList(
                        user = makeAwesomeIcon(
                            icon = "user",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "darkblue"
                        ),
                        sighting1 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "black",
                            
                        ),
                        sighting2 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "lightgray",
                            
                        ),
                        sighting3 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "beige",
                            
                        ),
                        sighting4 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "lightgreen",
                            
                        ),
                        sighting5 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "green",
                            
                        ),
                        sighting6 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "darkgreen",
                            
                        ),
                        sighting7 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "cadetblue",
                            
                        ),
                        sighting8 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "lightblue",
                            
                        ),
                        sighting9 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "blue",
                            
                        ),
                        sighting10 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "darkblue",
                            
                        ),
                        sighting11 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "pink",
                            
                        ),
                        sighting12 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "purple",
                            
                        ),
                        sighting13 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "darkpurple",
                            
                        ),
                        sighting14 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "orange",
                            
                        ),
                        sighting15 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "lightred",
                            
                        ),
                        sighting16 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "red",
                            
                        ),
                        sighting17 = makeAwesomeIcon(
                            icon = "fire",
                            iconColor = "black",
                            library = "fa",
                            markerColor = "darkred",
                            
                        )
                    )
                }
            }
            output$hotspotMap <- renderLeaflet({
                leaflet(data = dataFrame) %>%
                    addProviderTiles(providers$Esri.WorldImagery)%>%
                    addProviderTiles(providers$Stamen.TonerLabels) %>%
                    addAwesomeMarkers(~long, ~lat, icon = ~icons[type], label = ~label)
                
            })
        }
        
    })
    
    
    
}

# creates the app
shinyApp(ui = ui, server = server)