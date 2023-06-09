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
        else {
            hotspotDF2 <- nearbyHotspots(key = key, lat = latitude, lng = longitude, dist = input$radius, back = input$daysback)
            hotspotDF2$locName <- tolower(hotspotDF2$locName)
            hotspotIndex <- which(hotspotDF2$locName == tolower(speciesInput), arr.ind = TRUE)
            print(hotspotIndex)
            if (length(hotspotIndex) == 1)
            {
                hotspot <- hotspotDF2[hotspotIndex, ]
                print(hotspot)
                hotspotExtraInfo <- ebirdregion(loc = hotspot$locId, key = key, back = input$daysback)
                if (nrow(hotspotExtraInfo) > 0)
                {
                    lifeList <- unlist(str_split(user_info()$lifeList, "[;]"))
                    if (!is.null(lifeList))
                    {
                        newSpecies <- setdiff(hotspotExtraInfo$comName, lifeList)
                    }
                    else
                    {
                        newSpecies <- hotspotExtraInfo$comName
                    }
                    hotspotInfoText <- paste0(
                        "Number of Species: ",
                        hotspot$speciesCount,
                        "<br/>Last Sighting: ",
                        hotspot$lastSighting,
                        "<br/>Number of New Species: ",
                        length(newSpecies),
                        "<br/>New Species:<br/>",
                        paste(newSpecies, collapse = "<br/>"),
                        "<br/>Species:<br/>",
                        paste(hotspotExtraInfo$comName, collapse = "<br/>")
                        
                        
                    )
                }
                else
                {
                    hotspotInfoText <- paste0(
                        "Number of Species: ",
                        hotspot$speciesCount,
                        "<br/>Last Sighting: ",
                        hotspot$lastSighting
                        
                    )
                }
                output$hotspotInfoTitle <- renderUI({
                    str_to_title(hotspot$locName)
                })
                output$hotspotInfo <- renderUI({
                    HTML(hotspotInfoText)
                })
            }
        }
        # else
        # {
        #     shinyalert("Invalid Species", "")
        # }
    }
    else
    {
        shinyalert("No API Key Set", "Please Set It In Settings")
    }
    searchVar <<- as.integer(input$speciesSearchButton)
})