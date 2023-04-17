# reactive search stuff goes here:
observeEvent(ignoreInit = TRUE, c(input$speciesSearchButton), {
    shinyalert("Searching...", "It will take a second.") # searching pop-up
    locationSet <- input$specificlocationtoggle # setting for if the user has set a specific location
    
    speciesInput <- input$species # species input
    searchRad <- input$radius # radius for the search
    amountDaysBack <- input$daysback # amount of days back the api will look back
    key <- input$apikey # key for ebird API
    index <- commonToIndex(speciesInput) # index for the species in the speciesTibble
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
    }
    # tibble that has all the codes and names for counties in the state
    # but it will be 0x0 if there are none
    subregion2Tibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
    # checks if there are counties for the given state
    if (nrow(subregion2Tibble) != 0)
    {
        # if there were counties it goes through and finds the county code
        # for the given input
        for (i in 1:nrow(subregion2Tibble))
        {
            
            countyCode <- subregion2Tibble[[as.integer(searchTibble(subregion2Tibble,county)[1]), 1]]
        }
        
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
    else # if there is not a county
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
    
    
    # output for species information
    output$SpeciesInfoOut <- renderText({ 
        
        # checks if the species if valid by making sure that it was found in the species index
        if (index != FALSE)
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
                
                if (countyCode != "None") # checks if there is a county selected
                {
                    # if there is get the a tibble with all the ebird codes of birds seen in that county
                    birdsSightedTibble <- ebirdregionspecies(countyCode, key = key)
                }
                else # if a county is not selected use the state instead
                {
                    # get the a tibble with all the ebird codes of birds seen in that state
                    birdsSightedTibble <- ebirdregionspecies(stateCode, key = key)
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
    
    # output for sighting locations
    output$sightingLocations <-renderUI({
        # temporary tibble that has the information of the bird
        tempTibble <- speciesTibble[index, 1:15]
        # species code that ebird use for look-ups
        speciesCode <- tempTibble[[1, 3]]
        # list of the locations it has been sighted at
        sightingLocationsTibble <- findClosestSighting(speciesCode, searchRad, key, latitude, longitude, amountDaysBack)[[4]]
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
    })
    # end of reactive search
})