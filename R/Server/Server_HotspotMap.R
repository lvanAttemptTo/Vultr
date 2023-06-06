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
        dataFrame <- NA
        
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
        
        
        hotspotDF <- nearbyHotspots(key = key, lat = latitude, lng = longitude, dist = input$radius, back = input$daysback)
        print(hotspotDF)
        if (hotspotMapSettings[1] == "speciesCount")
        {
                
                
            if (nrow(hotspotDF) > 1)
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
                typeVector <- 0
                
                print(latList)
                print(lngList)
                typeVector <- append(typeVector, hotspotDF$speciesCount)
                
                # data frame with the information
                dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
                icons <- awesomeIconList(
                    user = makeAwesomeIcon(
                        icon = "user",
                        iconColor = "black",
                        library = "fa",
                        markerColor = "darkblue"
                    )
                )
                output$hotspotMap <- renderLeaflet({
                    leaflet(data = dataFrame) %>%
                        addProviderTiles(providers$Esri.WorldImagery) %>%
                        addProviderTiles(providers$Stamen.TonerLabels) %>%
                        addCircleMarkers(lng = ~long, lat = ~lat, color = ~markerColor(type), label = ~label, fillOpacity = 1, radius = 5) %>%
                        addAwesomeMarkers(~long[1], ~lat[1], icon = ~icons["user"], label = "User") %>%
                        addLegend_decreasing("bottomright", pal = markerColor, values = ~type, title = "Species", opacity = 1, decreasing = TRUE, bins = 10)
                    
                })
            }
        }
        else if (hotspotMapSettings[1] == "newSpeciesCount")
        {
            
            
            if (nrow(hotspotDF) > 1)
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
                typeVector <- 0
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
                print(hotspotNewSpeciesCount)
                
                typeVector <- append(typeVector, hotspotNewSpeciesCount)
                
                # data frame with the information
                dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
                hotspotsWithNone <- c()
                for (i in 1:nrow(dataFrame))
                {
                    print(i)
                    if (hotspotNewSpeciesCount[i] == 0)
                    {
                        hotspotsWithNone <- append(hotspotsWithNone, i)
                    }
                }
                if (length(hotspotsWithNone) > 0)
                {
                    dataFrame <- dataFrame[-hotspotsWithNone, ]
                }
                icons <- awesomeIconList(
                    user = makeAwesomeIcon(
                        icon = "user",
                        iconColor = "black",
                        library = "fa",
                        markerColor = "darkblue"
                    )
                )
                output$hotspotMap <- renderLeaflet({
                    leaflet(data = dataFrame) %>%
                        addProviderTiles(providers$Esri.WorldImagery) %>%
                        addProviderTiles(providers$Stamen.TonerLabels) %>%
                        addCircleMarkers(lng = ~long, lat = ~lat, color = ~markerColor(type), label = ~label, fillOpacity = 1, radius = 5) %>%
                        addAwesomeMarkers(~long[1], ~lat[1], icon = ~icons["user"], label = "User") %>%
                        addLegend_decreasing("bottomright", pal = markerColor, values = ~type, title = "New Species", opacity = 1, decreasing = TRUE, bins = 10)
                    
                })
            }
        }
        else if (hotspotMapSettings[1] == "obsDate")
        {
            if (nrow(hotspotDF) > 1)
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
                typeVector <- 0
                daysback = input$daysback
                typeVector <- append(typeVector, difftime(currentDate, hotspotDF$lastSighting, units = "days"))
                
                
                dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
                icons <- awesomeIconList(
                    user = makeAwesomeIcon(
                        icon = "user",
                        iconColor = "black",
                        library = "fa",
                        markerColor = "darkblue"
                    )
                )
                output$hotspotMap <- renderLeaflet({
                    leaflet(data = dataFrame) %>%
                        addProviderTiles(providers$Esri.WorldImagery) %>%
                        addProviderTiles(providers$Stamen.TonerLabels) %>%
                        addCircleMarkers(lng = ~long, lat = ~lat, color = ~markerColor(500 - ((500 / daysback) * type)), label = ~label, fillOpacity = 1, radius = 5) %>%
                        addAwesomeMarkers(~long[1], ~lat[1], icon = ~icons["user"], label = "User") %>%
                        addLegend_decreasing("bottomright", pal = markerColorDate(daysback) , values = ~type, title = "How Many Days Ago", opacity = 1, decreasing = TRUE, bins = 10)
                    
                })
            }
        }
            
        
    }
    
})


