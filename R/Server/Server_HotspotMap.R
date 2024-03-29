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
        
        
        hotspotDF <<- nearbyHotspots(key = key, lat = latitude, lng = longitude, dist = input$radius, back = input$daysback)
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
                typeVector <- 0
                
                print(latList)
                print(lngList)
                typeVector <- append(typeVector, hotspotDF$speciesCount)
                locationNames <- append(locationNames, paste0(hotspotDF$locName, ", ", typeVector[2:length(typeVector)], " Species."))
                
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
                mrckColor <- markerColor(max(typeVector))
                output$hotspotMap <- renderLeaflet({
                    leaflet(data = dataFrame) %>%
                        addProviderTiles(providers$Esri.WorldImagery) %>%
                        addProviderTiles(providers$Stamen.TonerLabels) %>%
                        addCircleMarkers(lng = ~long[2:nrow(dataFrame)], lat = ~lat[2:nrow(dataFrame)], color = ~mrckColor(type[2:nrow(dataFrame)]), label = ~label[2:nrow(dataFrame)], fillOpacity = 1, radius = 5) %>%
                        addAwesomeMarkers(~longitude, ~latitude, icon = ~icons["user"], label = "User") %>%
                        addLegend_decreasing("bottomright", pal = mrckColor, values = ~type[2:nrow(dataFrame)], title = "Species", opacity = 1, decreasing = TRUE, bins = 10)
                    
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
                
                typeVector <- 0
                hotspotNewSpeciesCount <- c()
                lifeList <- unlist(str_split(user_info()$lifeList, "[;]"))
                if (!is.null(lifeList))
                {
                    for (i in 1:nrow(hotspotDF))
                    {
                        
                        print(lifeList)
                        suppressWarnings({
                            temp <- ebirdregion(loc = hotspotDF$locId[i], key = key, back = input$daysback)
                            
                            if (nrow(temp) > 0)
                            {
                                
                                hotspotNewSpeciesCount <- append(hotspotNewSpeciesCount, length(setdiff(temp$comName, lifeList)))
                            }
                            else
                            {
                                hotspotNewSpeciesCount <- append(hotspotNewSpeciesCount, 0)
                            }
                            if (hotspotDF$locName[i] == "Keāhole Point")
                            {
                                print(temp)
                                print(length(setdiff(temp$comName, lifeList)))
                            }
                        })
                    }
                }
                else
                {
                    hotspotNewSpeciesCount <- append(hotspotNewSpeciesCount, hotspotDF$speciesCount)
                }
                print(hotspotNewSpeciesCount)
                
                typeVector <- append(typeVector, hotspotNewSpeciesCount)
                locationNames <- append(locationNames, paste0(hotspotDF$locName, ", ", typeVector[2:length(typeVector)], " New Species."))
                
                # data frame with the information
                dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
                print(dataFrame)
                # for (i in 1:length(hotspotNewSpeciesCount))
                # {
                #     print(i)
                #     if (hotspotNewSpeciesCount[i] == 0)
                #     {
                #         hotspotsWithNone <- append(hotspotsWithNone, i)
                #     }
                # }
                # if (length(hotspotsWithNone) > 0)
                # {
                #     dataFrame <- dataFrame[-hotspotsWithNone, ]
                #     typeVector <- append(typeVector, hotspotNewSpeciesCount[-hotspotsWithNone])
                # }
                # else
                # {
                #     typeVector <- append(typeVector, hotspotNewSpeciesCount)
                # }
                
                icons <- awesomeIconList(
                    user = makeAwesomeIcon(
                        icon = "user",
                        iconColor = "black",
                        library = "fa",
                        markerColor = "darkblue"
                    )
                )
                mrckColor <- markerColor(max(dataFrame$type))
                if (max(dataFrame$type) < 10)
                {
                    numBins <- max(dataFrame$type)
                }
                else
                {
                    numBins <- 10
                    
                }
                print(numBins)
                output$hotspotMap <- renderLeaflet({
                    leaflet(data = dataFrame) %>%
                        addProviderTiles(providers$Esri.WorldImagery) %>%
                        addProviderTiles(providers$Stamen.TonerLabels) %>%
                        addCircleMarkers(lng = ~long[2:nrow(dataFrame)], lat = ~lat[2:nrow(dataFrame)], color = ~mrckColor(type[2:nrow(dataFrame)]), label = ~label[2:nrow(dataFrame)], fillOpacity = 1, radius = 5) %>%
                        addAwesomeMarkers(~longitude, ~latitude, icon = ~icons["user"], label = "User") %>%
                        addLegend_decreasing("bottomright", pal = mrckColor, values = ~type[2:nrow(dataFrame)], title = "New Species", opacity = 1, decreasing = TRUE, bins = numBins)
                    
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
                
                typeVector <- 0
                daysback = input$daysback
                typeVector <- append(typeVector, difftime(currentDate, hotspotDF$lastSighting, units = "days"))
                locationNames <- append(locationNames, paste0(hotspotDF$locName, ", Last Visited ", floor(typeVector[2:length(typeVector)]), " Days Ago."))
                
                dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
                icons <- awesomeIconList(
                    user = makeAwesomeIcon(
                        icon = "user",
                        iconColor = "black",
                        library = "fa",
                        markerColor = "darkblue"
                    )
                )
                mrckColor <- markerColorDate(daysback)
                output$hotspotMap <- renderLeaflet({
                    leaflet(data = dataFrame) %>%
                        addProviderTiles(providers$Esri.WorldImagery) %>%
                        addProviderTiles(providers$Stamen.TonerLabels) %>%
                        addCircleMarkers(lng = ~long[2:nrow(dataFrame)], lat = ~lat[2:nrow(dataFrame)], color = ~mrckColor(type[2:nrow(dataFrame)]), label = ~label[2:nrow(dataFrame)], fillOpacity = 1, radius = 5) %>%
                        addAwesomeMarkers(~longitude, ~latitude, icon = ~icons["user"], label = "User") %>%
                        addLegend_decreasing("bottomright", pal = mrckColor , values = ~type[2:nrow(dataFrame)], title = "How Many Days Ago", opacity = 1, decreasing = TRUE, bins = 10)
                    
                })
            }
        }
            
        
    }
    
})

observeEvent(input$hotspotMap_marker_click, {
    latMatches <- which(hotspotDF$lat == input$hotspotMap_marker_click$lat, arr.ind = TRUE)
    lngMatches <- which(hotspotDF$lng == input$hotspotMap_marker_click$lng, arr.ind = TRUE)
    index <- intersect(latMatches, lngMatches)

   
    if (length(index) == 1)
    {
        hotspot <- hotspotDF[index, ]
        print(hotspot)
        hotspotExtraInfo <- ebirdregion(loc = hotspot$locId, key = input$apikey, back = input$daysback)
        hotspotSpeciesEver <- ebirdregionspecies(loc = hotspot$locId, key = input$apikey)$speciesCode
        for (i in 1:length(hotspotSpeciesEver))
        {
            hotspotSpeciesEver[i] <- ebirdCodeToCommon(hotspotSpeciesEver[i])
        }
        print(hotspotExtraInfo)
        if (nrow(hotspotExtraInfo) > 0 & length(hotspotSpeciesEver))
        {
            lifeList <- unlist(str_split(user_info()$lifeList, "[;]"))
            if (!is.null(lifeList))
            {
                newSpecies <- setdiff(hotspotExtraInfo$comName, lifeList)
                newSpeciesEver <- setdiff(hotspotSpeciesEver, lifeList)
            }
            else
            {
                newSpecies <- hotspotExtraInfo$comName
                newSpeciesEver <- hotspotSpeciesEver
            }
            print(hotspotExtraInfo)
            hotspotInfoText <- paste0(
                "Number of Species: ",
                hotspot$speciesCount,
                "<br/>Last Sighting: ",
                hotspot$lastSighting,
                "<br/>Number of Species In Last ",
                input$daysback,
                " Days: ",
                length(hotspotExtraInfo$comName),
                "<br/>Number of New Species In Last ",
                input$daysback,
                " Days: ",
                length(newSpecies),
                "<br/>New Species In Last ",
                input$daysback,
                " Days:<br/>",
                paste(newSpecies, collapse = "<br/>"),
                "<br/><br/>Species in last ",
                input$daysback,
                " Days:<br/>",
                paste(hotspotExtraInfo$comName, collapse = "<br/>"),
                "<br/><br/>Number of New Species: ",
                length(newSpeciesEver),
                "<br/>New Species:<br/>",
                paste(newSpeciesEver, collapse = "<br/>"),
                "<br/><br/>Number of Species: ",
                length(hotspotSpeciesEver),
                "<br/>Species:<br/>",
                paste(hotspotSpeciesEver, collapse = "<br/>")

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
})
