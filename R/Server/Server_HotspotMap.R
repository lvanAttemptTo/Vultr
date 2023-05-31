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


