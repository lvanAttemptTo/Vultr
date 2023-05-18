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
        
        # if there is any states
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
        
        # tibble of notable sightings
        notableTibble <- ebirdnotable(lat = latitude, lng = longitude, key = key, dist = input$radius, back = input$daysBack)
        if (nrow(notableTibble) > 0) # if there is any
        {
            latVec <- c(latitude) # adds user's lat to vector
            lngVec <- c(longitude) # adds user's lng to vector
            latVec <- append(latVec, notableTibble$lat + runif(1, -0.002, 0.002)) # adds sightings lats to vector with slight offsets
            lngVec <- append(lngVec, notableTibble$lng + runif(1, -0.002, 0.002)) # adds sightings lngs to vector with slight offsets
            locationNames <- c("User") # adds user to the list of marker names
            locationNames <- append(locationNames, paste0(notableTibble$comName, " : ", notableTibble$locName)) # adds species and where they where sighted to location names
            typeVector <- "user" # vector of the marker types
            
            # sets the rest of the types to "sighting"
            for (i in 2:length(lngVec))
            {
                typeVector <- append(typeVector, "sighting")
            }
            # data frame with the information
            dataFrame <- data.frame(lat = latVec, long = lngVec, type = typeVector, label = locationNames)
            
            # icons for the markers
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
            
            # outputs map
            output$notableMap <- renderLeaflet({
                leaflet(data = dataFrame) %>%
                    addProviderTiles(providers$Esri.WorldImagery)%>%
                    addProviderTiles(providers$Stamen.TonerLabels) %>%
                    addAwesomeMarkers(~long, ~lat, icon = ~icons[type], label = ~label)
                
            })
            
            # output for the list of notable sightings
            output$notableList <- renderUI({
                
                # list of location names excluding the first because that is the user
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


