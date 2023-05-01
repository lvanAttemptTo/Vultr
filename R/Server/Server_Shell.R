server <- function(input, output, session)
{
    # log in code
    source("Server/Server_LogIn.R", local = TRUE)
    
    # settings tab
    source("Server/Server_Settings.R", local = TRUE)
	
	# reactive titles
	source("Server/Server_Titles.R", local = TRUE)
	
	
    # reactive script
	source("Server/Server_Search.R", local = TRUE)

	# species tab
	source("Server/Server_Species.R", local = TRUE)
    
    # quiz tab
    source("Server/Server_Quiz.R", local = TRUE)
    
    #notable sightings map
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
                latList <- c(latitude)
                lngList <- c(longitude)
                latList <- append(latList, notableTibble$lat)
                lngList <- append(lngList, notableTibble$lng)
                locationNames <- c("User")
                locationNames <- append(locationNames, notableTibble$comName)
                typeVector <- "user"
                
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
            }
            output$notableMap <- renderLeaflet({
                leaflet(data = dataFrame) %>%
                    addProviderTiles(providers$Esri.WorldImagery)%>%
                    addProviderTiles(providers$Stamen.TonerLabels) %>%
                    addAwesomeMarkers(~long, ~lat, icon = ~icons[type], label = ~label)
            
            })
            output$notableList <- renderUI({
                notableNames <- notableTibble$comName
                notableNames <- notableNames[!duplicated(notableNames)]
                notableNames <- as.list(notableNames)
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
        
    })

}