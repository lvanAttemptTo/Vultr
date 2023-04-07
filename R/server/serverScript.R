server <- function(input, output, session)
{
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
		updateNumericInput(inputId = "longitudeinput", value = lng) # numeric inut for lng
		toggleModal(session = session, "locationInModal") # hides the modal
		
		
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
	observeEvent(input$country, {
		key <- input$apikey # key for the ebird APi
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
			for (i in 1:nrow(statesTibble))
			{
				# appends the full name to the list
				statesList <- append(statesList, statesTibble[[i, 2]])
			}
			# updates the SelectInput for the state with the list of states
			updateSelectInput(
				inputId = "state",
				label = "State",
				choices = statesList,
				selected = NULL
			)
			# end of tibble check
		}
		# end of observe event for country selection
	})
	
	# code that updates when a new state is selected
	# it is used to generate the list of counties or set them to "None"
	observeEvent(input$state, {
		key <- input$apikey # key for ebird API
		state <- input$state # full name of the state
		country <- input$country # full name of the country
		# 2 character country code
		countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c')
		# tibble that rebird returns that is 2 x n (state code, full name)
		subregion1Tibble <- ebirdsubregionlist("subnational1", countryCode, key = key)
		# checks if state is not an empty string
		if (state != "")
		{
			
			stateCode <- "" # state code
			# goes through the subregion1 tibble to find what the ebird code is
			# for the state
			for (i in 1:nrow(subregion1Tibble))
			{
				if (subregion1Tibble[[i, 2]] == state)
				{
					stateCode <- subregion1Tibble[[i, 1]]
				}
			}
			
			# tibble the rebird that contains the ebird code for the county and 
			# full names of the counties 
			countyTibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
			countyList <- list() # list that will store the full names of the counties
			# checks if there are counties for the state
			if (nrow(countyTibble) > 0)
			{
				# goes through all the counties and add the full name to the list
				for (i in 1:nrow(countyTibble))
				{
					countyList <- append(countyList, countyTibble[[i, 2]])
				}
				# updates the selectInput for county with the list of counties
				updateSelectInput(
					inputId = "county",
					label = "County",
					choices = countyList,
					selected = NULL
				)
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
		# end of observe event for state select
	})
	
	# code that generates the title of the similar species box
	output$SimilarSpeciesTitle <- renderUI({
		country <- input$country # full name of country
		state <- input$state # full name of state
		county <- input$county # full name of county
		if(county != "None") # if there is a county
		{
			# set location to the county
			locationName <- county
		}
		else if(state != "") # if there is a state
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
		if(county != "None" & area == "County") # if there is a county and it is selected
		{
			# location is the county
			locationName <- county
		}
		else if(state != "" & area == "State") # if the state is set and selected
		{
			# location is the state
			locationName <- state
		}
		else if(area == "Country") # if country is selected
		{
			# location is the country
			locationName <- country
		}
		else # this triggers for the case where there is not county but is selected
		{
			# sets location to error for no county
			locationName <- "ERROR: No County"
		}
		# sets the string for the title
		paste(
			"Species Sighted In ",
			locationName,
			sep = ""
		)
		# end of similar species title code
	})
	
	
	
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
	    
	    # this block of code finds the ebird codes for the state and county
	    for (i in 1:nrow(subregion1Tibble))
	    {
	        # checks if the full name of the input state is the same one as the ith row of the tibble
	        if (subregion1Tibble[[i, 2]] == state)
	        {
	            # if the input state full name matches the full name from the tibble
	            # set the code to the corresponding one in the tibble
	            stateCode <- subregion1Tibble[[i, 1]]
	        }
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
	            
	            if (subregion2Tibble[[i, 2]] == county)
	            {
	                countyCode <- subregion2Tibble[[i, 1]]
	            }
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
	            if(length(birdFamilyIndices ) != 0)
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
	                for(i in 1:length(birdFamilyIndices ))
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
	
	# code that displays all the species that have been sighted in the region
	output$speciesList <- renderUI({
		key <- input$apikey # key for the ebird API
		
		
		country <- input$country # full name of the country
		countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
		state <- input$state # full name of the state
		stateCode <- "" # ebird state code
		county <- input$county # full name of county
		countyCode <- "" # ebird county code
		# tibble that contains all the state codes and full names for the country
		subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
		
		# this block of code finds the ebird code for the state
		for (i in 1:nrow(subregion1Tibble))
		{
			if (subregion1Tibble[[i, 2]] == state)
			{
				stateCode <- subregion1Tibble[[i, 1]]
			}
		}
		# tibble that contains all the county codes and full names for the state
		subregion2Tibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
		# this block of code finds the ebird code for the county if there are any counties in the state
		if (nrow(subregion2Tibble) != 0)
		{
			# if there were counties it goes through and finds the county code
			# for the given input
			for (i in 1:nrow(subregion2Tibble))
			{
				
				if (subregion2Tibble[[i, 2]] == county)
				{
					countyCode <- subregion2Tibble[[i, 1]]
				}
			}
			
		}
		
		
		
		selection <- input$speciesListArea # selection for the area(County, State, Country)
		
		if(countyCode != "None" & selection == "County") # if there is a county and it is selected
		{
			# set the code list to the species in the county
			speciesCodeList <- ebirdregionspecies(countyCode, key = key)[[1]]
		}
		else if(stateCode != "" & selection == "State") # if there is a state and it is selected
		{
			# set the code list to the species in the state
			speciesCodeList <- ebirdregionspecies(stateCode, key = key)[[1]]
		}
		else if(selection == "Country") # if the selection is country
		{
			# set the code list to the species in the country
			speciesCodeList <- ebirdregionspecies(countryCode, key = key)[[1]]
		}
		
		# converts all of the codes to common names
		for (i in 1:length(speciesCodeList))
		{
			speciesCodeList[i] <- ebirdCodeToCommon(speciesCodeList[[i]])
		}
		# HTML code for the body of the species list box
		HTML(
			# turns the entire list into one string with a separator
			paste(
				speciesCodeList,
				colapase = "<br/>"
			)
		)
	})
}