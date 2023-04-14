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
	
	if (input$speciesDateSwitch == FALSE)
	{
	    speciesCodeList <- c()
		if(county != "None" & selection == "County") # if there is a county and it is selected
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
	    if (length(speciesCodeList) != 0)
	    {
    	    for (i in 1:length(speciesCodeList))
    	    {
    	        speciesCodeList[i] <- ebirdCodeToCommon(speciesCodeList[[i]])
    	    }
	    }
	    speciesCodeList <- sort(speciesCodeList)
	}
	else
	{
	    daysBack <- input$daysback
	    speciesCodeList <- list()

	    if(county != "None" & selection == "County") # if there is a county and it is selected
	    {

	        for (i in 0:daysBack)
	        {
	            searchDate <- as.Date(currentDate) - i
	            responseTibble <- ebirdhistorical(loc = countyCode, date = searchDate, key = key)
	            dateList <- as.list(responseTibble$comName)
	            speciesCodeList <- union(speciesCodeList, dateList)
	        }
	    }
	    else if(stateCode != "" & selection == "State") # if there is a state and it is selected
	    {
	        for (i in 0:daysBack)
	        {
	            searchDate <- as.Date(currentDate) - i
	            responseTibble <- ebirdhistorical(loc = stateCode, date = searchDate, key = key)
	            dateList <- as.list(responseTibble$comName)
	            speciesCodeList <- union(speciesCodeList, dateList)
	        }
	    }
	    else if(selection == "Country") # if the selection is country
	    {
	        for (i in 0:daysBack)
	        {
	            searchDate <- as.Date(currentDate) - i
	            responseTibble <- ebirdhistorical(loc = countryCode, date = searchDate, key = key)
	            dateList <- as.list(responseTibble$comName)
	            speciesCodeList <- union(speciesCodeList, dateList)
	        }
	    }
	    
	    speciesCodeList <- sort(unlist(speciesCodeList))

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