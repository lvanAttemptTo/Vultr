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
		locationName <- paste(county, "", sep = "")
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