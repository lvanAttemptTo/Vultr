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