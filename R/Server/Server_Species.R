output$speciesList <- renderUI({
    key <- input$apikey # key for the ebird API
    if (key != "")
    {
        print("1")
        country <- input$country # full name of the country
        countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
        state <- input$state # full name of the state
        stateCode <- "" # ebird state code
        county <- input$county # full name of county
        countyCode <- "" # ebird county code
        # tibble that contains all the state codes and full names for the country
        subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
        print("2")
        # this block of code finds the ebird code for the state
        if (state != "None")
        {
            stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
            subregion2Tibble <- ebirdsubregionlist("subnational2", stateCode, key = key)
        }
        else 
        {
            subregion2Tibble <- tibble()
        }
        # tibble that contains all the county codes and full names for the state
        
        # this block of code finds the ebird code for the county if there are any counties in the state
        if (nrow(subregion2Tibble) != 0)
        {
            # if there were counties it goes through and finds the county code
            # for the given input
            countyCode <- subregion2Tibble[[as.integer(searchTibble(subregion2Tibble,county)[1]), 1]]
            
        }
        
        print("3")
        selection <- input$speciesListArea # selection for the area(County, State, Country)
        
        if (input$speciesDateSwitch == FALSE)
        {
            speciesCodeList <<- c()
            if(county != "None" & selection == "County") # if there is a county and it is selected
            {
                # set the code list to the species in the county
                speciesCodeList <<- ebirdregionspecies(countyCode, key = key)[[1]]
            }
            else if(stateCode != "" & selection == "State") # if there is a state and it is selected
            {
                # set the code list to the species in the state
                speciesCodeList <<- ebirdregionspecies(stateCode, key = key)[[1]]
            }
            else if(selection == "Country") # if the selection is country
            {
                # set the code list to the species in the country
                speciesCodeList <<- ebirdregionspecies(countryCode, key = key)[[1]]
            }
            
            # converts all of the codes to common names
            if (length(speciesCodeList) != 0)
            {
                for (i in 1:length(speciesCodeList))
                {
                    speciesCodeList[i] <<- ebirdCodeToCommon(speciesCodeList[[i]])
                }
            }
            speciesCodeList <<- sort(speciesCodeList)
        }
        else
        {
            print("4")
            daysBack <- input$daysback
            speciesCodeList <<- list()
            
            if(county != "None" & selection == "County") # if there is a county and it is selected
            {
                speciesCodeList <- regionObs(key = key, regionCode = countyCode)$comName
            }
            else if(stateCode != "" & selection == "State") # if there is a state and it is selected
            {
                speciesCodeList <- regionObs(key = key, regionCode = stateCode)$comName
            }
            else if(selection == "Country") # if the selection is country
            {
                speciesCodeList <- regionObs(key = key, regionCode = countryCode)$comName
            }
            print("5")
            speciesCodeList <<- sort(unlist(speciesCodeList))
            print("6")
            
        }
        
        
        
        # HTML code for the body of the species list box
        HTML(
            # turns the entire list into one string with a separator
            paste(
                speciesCodeList,
                colapase = "<br/>"
            )
        )
    }
})