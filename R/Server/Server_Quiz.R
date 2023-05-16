observeEvent(input$quizSubmit,
{
    if(as.integer(input$quizSubmit) == 1)
    {
        correct <<- 0
        incorrect <<- 0
    }
    key <- input$apikey # key for the ebird API
    if (key != "")
    {
        output$quizScore <- renderText({
            # check previous answer
            if (as.integer(input$quizSubmit) != quizSubmit)
            {
                quizSubmit <<- as.integer(input$quizSubmit)
                guess <- input$guess
                
                if(guess == TRUE)
                {
                    correct <<- correct + 1
                }
                else
                {
                    incorrect <<- incorrect + 1
                }
                
                # display score
                c("Correct: ", correct, " Incorrect: ", incorrect)
            }
            else
            {
                c("Correct: ", correct, " Incorrect: ", incorrect)
                
            }
        })
    }
    
    output$quizImage <- renderText({
        
        
        
        # change bird
        if (length(speciesCodeList) == 0)
        {
            
            if (key != "")
            {
            
                country <- input$country # full name of the country
                countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
                state <- input$state # full name of the state
                stateCode <- "" # ebird state code
                county <- input$county # full name of county
                countyCode <- "" # ebird county code
                # tibble that contains all the state codes and full names for the country
                subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
                
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
                
                
                selection <- input$speciesListArea # selection for the area(County, State, Country)
                
                if (input$speciesDateSwitch == FALSE)
                {
                    speciesCodeList <<- c()
                    if(county != "None" & selection == "County") # if there is a county and it is selected
                    {
                        # set the code list to the species in the county
                        speciesCodeList <<- ebirdregionspecies(countyCode, key = key)[[1]]
                    }
                    else if(state != "None" & selection == "State") # if there is a state and it is selected
                    {
                        # set the code list to the species in the state
                        speciesCodeList <<- ebirdregionspecies(stateCode, key = key)[[1]]
                    }
                    else # if the selection is country
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
                    daysBack <- input$daysback
                    speciesCodeList <<- list()
                    
                    if(county != "None" & selection == "County") # if there is a county and it is selected
                    {
                        
                        speciesCodeList <<- regionObs(key = key, regionCode = countyCode, back = daysBack)$comName
                    }
                    else if(state != "None" & selection == "State") # if there is a state and it is selected
                    {
                        speciesCodeList <<- regionObs(key = key, regionCode = stateCode, back = daysBack)$comName
                        
                    }
                    else # if the selection is country
                    {
                        speciesCodeList <<- regionObs(key = key, regionCode = countryCode, back = daysBack)$comName
                        
                    }
                    
                    speciesCodeList <<- sort(unlist(speciesCodeList))
                    
                }
            }
        }
        if (key != "")
        {
            speciesIndex <- round(runif(1, 1, length(speciesCodeList)))
            choices <- c(speciesCodeList[speciesIndex], speciesCodeList[round(runif(4, 1, length(speciesCodeList)))])
            choices <- sample(choices)
            values <- c()
            for (i in 1:5)
            {
                if (choices[i] == speciesCodeList[speciesIndex])
                {
                    values <- append(values, TRUE)
                }
                else
                {
                    values <- append(values, FALSE)
                }
            }
            updateRadioButtons(inputId = "guess", choiceNames = choices, choiceValues = values)
            photo <- getPhotoSearch(tags = c(speciesCodeList[speciesIndex]), per_page = 50, sort = "relevance", api_key = flickerAPIkey)
            photoIndex <- round(runif(min = 1, max = 3, n = 1))
            
            serverID <- photo$server[photoIndex]
            ID <- photo$id[photoIndex]
            secret <- photo$secret[photoIndex]
            size = "w"
            format = "jpg"
            
            src= paste(
                sep = "",
                "https://live.staticflickr.com/",
                serverID,
                "/",
                ID,
                "_",
                secret,
                "_",
                size,
                ".",
                format
                       )
            cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
            print(cite)
            author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
            print(author)
            if (author == "")
            {
                author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
            }
            c('<img src="', src, '">', '<br/>
              <p>Photo property of 
              <a href="', cite, '" target="_blank">' ,author, '</a>
              and API services via
              <a href="https://www.flickr.com" target="_blank">Flickr</a>
              </p>')
        }
    })
})

observeEvent(c(input$resetQuiz),
{
    print(input$tabs)
    correct <<- 0
    incorrect <<- 0
    output$quizScore <- renderText({
        c("Correct: ", correct, " Incorrect: ", incorrect)
        
    })
    output$quizImage <- renderText({
        key <- input$apikey # key for the ebird API
        
        
        # change bird
        if (length(speciesCodeList) == 0)
        {
            
            
            if (key != "")
            {
                country <- input$country # full name of the country
                countryCode <- countrycode(country,origin = 'country.name', destination = 'iso2c') # 2 character code for the country
                state <- input$state # full name of the state
                stateCode <- "" # ebird state code
                county <- input$county # full name of county
                countyCode <- "" # ebird county code
                # tibble that contains all the state codes and full names for the country
                subregion1Tibble <- ebirdsubregionlist("subnational1",  countryCode, key = key)
                
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
                
                
                selection <- input$speciesListArea # selection for the area(County, State, Country)
                
                if (input$speciesDateSwitch == FALSE)
                {
                    speciesCodeList <<- c()
                    if(county != "None" & selection == "County") # if there is a county and it is selected
                    {
                        # set the code list to the species in the county
                        speciesCodeList <<- ebirdregionspecies(countyCode, key = key)[[1]]
                    }
                    else if(state != "None" & selection == "State") # if there is a state and it is selected
                    {
                        # set the code list to the species in the state
                        speciesCodeList <<- ebirdregionspecies(stateCode, key = key)[[1]]
                    }
                    else # if the selection is country
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
                    daysBack <- input$daysback
                    speciesCodeList <<- list()
                    
                    if(county != "None" & selection == "County") # if there is a county and it is selected
                    {
                        
                        speciesCodeList <<- regionObs(key = key, regionCode = countyCode, back = daysBack)$comName
                    }
                    else if(state != "None" & selection == "State") # if there is a state and it is selected
                    {
                        speciesCodeList <<- regionObs(key = key, regionCode = stateCode, back = daysBack)$comName
                        
                    }
                    else # if the selection is country
                    {
                        speciesCodeList <<- regionObs(key = key, regionCode = countryCode, back = daysBack)$comName
                        
                    }
                    
                    speciesCodeList <<- sort(unlist(speciesCodeList))
                    
                }
            }
        }
        if (key != "")
        {
            speciesIndex <- round(runif(1, 1, length(speciesCodeList)))
            choices <- c(speciesCodeList[speciesIndex], speciesCodeList[round(runif(4, 1, length(speciesCodeList)))])
            choices <- sample(choices)
            values <- c()
            for (i in 1:5)
            {
                if (choices[i] == speciesCodeList[speciesIndex])
                {
                    values <- append(values, TRUE)
                }
                else
                {
                    values <- append(values, FALSE)
                }
            }
            updateRadioButtons(inputId = "guess", choiceNames = choices, choiceValues = values)
            photo <- getPhotoSearch(tags = c(speciesCodeList[speciesIndex]), per_page = 3, sort = "relevance", api_key = flickerAPIkey)
            photoIndex <- round(runif(min = 1, max = 3, n = 1))
            
            serverID <- photo$server[photoIndex]
            ID <- photo$id[photoIndex]
            secret <- photo$secret[photoIndex]
            size = "w"
            format = "jpg"
            
            src= paste(
                sep = "",
                "https://live.staticflickr.com/",
                serverID,
                "/",
                ID,
                "_",
                secret,
                "_",
                size,
                ".",
                format
            )
            
            cite <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "url", api_key = flickerAPIkey)$content
            print(cite)
            author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$realname
            print(author)
            if (author == "")
            {
                author <- getPhotoInfo(photo_id = photo$id[photoIndex], output = "all", api_key = flickerAPIkey)$owner$username
            }
            c('<img src="', src, '">', '<br/>
              <p>Photo property of 
              <a href="', cite, '" target="_blank">' ,author, '</a>
              and API services via
              <a href="flickr.com" target="_blank">Flickr</a>
              </p>')
        }
    })
})