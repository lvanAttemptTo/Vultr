credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password_hash,
    sodium_hashed = TRUE,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessions_from_db,
    cookie_setter = add_session_to_db,
    log_out = reactive(logout_init())
)

# call the logout module with reactive trigger to hide/show
logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
    
    
)

user_info <- reactive({
    credentials()$info
})


# output that shows the users name or the login menu
output$loginName <- renderMenu({
    userdata <- user_info() # gets the user's data
    signedInFlag <<- TRUE # sets the flag for being signed in to TRUE 
    if (!is.null(userdata$name))
    {
        key <- userdata$ebirdKey # ebird API Key of user
        updateTextInput(inputId = "apikey", value = key) # sets ebird API Key
        updateSelectInput(inputId = "country", choices = countryList, selected = userdata$country) # set user's country
        countryCode <- countrycode(userdata$country,origin = 'country.name', destination = 'iso2c')
        subregion1Tibble <- ebirdsubregionlist(regionType = "subnational1", parentRegionCode = countryCode, key = key) # tibble of subreagions in the country
        statesList <- as.list(subregion1Tibble$name) # sets the state list
        state = userdata$state # gets the user's state
        if (length(statesList) > 0) # checks if there are any states in the country
        {
            # updates the state 
            updateSelectInput(
                inputId = "state",
                label = "State",
                choices = statesList,
                selected = state
            )
            stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
            county <- userdata$county # gets the user's county
            if (county != "None") # checks if there is a county 
            {
                # list of all the counties
                countyList <- as.list(ebirdsubregionlist("subnational2", stateCode, key = key)$name)
                # sets the county
                updateSelectInput(
                    inputId = "county",
                    label = "County",
                    choices = countyList,
                    selected = county
                )
            }
            else # else triggers if there is no county
            {
                # sets it to none
                updateSelectInput(
                    inputId = "county",
                    label = "County",
                    choices = c("None"),
                    selected = "None"
                )
            }
        }
        else # else triggers if there is no state
        {
            # sets to none
            updateSelectInput(
                inputId = "state",
                label = "State",
                choices = c("None"),
                selected = "None"
            )
        }
        # updates the log in tab to be the users name
        
        # sets the toggle to the user's set value
        updateSwitchInput(inputId = "specificlocationtoggle", value = userdata$specificLocation)
        # if they have it set, set the specific location
        if (userdata$specificLocation)
        {
            updateNumericInput(inputId = "latiudeinput", value = userdata$specificLatitude) # numeric input for lat
            updateNumericInput(inputId = "longitudeinput", value = userdata$specificLongitude) # numeric input for lng
        }
        else # if not set them to NULL
        {
            updateNumericInput(inputId = "latiudeinput", value = NULL) # numeric input for lat
            updateNumericInput(inputId = "longitudeinput", value = NULL) # numeric input for lng
        }
        # sets the user's other settings
        updateSliderInput(inputId = "radius", value = userdata$radius)
        updateSliderInput(inputId = "daysback", value = userdata$daysBack)
        
        # keep update for menu Item at the very end
        menuSubItem(userdata$name, icon = icon("user", lib = "font-awesome"))
        
    }
    else # if the the user is not signed in
    {
        # show the login tab option
        menuSubItem("Log In", tabName = "login", icon = icon("user", lib = "font-awesome"), selected = TRUE)
    }
    
})
