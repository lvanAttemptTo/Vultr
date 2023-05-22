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



output$loginName <- renderMenu({
    userdata <- user_info()
    signedInFlag <<-TRUE
    if (!is.null(userdata$name))
    {
        key <- userdata$ebirdKey
        updateTextInput(inputId = "apikey", value = key)
        updateSelectInput(inputId = "country", choices = countryList, selected = userdata$country)
        countryCode <- countrycode(userdata$country,origin = 'country.name', destination = 'iso2c')
        subregion1Tibble <- ebirdsubregionlist(regionType = "subnational1", parentRegionCode = countryCode, key = key)
        statesList <- as.list(subregion1Tibble$name)
        state = userdata$state
        if (length(statesList) > 0)
        {
            updateSelectInput(
                inputId = "state",
                label = "State",
                choices = statesList,
                selected = state
            )
            stateCode <- subregion1Tibble[[as.integer(searchTibble(subregion1Tibble, state)[1]), 1]]
            county <- userdata$county
            if (county != "None")
            {
                countyList <- as.list(ebirdsubregionlist("subnational2", stateCode, key = key)$name)
                updateSelectInput(
                    inputId = "county",
                    label = "County",
                    choices = countyList,
                    selected = county
                )
            }
            else
            {
                updateSelectInput(
                    inputId = "county",
                    label = "County",
                    choices = c("None"),
                    selected = "None"
                )
            }
        }
        else
        {
            updateSelectInput(
                inputId = "state",
                label = "State",
                choices = c("None"),
                selected = "None"
            )
        }
        print(userdata$specificLocation)
        # updates the log in tab to be the users name
        
        updateSwitchInput(inputId = "specificlocationtoggle", value = userdata$specificLocation)
        if (userdata$specificLocation)
        {
            updateNumericInput(inputId = "latiudeinput", value = userdata$specificLatitude) # numeric input for lat
            updateNumericInput(inputId = "longitudeinput", value = userdata$specificLongitude) # numeric input for lng
        }
        else
        {
            updateNumericInput(inputId = "latiudeinput", value = NULL) # numeric input for lat
            updateNumericInput(inputId = "longitudeinput", value = NULL) # numeric input for lng
        }
        updateSliderInput(inputId = "radius", value = userdata$radius)
        updateSliderInput(inputId = "daysback", value = userdata$daysBack)
        
        # keep update for menu Item at the very end
        menuSubItem(userdata$name, icon = icon("user", lib = "font-awesome"))
        
    }
    else
    {
        menuSubItem("Log In", tabName = "login", icon = icon("user", lib = "font-awesome"), selected = TRUE)
    }
    
})