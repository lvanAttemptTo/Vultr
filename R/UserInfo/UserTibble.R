user_base <- tibble::tibble(
    
    # username
    username = c("luke vandegrift"),
    
    # user password
    password = c("test"),
    
    # permissions for the user (owner, admin, user)
    permissions = c("owner"),
    
    # display name for the user
    displayName = c("Luke VanDeGrift"),
    
    # general location (country:state:county, county and state can be "None")
    location = c("United States:Oregon:Multnomah"),
    
    # specific location (latitude,longitude or FALSE if setting is off)
    specificLocation = c("0,0"),
    
    # ebird API key
    ebirdAPIKey = c("vmgu1o6c6ihc"),
    
    # search settings (radius,days back)
    searchSettings = c("25,14")
)