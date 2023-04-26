server <- function(input, output, session)
{
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

}