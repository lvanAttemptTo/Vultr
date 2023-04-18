server <- function(input, output, session)
{
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