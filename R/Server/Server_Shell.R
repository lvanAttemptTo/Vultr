server <- function(input, output, session)
{
    # log in code
    source("Server/Server_LogIn.R", local = TRUE)
    
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
    
    # notable sightings map
    source("Server/Server_NotableMap.R", local = TRUE)
    
    # target map
    source("Server/Server_TargetMap.R", local = TRUE)
    
    # change species photo
    source("Server/Server_ChangeSpeciesPhoto.R", local = TRUE)
    
    # hotspot map
    source("Server/Server_HotspotMap.R", local = TRUE)
}