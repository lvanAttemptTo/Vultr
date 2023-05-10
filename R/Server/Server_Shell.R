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
    
    # notable sightings tab
    source("Server/Server_Notable.R", local = TRUE)

    
    observeEvent(input$targetMapReload, {
        key <- input$apikey
        if (key != "")
        {
            latList
            lngList
            typeVector
            locationNames
            
            dataFrame <- data.frame(lat = latList, long = lngList, type = typeVector, label = locationNames)
            icons <- awesomeIconList(
                user = makeAwesomeIcon(
                    icon = "user",
                    iconColor = "black",
                    library = "fa",
                    markerColor = "darkblue"
                ),
                sighting = makeAwesomeIcon(
                    icon = "binoculars",
                    iconColor = "black",
                    library = "fa",
                    markerColor = "blue",
                    
                )
            )
            
        }
        
        # output the leaflet map
        output$notableMap <- renderLeaflet({
            leaflet(data = dataFrame) %>%
                addProviderTiles(providers$Esri.WorldImagery)%>%
                addProviderTiles(providers$Stamen.TonerLabels) %>%
                addAwesomeMarkers(~long, ~lat, icon = ~icons[type], label = ~label)
            
        })
    })
}