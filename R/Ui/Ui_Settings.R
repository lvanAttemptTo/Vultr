SettingsTab <- tabItem(
	tabName = "settings", # tab ID
	# modal for setting specific location
	bsModal("locationInModal", "", "InputMapOpen", size = "large", tags$span(
		leafletOutput("locationInMap"),
		style = "color: #121212")),
	
	shinyauthr::loginUI((id = "login")),
	# fluid row for settings boxes
	fluidRow(
		# column for the location
		column(
			width = 6,
			# box for setting the location information
			box(
				# box appearance settings
				width = 12,
				title = "Location",
				background = "black",
				collapsible = TRUE,
				solidHeader = TRUE,
				status = "primary",
				
				# drop-down menus for the country, state, and county
				selectInput("country","Country", countryList),
				selectInput("state", "State", c("None")),
				selectInput("county", "County", c("None")),
				
				# box for specific location
				box(
					# box appearance settings
					width = 12,
					background = "black",
					collapsible = TRUE,
					solidHeader = TRUE,
					status = "primary",
					# switch for enabling specific location
					title = switchInput("specificlocationtoggle", "Specific Location", size = 'mini', onStatus = "danger", onLabel = "YES", offLabel = "NO"),
					
					# switchInput color while on
					tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-danger,
                                           .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-danger {
                                            background: #004475;
                                            color: white;
                                            }'))),
					# box for setting the specific location
					box(
						# box appearance settings
						id = "specificLocalBox",
						background = "black",
						width = 12,
						collapsible = TRUE,
						closable = TRUE,
						solidHeader = TRUE,
						status = "primary",
						title = "Specific Location",
						
						# button for opening modal with map to set location
						actionBttn("InputMapOpen", "Set Location With Map", icon = icon("map"), size = "sm"),
						# numeric inputs for lat and lng
						numericInput("latiudeinput", "Latitude", value = 0, min = -90, max = 90, step = .000001),
						numericInput("longitudeinput", "Longitude", value = 0, min = -180, max = 180, step = .0000001)
						
						
						# end of specific location input box
					)
					# end of specific location box
				)
				
				
				# end of main location box
			)
			# end of column for location
		),
		# column for API key input, radius input, and days back input
		column(
			width = 6,
			
			# box for API key input
			box(
				# box appearance settings
				width = 12,
				title = "API key",
				background = "black",
				collapsible = TRUE,
				solidHeader = TRUE,
				status = "primary",
				
				# text with a link to where you can get a key
				uiOutput("APILink"),
				# text box for inputting key
				textInput("apikey", value = APIkey, label = "")
				# end of API key box
			),
			
			# box for radius input
			box(
				# box appearance settings
				width = 12,
				title = "Radius",
				background = "black",
				collapsible = TRUE,
				solidHeader = TRUE,
				status = "primary",
				
				# slider for inputting search radius
				sliderInput("radius", "", min = 1, max = 50, value = 25, step = 1)
				# end of radius box
			),
			
			# box for days back
			box(
				# box appearance settings 
				width = 12,
				title = "Days Back",
				background = "black",
				collapsible = TRUE,
				solidHeader = TRUE,
				status = "primary",
				
				# slider for inputting the amount of days back
				sliderInput("daysback", "", min = 1, max = 30, value = 14, step = 1)
				# end of days back box
			)
		
		# end of column
		)
		# end of fluid row
	)
#end of settings tab
)