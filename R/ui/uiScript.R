# custom color scheme 
colorScheme <- create_theme(
	adminlte_color(
		light_blue = "#004475",
		
	),
	adminlte_sidebar(
		dark_bg = "#121212",
		dark_hover_bg = "#002b4a",
		dark_color = "#2E3440"
	),
	adminlte_global(
		content_bg = "#2e2e2e",
		box_bg = "#121212", 
		info_box_bg = "#121212"
	)
)
ebirdRegionName <- "Multnomah County"
ebirdRegionCode <- "US-OR-051"

ui <- function()
{    
	dashboardPage(
		skin = "blue",
		
		header = dashboardHeader(title = tags$a(href='http://ebird.org',
						tags$img(src='DSC06705.jpg'))), 
		
		sidebar = dashboardSidebar(
			sidebarMenu(
				sidebarSearchForm(textId = "species", buttonId = "speciesSearchButton", label = "Search Species"),
				#menuItem("Home", tabName = "home", icon = icon("house", lib = "font-awesome")),
				menuItem("Species Information", tabName = "speciesSearch", icon = icon("magnifying-glass", lib = "font-awesome")),
				menuItem("Settings", tabName = "settings", icon = icon("gear", lib = "font-awesome")),
				menuItem("Species", tabName = "species", icon = icon("feather", lib = "font-awesome")),
				menuItem("Map", tabName = "map", icon = icon("location-dot", lib = "font-awesome"))
				
				# end of sidebar menu
			)
			# end of dashboard sidebar
		),
		
		body = dashboardBody(
			use_theme(colorScheme),
			# holds all the tabs
			tabItems(
				
				# # home tab
				# tabItem(
				#     tabName = "home",
				#     
				# ),
				
				#Species Search tab
				tabItem(
					tabName = "speciesSearch", # tab ID
					
					# fluid row for boxes
					fluidRow(
						
						# column for sighting locations
						column(
							width = 6, # covers half the width
							# box that has a list of all the locations that the species has been sighted at
							box(
								# box appearance settings 
								width = 12,
								title = "Sightings Locations",
								background = "black",
								collapsible = TRUE,
								solidHeader = TRUE,
								status = "primary",
								
								# output for the locations list
								uiOutput("sightingLocations") 
							)
							# end of column for sighting locations
						),
						# column for similar species and species info
						column(
							width = 6, # column covers half the width
							
							# box for species info
							box(
								# box appearance settings
								width = 12,
								title = "Species Info",
								background = "black",
								collapsible = TRUE,
								solidHeader = TRUE,
								status = "primary",
								
								# output for the species info text
								htmlOutput("SpeciesInfoOut")
							),
							
							
							# box for similar species output
							box(
								# box appearance settings
								width = 12,
								title = uiOutput("SimilarSpeciesTitle"),
								background = "black",
								collapsible = TRUE,
								solidHeader = TRUE,
								status = "primary",
								
								# output for the similar species list
								uiOutput("SimilarSpeciesOut")
							)
							# end of column for species info and simlar species
						)
						# end of fluid row
					)
					# end of tab for species search
				),
				
				# settings tab
				tabItem(
					tabName = "settings", # tab ID
					# modal for setting specific location
					bsModal("locationInModal", "", "InputMapOpen", size = "large", tags$span(
						leafletOutput("locationInMap"),
						style = "color: #121212")),
					
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
								selectInput("state", "State", c()),
								selectInput("county", "County", c()),
								
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
								textInput("apikey", "", value = "vmgu1o6c6ihc")
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
				),
				
				tabItem(
					tabName = "species", # tab ID
					# box for the species list output
					box(
						# box appearance settings
						title = uiOutput("regionSightingsTitle"),
						background = "black",
						collapsible = TRUE,
						solidHeader = TRUE,
						width = 12,
						status = "primary",
						# drop down menu for setting area to get list for
						selectInput("speciesListArea", "", c("County", "State", "Country")),
						# switch for days back on (not used yet)
						switchInput("speciesDateSwitch", "Time Range", size = 'mini', onStatus = "danger"),
						# switchInput color while on
						tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-danger,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-danger {
                                        background: #004475;
                                        color: white;
                                        }'))),
						
						# output for list of species sighted in region
						htmlOutput(outputId = "speciesList")
						# end of species list box
					)
					# end of species tab
				),
				
				# tab for map of sighting locations
				tabItem(
					tabName = "map", # tab ID
					# box for the map
					box(
						# box appearance settings 
						title = "Sightings",
						background = "black",
						collapsible = TRUE,
						solidHeader = TRUE,
						width = 12,
						height = 24,
						status = "primary",
						
						# sets map height to height of window
						tags$style(type = "text/css", "#speciesMap {height: calc(100vh - 80px) !important;}"),
						# map for displaying locations the species been sighted at
						leafletOutput("speciesMap")
						# end of map box
					)
					# end of map tab
				)
				# end of tab items
			)
			
			# end of dashboard body
		)
		
		
		# end of dashboard page
	)
	# end of ui
}