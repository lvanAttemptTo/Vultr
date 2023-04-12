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

