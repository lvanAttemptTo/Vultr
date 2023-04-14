SpeciesListTab <- tabItem(
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
)