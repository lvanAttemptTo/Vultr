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
)