NotableTab <- tabItem(
    tabName = "notableMapTab",
    box(
        # box appearance settings 
        title = "Notable Sightings",
        background = "black",
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        height = 24,
        status = "primary",
        
        # sets map height to height of window
        tags$style(type = "text/css", "#speciesMap {height: calc(100vh - 80px) !important;}"),
        # map for displaying locations the species been sighted at
        actionButton("notableMapReload", label = "Reload"),
        leafletOutput("notableMap"), 
        uiOutput("notableList")
        # end of map box
    )
    # end of map tab
)