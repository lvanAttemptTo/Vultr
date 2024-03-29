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
source("Ui/Ui_SpeciesInfo.R", local = TRUE)
source("Ui/Ui_Settings.R", local = TRUE)
source("Ui/Ui_SpeciesList.R", local = TRUE)
source("Ui/Ui_SpeciesMap.R", local = TRUE)
source("Ui/Ui_Quiz.R", local = TRUE)
ui <- function()
{
    dashboardPage(

        header = dashboardHeader(title = "Vultr"), 
        #title = tags$a(href='http://ebird.org',
        #tags$img(src='DSC06705.jpg'))
        
        sidebar = dashboardSidebar(
            sidebarMenu(
                id = "tabs",
                
                sidebarSearchForm(textId = "species", buttonId = "speciesSearchButton", label = "Search..."),
                menuItem(
                    "Account",
                     menuItemOutput("loginName"),
                     
                     menuSubItem("Settings", tabName = "settings", icon = icon("gear", lib = "font-awesome")),
                     shinyauthr::logoutUI("logout")
                ),
                #menuItem("Home", tabName = "home", icon = icon("house", lib = "font-awesome")),
                
                menuItem("Species Search", icon = icon("magnifying-glass", lib = "font-awesome"),
                     menuSubItem("Species Information", tabName = "speciesSearch", icon = icon("info", lib = "font-awesome")),
                     menuSubItem("Map", tabName = "speciesMap", icon = icon("map", lib = "font-awesome"))
                ),
                menuItem("Species", icon = icon("feather", lib = "font-awesome"),
                    menuSubItem("Species List", tabName = "species", icon = icon("feather", lib = "font-awesome")),
                    menuSubItem("Notable sightings", tabName = "notableMapTab", icon = icon("circle-exclamation", lib = "font-awesome")),
                    menuSubItem("Target Map", tabName = "targetMapTab", icon = icon("plus", lib = "font-awesome"))
                ),
                menuItem("Quiz", tabName = "quiz", icon = icon("question", lib = "font-awesome")),
                menuItem("Hotspots", icon = icon("fire", lib = "font-awesome"),
                         menuSubItem("Hotspot Information", tabName = "hotspotInfo", icon = icon("info", lib = "font-awesome")),
                     menuSubItem("Hotspot Map", tabName = "hotspotMap", icon = icon("map", lib = "font-awesome"))
                 )

                
                
                # end of sidebar menu
            )
            # end of dashboard sidebar
        ),
        
        body = dashboardBody(
            use_theme(colorScheme),
            
            
            
            # holds all the tabs
            tabItems(
                
                tabItem(
                    tabName = "login",
                    shinyauthr::loginUI(
                        "login", 
                        cookie_expiry = cookie_expiry
                    )
                ),
                
                #Species Search tab
                SpeciesInfoTab,
                
                # settings tab
                SettingsTab,
                
                SpeciesListTab,
                
                # tab for map of sighting locations
                SpeciesMapTab,
                
                QuizTab,
                
                tabItem(
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
                        tags$style(type = "text/css", "#notableMap {height: calc(100vh - 80px) !important;}"),
                        # map for displaying locations the species been sighted at
                        actionButton("notableMapReload", label = "Reload"),
                        leafletOutput("notableMap"), 
                        uiOutput("notableList")
                        # end of map box
                    )
                    # end of map tab
                ),
                
                tabItem(
                	tabName = "targetMapTab",
                	box(
                		# box appearance settings 
                		title = "Target Birds",
                		background = "black",
                		collapsible = TRUE,
                		solidHeader = TRUE,
                		width = 12,
                		height = 24,
                		status = "primary",
                		
                		# sets map height to height of window
                		tags$style(type = "text/css", "#targetMap {height: calc(100vh - 80px) !important;}"),
                		# map for displaying locations the species been sighted at
                		actionButton("targetMapReload", label = "Reload"),
                		leafletOutput("targetMap"), 
                		uiOutput("targetList")
                		# end of map box
                	)
                	# end of map tab
                ),
                
                tabItem(
                    tabName = "hotspotMap",
                    box(
                        # box appearance settings 
                        title = "",
                        background = "black",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        width = 12,
                        height = 24,
                        status = "primary",
                        
                        # sets map height to height of window
                        tags$style(type = "text/css", "#targetMap {height: calc(100vh - 80px) !important;}"),
                        # map for displaying locations the species been sighted at
                        actionButton("hotspotMapReload", label = "Reload"),
                        numericInput("speciesLimit", "Minimum Number of Species", value = 0),
                        dropdownMenu = boxDropdown(
                            boxDropdownItem("Number of Species Sighted", id = "speciesCountHotspotMap"),
                            boxDropdownItem("Number of New Species", id = "newSpeciesCountHotspotMap"),
                            boxDropdownItem("How Recently Visited", id = "obsDateHotspotMap")
                            
                            
                        ),
                        leafletOutput("hotspotMap")
                    )
                ),
                
                tabItem(
                    tabName = "hotspotInfo",
                    box(
                        # box appearance settings 
                        title = uiOutput("hotspotInfoTitle"),
                        background = "black",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        width = 12,
                        height = 24,
                        status = "primary",
                        
                        uiOutput("hotspotInfo")
                    )
                )
                
                
                
            # end of tab items
            )
        
        # end of dashboard body
        )
        
        
    # end of dashboard page
    )
# end of ui
}
