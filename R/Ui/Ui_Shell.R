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
source("Ui/Ui_Notable.R", local = TRUE)
ui <- function()
{
    dashboardPage(

        header = dashboardHeader(title = "Vultr"), 
        #title = tags$a(href='http://ebird.org',
        #tags$img(src='DSC06705.jpg'))
        
        sidebar = dashboardSidebar(
            sidebarMenu(
                id = "tabs",
                
                sidebarSearchForm(textId = "species", buttonId = "speciesSearchButton", label = "Search Species"),
                menuItem(
                    "Account",
                     menuItemOutput("loginName"),
                     
                     menuSubItem("Settings", tabName = "settings", icon = icon("gear", lib = "font-awesome")),
                     shinyauthr::logoutUI("logout")
                ),
                #menuItem("Home", tabName = "home", icon = icon("house", lib = "font-awesome")),
                
                menuItem("Species Search", icon = icon("magnifying-glass", lib = "font-awesome"),
                     menuSubItem("Species Information", tabName = "speciesSearch", icon = icon("info", lib = "font-awesome")),
                     menuSubItem("Map", tabName = "speciesMap", icon = icon("location-dot", lib = "font-awesome"))
                ),
                menuItem("Species", icon = icon("feather", lib = "font-awesome"),
                    menuSubItem("Species List", tabName = "species", icon = icon("feather", lib = "font-awesome")),
                    menuSubItem("Notable sightings", tabName = "notableMapTab", icon = icon("circle-exclamation", lib = "font-awesome")),
                    menuSubItem("Target Species", tabName = "targetSpecies", icon = icon("plus", lib = "font-awesome"))
                ),
                menuItem("Quiz", tabName = "quiz", icon = icon("question", lib = "font-awesome"))

                
                
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
                
                NotableTab,
                
                tabItem(
                    tabName = "targetSpecies",
                    box(
                        title = "Target Species",
                        background = "black",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        width = 12,
                        height = 24,
                        status = "primary",
                        
                        # sets map height to height of window
                        tags$style(type = "text/css", "#speciesMap {height: calc(100vh - 80px) !important;}"),
                        # map for displaying locations the species been sighted at
                        actionButton("targetMapReload", label = "Reload"),
                        leafletOutput("targetMap"), 
                        uiOutput("targetList")
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
