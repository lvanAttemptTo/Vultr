# Vultr
## Overview
  Vultr is written in R and compiled and sent to the web in HTML and JS. Vultr is currently in early development with limited abilities but will be frequently updated. The Main libraries that are used for API calls in Vultr are rebird and Pelican. Pelican is a homebrew library that is an extention to the functionality. The GUI is made using Shiny with the ShinyDashboard extention. Account management and authentication is done currently with  shinyauthr, but will hopefully be changed to polished soon. Images are pulled from flicker using their API and the API key is readily supplied. THe maps are done with the leaflet library with integration with shiny.
## Backend
### Rebird
  Rebird is currently the main library used for making API calls, but will be eventually be phased out in favor of Pelican. The general practice for rebird is that you pass in all the user settings, including: range, location, days back, and API key. Before making a call always check the user has inputted a key. 
### Pelican
  Pelican is a homebrew library that was created by Luke VanDeGrift. It is not currently on cran and might not be for a while. It's main goal is to make ebird API calls for Vultr. It currently only has two functions but more will be added over time.
# Frontend
### Shiny
  Shiny is the underlying library in Vultr it's what is used for the GUI and interactivity. Vultr does not use Shiny directly for the UI but it is what allows Vultr to be compiled down to HTML and JS for use on the web. Shiny uses two components to make a GUI. THe first one is the UI this is how shiny knows what to display to the screen and acts as HTML, and CSS. In most cases Vultr uses ShinyDashboard to do this, but in some cases it uses some other packages. The second component to a shiny app is the server, this is what tells the app what to change onscreen when thw user inputs something and what takes in the user's input. The server acts as JS.
### ShinyDashboard
  ShinyDashboard is what is mainly used for the UI component of Vultr. ShinyDashboard allows for more functionality compaired to shiny but is actually just an extention of shiny. Vultr also uses shinyDashboardPlus which adds some features to shiny dashboard.
### Fresh
  Fresh is a package that lets you change the color scheme of ShinyDashboard apps. At some point there will be a light or dark mode switch bu currently there is not.
### Leaflet
  Leaflet is the library that is used for displaying all of the maps in Vultr. The reason we picked leaflet is beacause of its easy use with shiny and multitude of inbuilt tiles. It also allows for easy marker creation and display.
### FlickrAPI
  FlickrAPI is a library that acts
## Website
### Shinyapps

### SQL database



## Code Submission Policies
