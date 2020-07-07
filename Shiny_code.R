#Making Shiny app

#Packages
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

#Make map
source("Creating_map.R")

#Shiny UI Code
ui <- fluidPage(leafletOutput(outputId = "mymap"))

#Shiny server code
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({leaflet(data = geo_dat2) %>% addTiles(group = "OSM (default)") %>% addPolygons(stroke = FALSE, fillOpacity = 0.9, color = ~pal(mean_IMD), group = "IMD") %>% 
      addLayersControl(overlayGroups = c("IMD"))})
}

shinyApp(ui, server)
