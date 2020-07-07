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
  output$mymap <- renderLeaflet({leaflet(geo_dat2) %>% addTiles() %>% addPolygons(stroke = FALSE)})
}

shinyApp(ui, server)
