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
  output$mymap <- renderLeaflet({leaflet(data = geo_dat2) %>% 
      addTiles(group = "OSM (default)") %>% 
      setView(lng = -1.884209, lat = 53.73652, zoom = 10) %>% 
      addPolygons(stroke = FALSE, fillOpacity = 0.9, color = ~IMD_pal(mean_IMD), group = "IMD") %>% 
      addPolygons(stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0.2, color = ~MD_pal(presenting_MD), group = "Mean Deviation") %>% 
      addLayersControl(overlayGroups = c("IMD", "Mean Deviation"), options = layersControlOptions(collapsed = FALSE))
    })
}

shinyApp(ui, server)
