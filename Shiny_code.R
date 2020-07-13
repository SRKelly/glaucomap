#Making Shiny app

#Packages
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(rsconnect)
#Make map
#source("Creating_map.R")

#Shiny UI Code
ui <- fluidPage(
  leafletOutput(outputId = "mymap", height = 720, width = 720),
  p(),
  actionButton("centre_hud", "Centre on Huddersfield"),
  actionButton("centre_glouc", "Centre on Gloucester"),
  actionButton("centre_port", "Centre on Portsmouth"),
  radioButtons("overlays", "Select overlay", choiceNames = c("IMD", "Age", "Afro-Caribbean", "Mean Deviation"), choiceValues = c("IMD", "Age", "Afro", "MD"))
  )

#Shiny server code
server <- function(input, output, session) {
  

  output$mymap <- renderLeaflet({leaflet(data = geo_dat2) %>% 
      addTiles(group = "OSM (default)") %>% 
      setView(lng = -1.884209, lat = 53.73652, zoom = 10)})
  
  observeEvent(input$centre_hud, {leafletProxy("mymap") %>% setView(-1.88, 53.66, zoom = 10) })
  observeEvent(input$centre_glouc, {leafletProxy("mymap") %>% setView(-2.2769, 51.8839, zoom = 9) })
  observeEvent(input$centre_port, {leafletProxy("mymap") %>% setView(-0.8652, 51, zoom = 9) })
  observeEvent(input$overlays, switch(input$overlays, 
                                      IMD = {leafletProxy("mymap", data = geo_dat2) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Mean IMD :", round(mean_IMD)), smoothFactor = 0.1, color = ~IMD_pal(mean_IMD), group = "IMD") %>%
                                          addLegend(position = "bottomright", pal = IMD_pal, values = ~mean_IMD, opacity = 1, title = "IMD", group = "IMD")
                                        },
                                      Age = {leafletProxy("mymap", data = geo_dat2) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Proportion over 60 :", round(age_prop, 2)), smoothFactor = 0.1, color = ~age_pal(age_prop), group = "Proportion over 60") %>%
                                          addLegend(position = "bottomright", pal = age_pal, values = ~age_prop, opacity = 1, title = "Proportion Over 60", group = "Proportion over 60")
                                      },
                                      Afro = {leafletProxy("mymap", data = geo_dat2) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Proportion of Afro-Caribbean :", round(proportion_black, 2)), smoothFactor = 0.1, color = ~Black_pal(proportion_black), group = "Proportion Afro-Caribbean") %>% 
                                          addLegend(position = "bottomright", pal = Black_pal, values = ~proportion_black, opacity = 1, title = "Proportion Afro-Caribbean", group = "Proportion Afro-Caribbean")
                                      },
                                      MD = {leafletProxy("mymap", data = geo_dat2) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Proportion < -12 MD :", round(proportion, 2)), smoothFactor = 0.1, color = ~MD_pal(proportion), group = "Mean Deviation") %>% 
                                          addLegend(position = "bottomright", pal = MD_pal, values = ~proportion, opacity = 1, title = "Proportion of MD <-12", group = "Mean Deviation")
                                      }
    ))
                                 
}

shinyApp(ui, server)
