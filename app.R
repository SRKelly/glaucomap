#Making Shiny app

#Packages
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(rsconnect)
library(rgdal)

#Make map
#source("Creating_map.R")

#Loading in data
geo_dat <- readOGR(dsn = "shapefiles/data.shp", layer = "data")

#Rename columns
colnames(geo_dat@data) <- c("mso01cd", "objectd", "mso01nm", "ms01nmw", "st_arsh", "st_lngt", "la_name", "mean_IMD", "proportion_black", "presenting_MD", "proportion", "age_prop")

IMD_pal <- colorNumeric(palette = "YlOrRd", domain = geo_dat$mean_IMD)
MD_pal <- colorNumeric(palette = "Blues", domain = geo_dat$proportion)
Black_pal <- colorNumeric(palette = "Reds", domain = geo_dat$proportion_black)
age_pal <- colorNumeric(palette = "BuGn", domain = geo_dat$age_prop)

css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

#Shiny UI Code
ui <- fluidPage(
  titlePanel("Glaucomap"),
  HTML(html_fix),
  sidebarLayout(
      sidebarPanel(
        radioButtons("overlays", "Select overlay", choiceNames = c("IMD", "Age", "Afro-Caribbean", "Mean Deviation"), choiceValues = c("IMD", "Age", "Afro", "MD")),
        selectInput("centre_select", label = "Select Area", choices = c("Huddersfield", "Gloucester", "Portsmouth"), selected = "Huddersfield")
    ), 
    mainPanel(
      leafletOutput(
        outputId = "mymap", height = 500, width = 1000),

      ))
  )

#Shiny server code
server <- function(input, output, session) {
  

  output$mymap <- renderLeaflet({leaflet(data = geo_dat) %>% 
      addTiles(group = "OSM (default)") %>% 
      setView(lng = -1.88, lat = 53.66, zoom = 10)})
  
  observeEvent(input$centre_select, switch(input$centre_select,
                                           Huddersfield = {leafletProxy("mymap") %>% setView(-1.88, 53.66, zoom = 10) },
                                           Gloucester = {leafletProxy("mymap") %>% setView(-2.2769, 52, zoom = 9) },
                                           Portsmouth = {leafletProxy("mymap") %>% setView(-0.8652, 51, zoom = 9) }))

  observeEvent(input$overlays, switch(input$overlays, 
                                      IMD = {leafletProxy("mymap", data = geo_dat) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Mean IMD :", round(mean_IMD)), smoothFactor = 0.1, color = ~IMD_pal(mean_IMD), group = "IMD") %>%
                                          addLegend(position = "bottomright", pal = IMD_pal, values = ~mean_IMD, opacity = 1, title = "IMD", group = "IMD")
                                        },
                                      Age = {leafletProxy("mymap", data = geo_dat) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Proportion over 60 :", round(age_prop, 2)), smoothFactor = 0.1, color = ~age_pal(age_prop), group = "Proportion over 60") %>%
                                          addLegend(position = "bottomright", pal = age_pal, values = ~age_prop, opacity = 1, title = "Proportion Over 60", group = "Proportion over 60")
                                      },
                                      Afro = {leafletProxy("mymap", data = geo_dat) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Proportion of Afro-Caribbean :", round(proportion_black, 2)), smoothFactor = 0.1, color = ~Black_pal(proportion_black), group = "Proportion Afro-Caribbean") %>% 
                                          addLegend(position = "bottomright", pal = Black_pal, values = ~proportion_black, opacity = 1, title = "Proportion Afro-Caribbean", group = "Proportion Afro-Caribbean")
                                      },
                                      MD = {leafletProxy("mymap", data = geo_dat) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Proportion < -12 MD :", round(proportion, 2)), smoothFactor = 0.1, color = ~MD_pal(proportion), group = "Mean Deviation") %>% 
                                          addLegend(position = "bottomright", pal = MD_pal, values = ~proportion, opacity = 1, title = "Proportion of MD <-12", group = "Mean Deviation")
                                      }
    ))
                                 
}

shinyApp(ui, server)
