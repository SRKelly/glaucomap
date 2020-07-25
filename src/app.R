#Making Shiny app

#Packages
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(rsconnect)
library(rgdal)
library(purrr)
library(htmlwidgets)
library(htmltools)
library(ggplot2)

# Loading in data
geo_dat <- readOGR(dsn = "shapefiles/data.shp", layer = "data")

# Load in functions and css style sheet
source("./functions.R")

geo_dat@data$hover_text_imd <- create_hover_text(geo_dat$mso11nm, geo_dat$num_vfs, "IMD Score: ", round(geo_dat$men_IMD, 2))
geo_dat@data$hover_text_age <- create_hover_text(geo_dat$mso11nm, geo_dat$num_vfs, "Proportion of population over 60: ", geo_dat$age_prp)
geo_dat@data$hover_text_black <- create_hover_text(geo_dat$mso11nm, geo_dat$num_vfs, "Proportion of population Afro-Caribbean: ", as.numeric(geo_dat$prprtn_))
geo_dat@data$hover_text_md <- create_hover_text(geo_dat$mso11nm, geo_dat$num_vfs, "Mean presenting MD: ", round(geo_dat$prsn_MD, 2))

# Rename columns
colnames(geo_dat@data) <- c("msoa01cd", "objectid", "msoa11nm", "msoa11nmw", "st_arsh", "st_lngt", "la_name", "mean_IMD", "proportion_black", "presenting_MD", "proportion", "num_vfs", "age_prop", "hover_text_imd", "hover_text_age", "hover_text_black", "hover_text_md")

# Reference data frame for subplot

ref_df <- geo_dat@data %>% 
  summarise(
    mean_IMD = mean(mean_IMD), 
    proportion_black = mean(proportion_black),
    proportion = mean(proportion),
    age_prop = mean(age_prop),
    which_group = "reference"
  )

# Make colour palattes

IMD_pal <- colorNumeric(palette = "YlOrRd", domain = geo_dat$mean_IMD)
MD_pal <- colorNumeric(palette = "Blues", domain = geo_dat$proportion)
Black_pal <- colorNumeric(palette = "Reds", domain = geo_dat$proportion_black)
age_pal <- colorNumeric(palette = "BuGn", domain = geo_dat$age_prop)


# Fixing bad NA placement on legend
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
        #textOutput(cat("test"))
    ), 
    mainPanel(
      leafletOutput(outputId = "mymap", height = 500, width = 1000)
      )
    ),
  fluidRow(
    # plotOutput("area_info")
  )
  )

#Shiny server code
server <- function(input, output, session) {
  
  #  output$area_table <- renderDataTable({geo_dat@data[geo_dat$msoa01cd == "E02002244",][,c(3,8:12)]})
  # 
  # This would be code to add plots of clicked region
  # 
  #
  # observeEvent(input$mymap_shape_click, {
  #   clicked_msoa <- as.character(point_in_df(input$mymap_shape_click$lng, input$mymap_shape_click$lat, geo_dat))
  #   
  #   output$area_info <- renderPlot({
  #     
  #     clicked_data <- add_column(geo_dat@data[geo_dat$msoa11cd == clicked_msoa, c(3,8:13)], which_group = "clicked")
  #     combined_data <- bind_rows(ref_df, clicked_data)
  #     print(combined_data)
  #     
  #     ggplot(combined_data) + 
  #       geom_bar(aes(x = mean_IMD, y = mean_IMD, fill = which_group), stat = "identity") +
  #       coord_flip() +
  #       theme_bw()
  #     
  #     }
  #     
  #     )
  #   })
  
  output$mymap <- renderLeaflet({leaflet(data = geo_dat) %>% 
      addTiles(group = "OSM (default)") %>% 
      setView(lng = -1.88, lat = 53.66, zoom = 10)})
  
  observeEvent(input$centre_select, switch(input$centre_select,
                                           Huddersfield = {leafletProxy("mymap") %>% setView(-1.88, 53.66, zoom = 10) },
                                           Gloucester = {leafletProxy("mymap") %>% setView(-2.2769, 51.9, zoom = 9) },
                                           Portsmouth = {leafletProxy("mymap") %>% setView(-0.99, 50.9, zoom = 10) }))

  observeEvent(input$overlays, switch(input$overlays, 
                                      IMD = {leafletProxy("mymap", data = geo_dat) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~hover_text_imd, labelOptions = labelOptions(style = style_sheet), smoothFactor = 0.1, color = ~IMD_pal(mean_IMD), group = "IMD") %>%
                                          addLegend(position = "bottomright", pal = IMD_pal, values = ~mean_IMD, opacity = 1, title = "IMD", group = "IMD")
                                        },
                                      Age = {leafletProxy("mymap", data = geo_dat) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~hover_text_age, labelOptions = labelOptions(style = style_sheet), smoothFactor = 0.1, color = ~age_pal(age_prop), group = "Proportion over 60") %>%
                                          addLegend(position = "bottomright", pal = age_pal, values = ~age_prop, opacity = 1, title = "Proportion Over 60", group = "Proportion over 60")
                                      },
                                      Afro = {leafletProxy("mymap", data = geo_dat) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~hover_text_black, labelOptions = labelOptions(style = style_sheet), smoothFactor = 0.1, color = ~Black_pal(proportion_black), group = "Proportion Afro-Caribbean") %>% 
                                          addLegend(position = "bottomright", pal = Black_pal, values = ~proportion_black, opacity = 1, title = "Proportion Afro-Caribbean", group = "<br> Proportion Afro-Caribbean")
                                      },
                                      MD = {leafletProxy("mymap", data = geo_dat) %>% clearShapes() %>% clearControls() %>%
                                          addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~hover_text_md, labelOptions = labelOptions(style = style_sheet), smoothFactor = 0.1, color = ~MD_pal(proportion), group = "Mean Deviation") %>% 
                                          addLegend(position = "bottomright", pal = MD_pal, values = ~proportion, opacity = 1, title = "Proportion of MD <-12", group = "Mean Deviation")
                                      }
    ))
                                 
}

shinyApp(ui, server)
