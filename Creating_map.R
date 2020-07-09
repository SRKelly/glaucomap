#Glaucoma SES 2020
#Packages
library(rgdal)
library(leaflet)
library(maps)
library(dplyr)
#Setting working directory
setwd("C:\\Users\\Steph\\Documents\\PhD Related Files\\glaucoma_ses")

#Load in functions
source("R Code\\functions.R")

#Loading in kml MSOA file of England and Wales
geo_dat <- readOGR("C:\\Users\\Steph\\Documents\\PhD Related Files\\glaucoma_ses\\Data", layer = "Middle_Layer_Super_Output_Areas__December_2001__Boundaries")
#Load in patient data
vf_dat <- read.csv("C:\\Users\\Steph\\Documents\\PhD Related Files\\VFs\\Older Medisoft data\\All_24_2s.csv")[,1:42]

#Load in postcode data
postcode1 <- read.csv("Data\\postcode1.csv")[,c(2,4,6)]
postcode2 <- read.csv("Data\\postcode2.csv")[,c(2,4,6)]
postcodes <- rbind(postcode1, postcode2)
rm(list = c("postcode1", "postcode2"))

#Removing space from postcode and merging
vf_dat$Postcode <- gsub(" ", "", vf_dat$Postcode)
postcodes$PCD8 <- gsub(" ", "", postcodes$PCD8)
vf_dat2 <- merge(vf_dat, postcodes, by.x = "Postcode", by.y = "PCD8", all.x = TRUE) #Merge error? 

#Loading in IMD data and merging
IMD_dat <- read.csv("Data\\IMD.csv")
IMD_dat <- IMD_dat[,c("LSOA.code..2011.", "Index.of.Multiple.Deprivation..IMD..Score")]
vf_dat3 <- merge(vf_dat2, IMD_dat, by.x = "LSOA11CD", by.y = "LSOA.code..2011.", all.x = TRUE)

#Subset MSOAs we want to plot
huddersfield <- c("Kirklees", "Calderdale")
cheltenham <- c("Forest of Dean", "Gloucester", "Cheltenham", "Tewkesbury", "Stroud", "Cotswold", "Wychavon", "Malvern Hills", "Worcester")
portsmouth <- c("Portsmouth", "Gosport", "Fareham", "Havant", "Winchester", "Chichester", "East Hampshire")
geo_dat$la_name <- trimws(gsub("[0-9]+", "", geo_dat$msoa01nm), "r")
geo_dat <- geo_dat[geo_dat$la_name %in% c(huddersfield, portsmouth, cheltenham),]

#Calculate statistics by MSOA
IMD_res <- vf_dat3 %>% group_by(MSOA11CD) %>% summarise(mean_IMD = mean(Index.of.Multiple.Deprivation..IMD..Score))
geo_dat@data <- merge(geo_dat@data, IMD_res, by.x = "msoa01cd", by.y = "MSOA11CD", all.x = TRUE)

MD_res <- vf_dat3 %>% group_by(MSOA11CD, patient_pseudoid, DateOfTest) %>% summarise(presenting_MD = MeanDeviation[1])
MD_res <- MD_res %>% group_by(MSOA11CD) %>% summarise(presenting_MD = mean(presenting_MD))
geo_dat@data <- merge(geo_dat@data, MD_res, by.x = "msoa01cd", by.y = "MSOA11CD", all.x = TRUE)

#Merge MSOAs of dataset into shapefile
#geo_dat@data <- geo_dat[geo_dat$msoa01cd %in% vf_dat3$MSOA11CD,]

#Transforming to leaflet coordinate reference system
geo_dat2 <- spTransform(geo_dat, CRSobj = CRS("+init=epsg:4326"))

IMD_pal <- colorNumeric(palette = "YlOrRd", domain = IMD_res$mean_IMD)
MD_pal <- colorNumeric(palette = "Blues", domain = MD_res$presenting_MD)

leaflet(data = geo_dat2) %>% 
  addTiles(group = "OSM (default)") %>% 
  setView(lng = -1.884209, lat = 53.73652, zoom = 10) %>% 
  addPolygons(stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0.1, color = ~IMD_pal(mean_IMD), group = "IMD") %>% 
  addLegend(pal = IMD_pal, values = ~mean_IMD, opacity = 1, title = "IMD", group = "IMD") %>%
  addPolygons(stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0.1, color = ~MD_pal(presenting_MD), group = "Mean Deviation") %>% 
  addLegend(pal = MD_pal, values = ~presenting_MD, opacity = 1, title = "Mean Deviation", group = "Mean Deviation") %>%
  addLayersControl(baseGroups = c("IMD", "Mean Deviation"), options = layersControlOptions(collapsed = FALSE))

