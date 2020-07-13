#Glaucoma SES 2020
#Packages
library(rgdal)
library(leaflet)
library(maps)
library(dplyr)
library(rmapshaper)
#Setting working directory
setwd("C:\\Users\\Steph\\Documents\\PhD Related Files\\glaucoma_ses")

#Load in functions
source("R Code\\functions.R")

#Loading in kml MSOA file of England and Wales
geo_dat <- readOGR("C:\\Users\\Steph\\Documents\\PhD Related Files\\glaucoma_ses\\Data", layer = "Middle_Layer_Super_Output_Areas__December_2001__Boundaries")

#Subset MSOAs we want to plot
huddersfield <- c("Kirklees", "Calderdale")
cheltenham <- c("Forest of Dean", "Gloucester", "Cheltenham", "Tewkesbury", "Stroud", "Cotswold", "Wychavon", "Malvern Hills", "Worcester")
portsmouth <- c("Portsmouth", "Gosport", "Fareham", "Havant", "Winchester", "Chichester", "East Hampshire")
geo_dat$la_name <- trimws(gsub("[0-9]+", "", geo_dat$msoa01nm), "r")
geo_dat <- geo_dat[geo_dat$la_name %in% c(huddersfield, portsmouth, cheltenham),]

geo_dat <- ms_simplify(geo_dat, sys = TRUE)

#Load in patient data
vf_dat <- read.csv("C:\\Users\\Steph\\Documents\\PhD Related Files\\VFs\\Older Medisoft data\\All_24_2s.csv")[,1:42]

vf_dat <- vf_dat %>% group_by(patient_pseudoid, EyeTested) %>% mutate(age = as.numeric(as.Date(DateOfTest) - as.Date(BirthDate))/365.25, NumOfVfs = length(MeanDeviation))

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



#Calculate statistics by MSOA
IMD_res <- vf_dat3 %>% group_by(MSOA11CD) %>% summarise(mean_IMD = mean(Index.of.Multiple.Deprivation..IMD..Score))
geo_dat@data <- merge(geo_dat@data, IMD_res, by.x = "msoa01cd", by.y = "MSOA11CD", all.x = TRUE)

#Load in demographics and merging
raceinfo <- read.csv("Data\\raceinfo.CSV")
propblack <- raceinfo %>% group_by(MSOA_CODE) %>% summarise(proportion_black = as.numeric(Black.or.Black.British)/as.numeric(All.People))
geo_dat@data <- merge(geo_dat@data, propblack, by.x = "msoa01cd", by.y = "MSOA_CODE")

MD_res <- vf_dat3 %>% arrange(patient_pseudoid, DateOfTest) %>% group_by(MSOA11CD, patient_pseudoid) %>% summarise(presenting_MD = MeanDeviation[1], second_MD = MeanDeviation[2])
MD_res <- MD_res %>% group_by(MSOA11CD) %>% summarise(presenting_MD = mean(presenting_MD), proportion = length(second_MD[second_MD < -12])/length(second_MD))
geo_dat@data <- merge(geo_dat@data, MD_res, by.x = "msoa01cd", by.y = "MSOA11CD", all.x = TRUE)

age_res <- vf_dat3 %>% group_by(MSOA11CD) %>% summarise(age_prop = length(age[age > 60])/length(age))
geo_dat@data <- merge(geo_dat@data, age_res, by.x = "msoa01cd", by.y = "MSOA11CD", all.x = TRUE)

#Transforming to leaflet coordinate reference system
geo_dat2 <- spTransform(geo_dat, CRSobj = CRS("+init=epsg:4326"))

IMD_pal <- colorNumeric(palette = "YlOrRd", domain = IMD_res$mean_IMD)
MD_pal <- colorNumeric(palette = "Blues", domain = MD_res$proportion)
Black_pal <- colorNumeric(palette = "Reds", domain = propblack$proportion_black)
age_pal <- colorNumeric(palette = "BuGn", domain = age_res$age_prop)

geo_dat2 <- geo_dat2[complete.cases(geo_dat2$mean_IMD),] #Maybe set NAs to grey..

leaflet(data = geo_dat2) %>% 
  addTiles(group = "OSM (default)") %>% 
  setView(lng = -1.884209, lat = 53.73652, zoom = 10) %>% 
  addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Mean IMD :", round(mean_IMD)), smoothFactor = 0.1, color = ~IMD_pal(mean_IMD), group = "IMD") %>% 
  addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Proportion < -12 MD :", round(proportion, 2)), smoothFactor = 0.1, color = ~MD_pal(proportion), group = "Mean Deviation") %>% 
  addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Proportion of Afro-Caribbean :", round(proportion_black, 2)), smoothFactor = 0.1, color = ~Black_pal(proportion_black), group = "Proportion Afro-Caribbean") %>% 
  addPolygons(stroke = FALSE, fillOpacity = 0.9, label = ~paste0("Proportion over 60 :", round(age_prop, 2)), smoothFactor = 0.1, color = ~age_pal(age_prop), group = "Proportion over 60") %>% 
  addLegend(position = "bottomright", pal = IMD_pal, values = ~mean_IMD, opacity = 1, title = "IMD", group = "IMD") %>%
  addLegend(position = "bottomright", pal = MD_pal, values = ~proportion, opacity = 1, title = "Proportion of MD <-12", group = "Mean Deviation") %>%
  addLegend(position = "bottomright", pal = Black_pal, values = ~proportion_black, opacity = 1, title = "Proportion Afro-Caribbean", group = "Proportion Afro-Caribbean") %>%
  addLegend(position = "bottomright", pal = age_pal, values = ~age_prop, opacity = 1, title = "Proportion Over 60", group = "Proportion over 60") %>%
  addLayersControl(overlayGroups = c("IMD", "Mean Deviation", "Proportion Afro-Caribbean", "Proportion over 60"), options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Mean Deviation", "Proportion Afro-Caribbean", "Proportion over 60"))


