# Glaucoma SES 2020
# Packages
library(rgdal)
library(leaflet)
library(maps)
library(dplyr)
library(rmapshaper)
library(ggplot2)
library(tidyverse)
library(sp)
# Load in functions

source("./functions.R")


# Loading in kml MSOA file of England and Wales

raw_dat <- readOGR("./data/raw_shapefiles/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp", 
                   layer = "Middle_Layer_Super_Output_Areas__December_2011__Boundaries")
geo_dat_original <- ms_simplify(raw_dat, sys = TRUE, keep = 0.05) # How much to keep: default 5%.

# Starting Analysis

geo_dat <- geo_dat_original # Make copy to work on


# Subset MSOAs we want to plot # # # NO CALDERDALE IN POSTCODE FILE FIND OUT WHAT POSTCODE THEY ARE

huddersfield <- c("Kirklees", "Calderdale")
cheltenham <- c("Forest of Dean", "Gloucester", "Cheltenham", "Tewkesbury", "Stroud", "Cotswold", "Wychavon", "Malvern Hills")
portsmouth <- c("Portsmouth", "Gosport", "Fareham", "Havant", "Winchester", "Chichester", "East Hampshire")

geo_dat$la_name <- trimws(gsub("[0-9]+", "", geo_dat$msoa11nm), "r") # # # THIS IS NOT LASOA NAME

geo_dat <- geo_dat[geo_dat$la_name %in% c(huddersfield, portsmouth, cheltenham),] # # # SOMETHING GOING WRONG HERE

#test <- as_tibble(geo_dat@data)

# Load in patient data

vf_dat <- read_csv("./data/All_24_2s.csv")[,1:42]

vf_dat <- vf_dat %>% 
  group_by(patient_pseudoid, EyeTested) %>% 
  mutate(age = as.numeric(as.Date(DateOfTest) - as.Date(BirthDate))/365.25, 
         NumOfVfs = length(MeanDeviation))


# Load in postcode data

postcode1 <- read_csv("./data/postcode1.csv")
postcode2 <- read_csv("./data/postcode2.csv") # No Calderdale??
postcodes <- bind_rows(postcode1, postcode2)
rm(list = c("postcode1", "postcode2"))


# Removing space from postcode and merging with vf_dat.
# This gives each patient an MSOA code.
# If the MSOA doesn't appear in the postcodes file then it won't be plotted.

vf_dat$Postcode <- gsub(" ", "", vf_dat$Postcode)
postcodes$PCD8 <- gsub(" ", "", postcodes$PCD8)
vf_dat2 <- merge(vf_dat, postcodes, by.x = "Postcode", by.y = "PCD8", all.x = TRUE) #Merge error? 


# Loading in IMD data and merging

IMD_dat <- read_csv("./data/IMD.csv")
IMD_dat <- IMD_dat[,c("LSOA code (2011)", "Index of Multiple Deprivation (IMD) Score")]

vf_dat3 <- merge(vf_dat2, IMD_dat, by.x = "LSOA11CD", by.y = "LSOA code (2011)", all.x = TRUE)


# Calculate statistics and merge by MSOA
# Starting with IMD for each MSOA

IMD_res <- vf_dat3 %>% 
  group_by(MSOA11CD) %>% 
  summarise(mean_IMD = as.numeric(mean(`Index of Multiple Deprivation (IMD) Score`, na.rm = TRUE)))

geo_dat <- merge(geo_dat, IMD_res, by.x = "msoa11cd", by.y = "MSOA11CD", all.x = TRUE)


# Load in demographics and merging

raceinfo <- read_csv("./data/raceinfo.CSV")
propblack <- raceinfo %>% 
  group_by(MSOA_CODE) %>% 
  summarise(proportion_black = as.numeric(`Black or Black British`)/as.numeric(`All People`))

geo_dat <- merge(geo_dat, propblack, by.x = "msoa11cd", by.y = "MSOA_CODE")

# Calculate mean deviation and merge

MD_res <- vf_dat3 %>% 
  arrange(patient_pseudoid, DateOfTest) %>% 
  group_by(MSOA11CD, patient_pseudoid, MSOA11NM) %>% 
  summarise(presenting_MD = MeanDeviation[1], 
            second_MD = MeanDeviation[2])

MD_res <- MD_res %>%
  group_by(MSOA11CD) %>% 
  summarise(presenting_MD = mean(second_MD, na.rm = TRUE),
            proportion = length(second_MD[second_MD < -12])/length(second_MD), 
            num_vfs = length(second_MD))

geo_dat <- merge(geo_dat, MD_res, by.x = "msoa11cd", by.y = "MSOA11CD", all.x = TRUE)

# Calculate age data and merge

age_res <- vf_dat3 %>% 
  group_by(MSOA11CD) %>% 
  summarise(age_prop = length(age[age > 60])/length(age))

geo_dat <- merge(geo_dat, age_res, by.x = "msoa11cd", by.y = "MSOA11CD", all.x = TRUE)


# Adding colour palettes

IMD_pal <- colorNumeric(palette = "YlOrRd", domain = IMD_res$mean_IMD)
MD_pal <- colorNumeric(palette = "Blues", domain = MD_res2$proportion)
Black_pal <- colorNumeric(palette = "Reds", domain = propblack$proportion_black)
age_pal <- colorNumeric(palette = "BuGn", domain = age_res$age_prop)

# Set NA if less than 10 VFs

geo_dat$proportion[geo_dat$num_vfs <= 10] <- NA
geo_dat$age_prop[geo_dat$num_vfs <= 10] <- NA
geo_dat$mean_IMD[geo_dat$num_vfs <= 10] <- NA
geo_dat$proportion_black[geo_dat$num_vfs <= 10] <- NA

# Need to remove the grey tiles on the edge

msoa_to_delete <- c("Chichester 014", "Chichester 012", "Chichester 010", "Chichester 006", "Chichester 008", "Chichester 002"
                    , "Chichester 004", "Chichester 005", "Chichester 001", "East Hampshire 009",
                    "East Hampshire 017", "East Hampshire 004", "East Hampshire 006", "East Hampshire 008", "East Hampshire 001",
                    "East Hampshire 002", "East Hampshire 003", "East Hampshire 007", "Winchester 004", "Winchester 002", "Winchester 001",
                    "Winchester 010", "Winchester 003", "Winchester 009", "Winchester 007", "Winchester 004", "Winchester 008",
                    "Winchester 006", "Winchester 005", "Malvern Hills 005", "Malvern Hills 004", "Malvern Hills 009", "Malvern Hills 010",
                    "Malvern Hills 006", "Malvern Hills 003", "Malvern Hills 013", "Malvern Hills 001", "Malvern Hills 002", "Malvern Hills 007",
                    "Wychavon 001", "Wychavon 006", "Wychavon 008", "Wychavon 009", "Wychavon 002", "Wychavon 003", "Wychavon 007",
                    "Wychavon 005", "Wychavon 004", "Kirklees 028", "Kirklees 023", "Kirklees 018", "Kirklees 019", "Kirklees 024", 
                    "Kirklees 016", "Kirklees 014", "Kirklees 007", "Kirklees 012", "Kirklees 013", "Kirklees 008", "Kirklees 002", 
                    "Kirklees 001", "Kirklees 017", "Kirklees 010", "Kirklees 006", "Kirklees 004", "Kirklees 011", "Kirklees 009")

geo_dat <- geo_dat[!(geo_dat$msoa11nm %in% msoa_to_delete),]

# Transforming to leaflet coordinate reference system

geo_dat <- spTransform(geo_dat, CRSobj = CRS("+init=epsg:4326"))

writeOGR(obj = geo_dat, dsn = "./shapefiles/data.shp", 
         layer = "data", 
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Testing comparison plots
# Idea: a little plot in the bottom right that compares the selected tile to the average
# Problem: How to display 4 different datapoints with different metrics on the same graph.

ref_df <- geo_dat@data %>% 
  summarise(
    men_IMD = mean(men_IMD), 
    prprtn_ = mean(prprtn_),
    prsn_MD = mean(prsn_MD),
    proprtn = mean(proprtn)
    )
