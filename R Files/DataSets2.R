# rm(list = ls())
setwd("/Volumes/Macintosh HD - Data/SCHOOL/WGU/CapstoneProject")  ##set your path
library(tidycensus)
library(dplyr)
library(tidyverse)
library(tigris)
library(leaflet)
library(stringr)
library(sf)
library(purrr)
library(zipcode)
library(stringi)
library(ggplot2)
library(devtools)
library(tmap)         
library(tmaptools)  
library(FactoMineR)
library(tm)
library(maps)
library(stats)
library(openintro)
library(missMDA)
library(pscl)
library(factoextra)
library(openintro)
library(missMDA)
library(devtools)
library(PerformanceAnalytics)
library(ggrepel)
library(scales)
library(conflicted)
library(ResourceSelection)
library(caret)
library(MASS)
library(mpath)
library(DataExplorer)
library(corrplot)
library(knitr)
library(rgdal)

## set conflicts
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

## custom functions
medianWithoutNA = function(x) {
  median(x[which(!is.na(x))])
}
add_cols <- function(.data, ..., .f = sum){
  tmp <- dplyr::select_at(.data, dplyr::vars(...))
  purrr::pmap_dbl(tmp, .f = .f)
} ## great function to sum up multiple columns from https://github.com/tidyverse/dplyr/issues/4544

######## RAW DATA ###############
farmers_markets = read.csv("Raw Data/farmers_markets_from_usda.csv") %>% mutate_if(is.factor, as.character) 

Election2016 = read.csv("Raw Data/Election2016byCounty.csv", 
                        colClasses = c(Fips = "character")) %>% mutate_if(is.factor, as.character) 

zips_in_county_subdiv = read.table("Raw Data/Zip Code Referential Table.txt", header = TRUE, sep = ",",
                                   colClasses = c(ZCTA5 = "character", 
                                                  STATE =    "character",  COUNTY = "character", COUSUB = "character",  
                                                  GEOID  = "character", CLASSFP = "character"))
animal_total_sales = read.csv("Raw Data/USDA DATA/AnimalTotalByCounty.csv",
                              colClasses = c(State.ANSI = "character", County.ANSI = "character" )) %>%
  mutate(State.ANSI = ifelse(nchar(State.ANSI) == 1, paste0("0", State.ANSI), State.ANSI),
         County.ANSI = ifelse(nchar(County.ANSI) == 1, paste0("0", County.ANSI), County.ANSI),
         County.ANSI = ifelse(nchar(County.ANSI) == 2, paste0("0", County.ANSI), County.ANSI),
         Fips = paste0(State.ANSI,County.ANSI))

crop_total_sales = read.csv("Raw Data/USDA DATA/CropTotalsSalesDollars.csv",
                            colClasses = c(State.ANSI = "character", County.ANSI = "character" )) %>%
  mutate(State.ANSI = ifelse(nchar(State.ANSI) == 1, paste0("0", State.ANSI), State.ANSI),
         County.ANSI = ifelse(nchar(County.ANSI) == 1, paste0("0", County.ANSI), County.ANSI),
         County.ANSI = ifelse(nchar(County.ANSI) == 2, paste0("0", County.ANSI), County.ANSI),
         Fips = paste0(State.ANSI,County.ANSI))

fruit_and_nuts_total_sales = read.csv("Raw Data/USDA DATA/FruitNutsSAlesDollars.csv",
                                      colClasses = c(State.ANSI = "character", County.ANSI = "character" )) %>%
  mutate(State.ANSI = ifelse(nchar(State.ANSI) == 1, paste0("0", State.ANSI), State.ANSI),
         County.ANSI = ifelse(nchar(County.ANSI) == 1, paste0("0", County.ANSI), County.ANSI),
         County.ANSI = ifelse(nchar(County.ANSI) == 2, paste0("0", County.ANSI), County.ANSI),
         Fips = paste0(State.ANSI,County.ANSI))

veggie_total_sales = read.csv("Raw Data/USDA DATA/VeggieTotalSalesDollars.csv",
                              colClasses = c(State.ANSI = "character", County.ANSI = "character" )) %>%
  mutate(State.ANSI = ifelse(nchar(State.ANSI) == 1, paste0("0", State.ANSI), State.ANSI),
         County.ANSI = ifelse(nchar(County.ANSI) == 1, paste0("0", County.ANSI), County.ANSI),
         County.ANSI = ifelse(nchar(County.ANSI) == 2, paste0("0", County.ANSI), County.ANSI),
         Fips = paste0(State.ANSI,County.ANSI))

AG_land = read.csv("Raw Data/USDA DATA/AGLand.csv",
                   colClasses = c(State.ANSI = "character", County.ANSI = "character" )) %>%
  mutate(State.ANSI = ifelse(nchar(State.ANSI) == 1, paste0("0", State.ANSI), State.ANSI),
         County.ANSI = ifelse(nchar(County.ANSI) == 1, paste0("0", County.ANSI), County.ANSI),
         County.ANSI = ifelse(nchar(County.ANSI) == 2, paste0("0", County.ANSI), County.ANSI),
         Fips = paste0(State.ANSI,County.ANSI))



########## Cleaned Data ##############

All_Final_Data_Cleaned = read.csv("Cleaned Data/All_Final_Data_Cleaned.csv", 
                                  colClasses = c(Fips = "character"), stringsAsFactors = FALSE)  %>% dplyr::select(-X) %>% 
   mutate( Fips = ifelse(nchar(Fips) == 4, paste0("0", Fips), Fips) ) %>% 
   mutate_if(is.factor, as.character) 
rownames(All_Final_Data_Cleaned) = All_Final_Data_Cleaned$RowName


All_Final_Data_Cleaned2 = read.csv("Cleaned Data/All_Final_Data_Cleaned2.csv", 
                                  colClasses = c(Fips = "character"), stringsAsFactors = FALSE)  %>% dplyr::select(-X) %>% 
  mutate( Fips = ifelse(nchar(Fips) == 4, paste0("0", Fips), Fips) ) %>% 
  mutate_if(is.factor, as.character) 
rownames(All_Final_Data_Cleaned2) = All_Final_Data_Cleaned2$RowName


All_Data_PCA = read.csv("Cleaned Data/All_Data_PCA.csv")
rownames(All_Data_PCA) = All_Data_PCA$X
All_Data_PCA = select(All_Data_PCA, -X)

Election2016Cleaned = read.csv("Cleaned Data/Election2016Cleaned.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-X)  %>% 
  mutate(Fips = ifelse(nchar(Fips) == 4, paste0("0", Fips), Fips))

USDA_data_cleaned = read.csv("Cleaned Data/USDA_data.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-X)  %>% 
  mutate(Fips = ifelse(nchar(Fips) == 4, paste0("0", Fips), Fips))

Census_data_cleaned = read.csv("Cleaned Data/Census_data_cleaned.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-X) %>% 
  mutate(Fips = ifelse(nchar(Fips) == 4, paste0("0", Fips), Fips))

FarmersMarketEach = read.csv("Cleaned Data/FarmersMarketEach.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-X)  %>% 
  mutate(Fips = ifelse(nchar(Fips) == 4, paste0("0", Fips), Fips))

AllCountyFMfreq = read.csv("Cleaned Data/AllCountyFarmersMarketFreq.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-X)  %>% 
  mutate(Fips = ifelse(nchar(Fips) == 4, paste0("0", Fips), Fips))

zip_unique = read.csv("Cleaned Data/Zip Unique.csv", stringsAsFactors = FALSE) %>% dplyr::select(-X) %>% 
  mutate(Zip = ifelse(nchar(Zip) == 3, paste0("00)", Zip), Zip),
         Zip = ifelse(nchar(Zip) == 4, paste0("0)", Zip), Zip),
         Fips = ifelse(nchar(Fips) == 4, paste0("0", Fips), Fips))
