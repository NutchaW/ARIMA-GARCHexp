library(rugarch)
library(cdcForecastUtils)
library(dplyr)
library(forecast)
library(imputeTS)
library(tidyverse)
library(tsibble)
library(lubridate)
library(feasts)
library(fable)

# get data
ili_data <- download_and_preprocess_flu_data()
locations <- unique(ili_data$region)
seasons <- unique(ili_data$season)

# plot NAs
for(i in 1:length(seasons)){
  for(j in 1:length(locations)){
  ili_dat_s<- ili_data %>%
    dplyr::filter(season==seasons[i],region==locations[j])
  pdf(paste0("figures/NAdis_region",j,"_location",i,".pdf"))
  plotNA.distribution(ili_dat_s$weighted_ili)
  dev.off()
  }
}


df <- ili_data %>%
  dplyr::filter(region==locations[1]) 
df<- tslm(df$weighted_ili  ~ trend + fourier(df$weighted_ili , 2))
