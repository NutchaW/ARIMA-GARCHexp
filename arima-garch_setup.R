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
# use ili- and confirm cases to augment
# get data
ili_data <- download_and_preprocess_flu_data()
locations <- unique(ili_data$region)
seasons <- unique(ili_data$season)

# impute 
imputed_ili <-data.frame()
for (j in 1:11){
data_info <- ili_data %>% 
  dplyr::filter(region==locations[j]) 
imputed <- data_info %>%
      dplyr::mutate(imputed_w = 
                      imputeTS::na_interpolation(weighted_ili,option="linear")) 
imputed_ili <-rbind(imputed_ili,imputed)
}
imputed_ili$imputed_w[imputed_ili$imputed_w==0] <- 0.0000000001

# transform
# Transform data
# Bounds
a <- 0
b <- 100
imputed_ili$log_ili <- log((imputed_ili$imputed_w-a)/(b-imputed_ili$imputed_w))
## Model specification (for simulation)
# use exponential and with no box-cox
for (j in 1:11){
  region_ili <- imputed_ili %>% dplyr::filter(region==locations[j]) 
  assign(paste0("ets_reg",j),
         ets(region_ili$log_ili))
}
# function to back transformation
backT<-function(sim){
  (b-a)*exp(sim)/(1+exp(sim)) + a
}

# simulate
for (j in 1:11){
  sim<-matrix(NA,ncol=27,nrow=100000)
for (i in 1:100000){
  sim[i,]<-simulate(get(paste0("ets_reg",j)), 27)
}
  assign(paste0("sim_region",j), apply(sim,1,backT))
}

# make bins
flu_data <- download_and_preprocess_flu_data() %>%
  mutate(
    region = ifelse(
      region == "National",
      "US National",
      paste0("HHS ", region)
    )
  )

season_start_ew <- "2020-ew10"
season_end_ew <- "2020-ew35"
cdc_report_ew <- get_current_date_from_flu_data(flu_data)
targets <-  c("wk ahead", "Peak height", "Peak week",
              "First week below baseline", "Below baseline for 3 weeks")

trajectories_by_location <- tibble(
  location = c("HHS Region 1", "US National")
) %>%
  mutate(
    trajectories = purrr::map(
      location,
      get_trajectories_one_location,
      nsim = 1000,
      flu_data = flu_data,
      target_variable = "weighted_ili",
      season_start_ew = season_start_ew,
      season_end_ew = season_end_ew,
      cdc_report_ew = cdc_report_ew,
      targets = targets)
  )

for (j in 1:11){
  assign(paste0("binDis",j),multi_trajectories_to_binned_distributions(
    multi_trajectories,
    targets,
    h_max,
    bins,
    season_start_ew,
    season_end_ew,
    cdc_report_ew
  )
)
}
