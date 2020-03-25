library(cdcForecastUtils)
library(dplyr)
library(rugarch)
library(forecast)
library(imputeTS)

state_data <- download_and_preprocess_state_flu_data()
states <- unique(state_data$region)[-c(10,55)]
seasons_state <- unique(state_data$season)

# state
imputed_state_ili <-data.frame()
for (j in 1:length(states)){
  imputed <- state_data%>%
    dplyr::filter(region==states[j]) %>%
    dplyr::select(unweighted_ili) %>%
    imputeTS::na_interpolation(option="linear") 
  imputed_state_ili <-rbind(imputed_state_ili,imputed)
}


