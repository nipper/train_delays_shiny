library(feather)
library(lubridate)
library(dplyr)
library(tools)

full_trains <- read_feather("data/full_trains.feather") %>% 
  mutate(observation_date = mdy(paste(month,1,year,sep = "/")))


departure_stations <- set_names(sort(unique(full_trains$departure_station)),
                                toTitleCase(tolower(sort(unique(full_trains$departure_station)))))


