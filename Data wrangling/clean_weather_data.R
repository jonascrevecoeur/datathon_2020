rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

version <- 2;

require(tidyverse)
require(lubridate)
require(chron)

data <- read.csv('../Data/Weather data.csv')

data$DATEUTC <- as.POSIXct(as.character(data$DATEUTC), "%Y-%m-%d %H:%M:%S", tz = "UTC")
data$ID <- as.numeric(substr(as.character(data$ID), 7, 10))

data <- data %>%
  mutate(date = as.Date(DATEUTC, tz = "UTC"),
         time = times(strftime(DATEUTC, format="%T", tz = 'UTC')),
         time_c = hour(DATEUTC) + minute(DATEUTC) / 60 + second(DATEUTC) / 3600,
         WINDCHILLF = (WINDCHILLF-32) / 1.8) 

data <- data %>%
  rename(date_time = "DATEUTC",
         id = "ID",
         lat = "LAT",
         lon = "LON",
         altitude = "ALT",
         temperature = "TEMPC",
         humidity = "HUMIDITY",
         dew_point = "DEWPTC",
         wind_chill = "WINDCHILLF",
         wind_direction = "WINDDIR",
         wind_speed = "WINDSPEEDKMH",
         wind_gust = "WINDGUSTKMH",
         rain = "RAININ",
         rain_day = "DAILYRAININ",
         rain_week = "WEEKLYRAININ", 
         rain_month = "MONTHLYRAININ",
         rain_year = "YEARLYRAININ",
         solar_radiation = "SOLARRADIATION",
         uv = "UV",
         num_record = "NRECORDS")

# remove observations that are not a multiple of 5 minutes
data <- data %>% 
  filter(second(date_time) == 0,
         minute(date_time) %% 5 == 0)

## NA values for 2019-06-22

## Weather stations with strange observations
# 63, 93: few observations
# 51: temperature does not decrease during the night, not all rain events observed
# 38: large and prolonged rain events. Probably located under a gutter or something similar?
# 39: the site mentions that the data for 38 and 39 is wrong
data <- data %>%
  filter(!(id %in% c(63, 93, 38, 39)))


# Observations are often wrong before or after a station has lost its signal.
# Remove 12 hours of data before and after signal loss:
# (warning: this is computationally intensive)
data <- data %>% 
  arrange(id, date_time) %>%
  group_by(id) %>%
  mutate(dtime = c(0, diff(date_time)))

ids <- unique(data$id)

boundaries <- list();

for(station in ids) {
  jump <- data %>% 
    filter(dtime >= 60,
           id == station) %>% 
    pull(date_time)
  
  if(length(jump) == 0) {
    boundary <- c(0, 0)
  } else {
    boundary <- c(jump[1] - 3600*12, jump[1] + 3600*12)
  }
  
  
  if(length(jump) > 1) {
    for(i in 2:length(jump)) {
      if(jump[i] - 3600*12 <= boundary[length(boundary)]) {
        boundary[length(boundary)] <- jump[i] + 3600*12
      } else {
        boundary <- c(boundary, jump[i] - 3600*12, jump[i] + 3600*12)
      }
    }
  }
  
  boundaries[[station]] <- boundary
}

data <- data %>%
  mutate(above_boundary = map2_dbl(date_time, id, function(x, y){sum(x > boundaries[[y]])}),
         below_boundary = map2_dbl(date_time, id, function(x, y){sum(x < boundaries[[y]])}))

data <- data %>%
  mutate(remove = (above_boundary %% 2 == 1) | (below_boundary %% 2 == 1))

data %>%
  group_by(id) %>%
  summarize(count = n(),
            count_remove = sum(remove),
            remove_pct = count_remove / count) %>%
  data.frame

data <- data %>%
  filter(!remove) %>%
  select(-c(remove, above_boundary, below_boundary, dtime))

weather_data <- data

save(weather_data, file = paste("../Data/Weather data_", version, ".RData", sep = ""))
