rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

version <- 5;

require(tidyverse)
require(chron)
require(lubridate)
minutes = lubridate::minutes
hours = lubridate::hours
seconds = lubridate::seconds

data_median <- read.csv('../Data/Air data median.csv')
data2018 <- read.csv('../Data/Air data 2018.csv')
data2019 <- read.csv('../Data/Air data 2019.csv')
data2020 <- read.csv('../Data/Air data 2020.csv')

data <- rbind(data2018, data2019, data2020)

data$DATEUTC <- as.POSIXct(as.character(data$DATEUTC), "%Y-%m-%d %H:%M:%S", tz = "UTC")

data <- data %>% 
  mutate(date = as.Date(DATEUTC, tz = "UTC"),
         time_d = hour(DATEUTC) * 60 + minute(DATEUTC),
         time_r = floor(time_d/5))

air_data <- data %>%
  group_by(SDS011ID, date, time_r) %>%
  summarize(num_record = sum(!is.na(PM2.5)),
            LAT = LAT[1],
            LON = LON[1],
            PM2.5 = mean(PM2.5, na.rm = TRUE),
            PM10 = mean(PM10, na.rm = TRUE),
            DHTID = DHTID[1],
            TEMPERATURE = mean(TEMPERATURE, na.rm = TRUE),
            HUMIDITY = mean(HUMIDITY, na.rm = TRUE))

air_data$date_time = as.POSIXct(air_data$date + minutes(air_data$time_r*5))

air_data <- air_data %>%
  ungroup() %>%
  mutate(time_c = (time_r*5)/60,
         time = times(strftime(date_time, format="%T", tz = 'UTC'))) %>%
  select(-time_r)
         
air_data <- air_data %>%
  rename(id = "SDS011ID",
         lat = "LAT",
         lon = "LON",
         pm25 = "PM2.5",
         pm100 = "PM10",
         temperature = "TEMPERATURE",
         humidity = "HUMIDITY")


save(air_data, file = paste("../Data/Air data_", version, ".RData", sep = ""))
