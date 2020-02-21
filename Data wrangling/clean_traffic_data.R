rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

version <- 2

require(tidyverse)
require(lubridate)

load(file = '../Data/Traffic data_1.RData')

# only use segments with observations on atleast 20 distinct dates
sel_id <- traffic_data %>%
  group_by(id) %>%
  summarize(dates = length(unique(date))) %>%
  filter(dates >= 20) %>% 
  pull(id)

traffic_data <- traffic_data %>%
  filter(id %in% sel_id)

# the variables oneway, road_type, road_speed and speed have near zero variance
traffic_segment <- traffic_segment %>%
  filter(id %in% sel_id) %>%
  select(-c(oneway, road_type, road_speed, speed))

traffic_segment_geom <- traffic_segment_geom %>%
  filter(id %in% sel_id)

traffic_typical <- traffic_typical %>%
  filter(id %in% sel_id)

save(traffic_data, 
     traffic_segment, 
     traffic_segment_geom, 
     traffic_typical, file = paste("../Data/Traffic data_", version, ".RData", sep = ""))
