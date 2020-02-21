rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(tidyverse)
require(lubridate)

load(file = '../Data/Traffic data_2.RData')

location <- traffic_segment %>% 
  group_by(id, lat, lon) %>%
  slice(1) %>%
  ungroup() %>% 
  select(id, lat, lon)

require(leaflet)
leaflet(data = location) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(id), label = ~as.character(id))
