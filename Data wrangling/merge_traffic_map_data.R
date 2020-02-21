rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(tidyverse)

load("../Data/Traffic data_2.RData")

traffic_segment_geom <- traffic_segment_geom %>%
  group_by(id, part) %>%
  slice(1)

ids <- unique(traffic_segment_geom$id)

zoom <- 16

traffic_map_data <- array(NA, dim = c(length(ids), 7, 256, 256))
traffic_map_id <- rep(NA, length(ids))

for(i in 1:length(ids)) {
  cat(i, '/', length(ids), '\n')

  file <- paste0('../Data/traffic map/', ids[i], '_', zoom, '.RData')
  if(file.exists(file)) {
    load(file)
    traffic_map_id[i] <- ids[i]
    require(abind)
    require(keras)
    traffic_map_data[i,,,] <- image_data
  } 
  
}

index <- which(!is.na(traffic_map_id))
traffic_map_data <- traffic_map_data[index,,,]
traffic_map_id <- traffic_map_id[index]

save(traffic_map_data, traffic_map_id, file = paste0('../Data/traffic map data', '_', zoom, '.RData'))
