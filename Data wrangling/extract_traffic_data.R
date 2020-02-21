rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(lubridate)

require(RCurl)
require(tidyverse)
require(rjson)
require(httr)

r <- GET("https://telraam-api.net/v0/segments/active")
result <- content(r,as="parsed") 

traffic_segment_geom <- data.frame();
traffic_segment <- data.frame();
traffic_typical <- data.frame();

null_to_na <- function(x){
  if(is.null(x)) return(NA)
  return(x)
}

for(i in 1:length(result$features)) { # per segment
  id <- result$features[[i]]$properties$id
  
  if(length(result$features[[i]]$geometry$coordinates) != 1) {
    cat(i, 'length(result$features[[1]]$geometry$coordinates) != 1', '\n')
  }
  coord <- unlist(result$features[[i]]$geometry$coordinates[[1]])
  lat <- coord[2*1:(length(coord)/2)]
  lon <- coord[2*1:(length(coord)/2)-1]
  
  traffic_segment_geom <- rbind(traffic_segment_geom, data.frame(id = id, part = 1:(length(coord)/2), lat = lat, lon = lon))
  
  property <- result$features[[i]]$properties
  
  traffic_segment <- rbind(traffic_segment,
                           data.frame(id = id, 
                                      speed = null_to_na(property$speed),
                                      oneway = null_to_na(property$oneway),
                                      road_type = null_to_na(property$road_type),
                                      road_speed = null_to_na(property$road_speed),
                                      pedestrian_avg = null_to_na(property$pedestrian_avg),
                                      bike_avg = null_to_na(property$bike_avg),
                                      car_avg = null_to_na(property$car_avg),
                                      lorry_avg = null_to_na(property$lorry_avg),
                                      last = null_to_na(property$last_data_package),
                                      lat = mean(lat),
                                      lon = mean(lon)))
  
  if(!is.null(property$typical_data)) {
    typical <- data.frame(id = id,
                          hour = as.numeric(substr(unlist(map(property$typical_data, 'hour')), 1, 2)),
                          pedestrian =  unlist(map(property$typical_data, function(x) {null_to_na(x$pedestrian)})),
                          bike =  unlist(map(property$typical_data, function(x) {null_to_na(x$bike)})),
                          car =  unlist(map(property$typical_data, function(x) {null_to_na(x$car)})),
                          lorry =  unlist(map(property$typical_data, function(x) {null_to_na(x$lorry)})))
    
    traffic_typical <- rbind(traffic_typical,
                             typical)
  }
  
 
}

# telraam is gestart in september 2018
start <- '2017-01-31 00:00'
end <- '2020-01-31 23:59'
format <- 'per-hour'

traffic_data <- data.frame()

for(i in 1:length(result$features)) {
  id <- result$features[[i]]$properties$id
  
  detail <- POST(paste("https://telraam-api.net/v0/reports/", id, sep = ''), 
                 body = paste('{\n    "time_start": "', start, '",\n    "time_end": "', end, '",\n    "level": "segments",\n    "format": "',format,'"\n}',
                              sep = ''), 
                 encode = "json",
                 add_headers(`Content-Type` = "application/json"))
  
  if(detail$status_code != 200) {
    cat(id, ' status != 200', '\n')
  }
  
  detail <- content(detail, as="parsed") 
  
  cat(i, ' - ', id, ' - obs: ', length(detail[[1]]), '\n')
  
  save(detail, file = paste('api response/segment', id, '.RData', sep = ''))
  
  if(length(detail[[1]]) == 0) next;
  
  traffic_data <- rbind(traffic_data,
                        data.frame(id = id,
                                   date_time = unlist(map(detail[[1]], 'date')),
                                   pct_up = unlist(map(detail[[1]], 'pct_up')),
                                   pedestrian = unlist(map(detail[[1]], function(x) {null_to_na(x$pedestrian)})),
                                   bike = unlist(map(detail[[1]], function(x) {null_to_na(x$bike)})),
                                   car = unlist(map(detail[[1]], function(x) {null_to_na(x$car)})),
                                   lorry = unlist(map(detail[[1]], function(x) {null_to_na(x$lorry)})),
                                   pedestrian_left = unlist(map(detail[[1]], function(x) {null_to_na(x$pedestrian_lft)})),
                                   bike_left = unlist(map(detail[[1]], function(x) {null_to_na(x$bike_lft)})),
                                   car_left = unlist(map(detail[[1]], function(x) {null_to_na(x$car_lft)})),
                                   lorry_left = unlist(map(detail[[1]], function(x) {null_to_na(x$lorry_lft)})),
                                   pedestrian_right = unlist(map(detail[[1]], function(x) {null_to_na(x$pedestrian_rgt)})),
                                   bike_right = unlist(map(detail[[1]], function(x) {null_to_na(x$bike_rgt)})),
                                   car_right = unlist(map(detail[[1]], function(x) {null_to_na(x$car_rgt)})),
                                   lorry_right = unlist(map(detail[[1]], function(x) {null_to_na(x$lorry_rgt)}))))
}

traffic_data$date_time <- as.POSIXct(traffic_data$date_time, format = '%Y-%m-%d %H:%M', tz = 'UTC')

traffic_data <- traffic_data %>%
  mutate(time_c = hour(date_time),
         date = as.Date(date_time))

save(traffic_data, traffic_typical, traffic_segment, traffic_segment_geom, file = '../Data/Traffic data_1.RData')
