rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

version <- 4

require(tidyverse)
require(chron)
require(lubridate)

load(paste('../Data/Air data_', version, '.RData', sep = ''))

spatial_distance <- function(lat1, lon1, lat2, lon2) {
  deg2rad <- function(deg){
    return (deg * (pi/180))
  }
  
  radius = 6371;
  dLat = deg2rad(lat2-lat1);  
  dLon = deg2rad(lon2-lon1); 
  a = sin(dLat/2)^2 + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLon/2) * sin(dLon/2)
  
  c = 2 * atan2(sqrt(a), sqrt(1-a)); 
  d = radius * c; 
  return(d * 1000);
}


location <- air_data %>% 
  group_by(id, lat, lon) %>%
  slice(1) %>%
  ungroup() %>% 
  select(id, lat, lon)

require(leaflet)
leaflet(data = location) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(id), label = ~as.character(id))


start <- as.Date("2019-01-01")
end <- as.Date("2019-07-01")

visualize_air_station(c(8765, 13526, 13528), start, end, c('pm25'))

visualize_air_station <- function(station, start, end, variables) {
  
  for(variable in variables) {
    df <- air_data %>%
      filter(id %in% station,
             date_time >= start,
             date_time < end) %>%
      select(!! sym(variable), id, date_time)
    
    print(ggplot(df) +
            theme_bw() +
            geom_point(aes(date_time, !!sym(variable), color = as.factor(id))) +
            xlab('time') +
            ggtitle(variable))
  }
  
}

# all stations have many measurements
result <- air_data %>% 
  group_by(id) %>%
  summarise(count = n()) %>%
  arrange(count)

df <- air_data %>%
  filter(id == 6561)

ggplot(df) +
  geom_point(aes(date_time, pm25))

df <- df %>%
  arrange(date_time) %>%
  mutate(dtime = c(0, diff(date_time)),
         dpm25 = c(0, diff(pm25)))


# The colder it is, the higher the concentraction pm25
# logical if burning wood is the main cause
pm_by_temp <- air_data %>% 
  filter(temperature < 50) %>%
  filter(!is.na(temperature)) %>%
  mutate(temp = round(temperature)) %>%
  group_by(temp) %>%
  summarize(avg = mean(pm25, na.rm = TRUE),
            count = n()) 

ggplot(pm_by_temp) +
  theme_bw() +
  geom_point(aes(temp, avg))

pm_by_humidity <- air_data %>% 
  filter(humidity <= 100) %>%
  filter(!is.na(humidity)) %>%
  mutate(humidity = round(humidity)) %>%
  group_by(humidity) %>%
  summarize(avg = mean(pm25, na.rm = TRUE),
            count = n()) 

ggplot(pm_by_humidity) +
  theme_bw() +
  geom_point(aes(humidity, avg))

pm_by_month <- air_data %>% 
  mutate(month = month(date_time)) %>%
  group_by(time_c, month) %>%
  summarize(avg = mean(pm25, na.rm = TRUE),
            count = n()) 

ggplot(pm_by_month) +
  theme_bw() +
  geom_point(aes(time_c, avg, color = factor(month)))

pm_by_temp_time <- air_data %>% 
  filter(temperature < 50) %>%
  filter(!is.na(temperature)) %>%
  mutate(temp = round(temperature/5)*5) %>%
  group_by(time_c, temp) %>%
  summarize(avg = mean(pm25, na.rm = TRUE),
            count = n()) 

ggplot(pm_by_temp_time) +
  theme_bw() +
  geom_point(aes(time_c, avg, color = factor(temp)))

res <- air_data %>%
  arrange(id, date_time) %>%
  group_by(id) %>%
  mutate(dtime = c(0, diff(date_time)))

meta <- read.csv('../Data/Air sensor meta.csv')


