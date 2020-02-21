rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

version <- 4

require(tidyverse)
require(chron)
require(lubridate)

load(paste('../Data/Air data_', version, '.RData', sep = ''))
load('../Data/Weather data_2.RData')

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

location_air <- air_data %>% 
  group_by(id, lat, lon) %>%
  slice(1) %>%
  ungroup() %>% 
  select(id, lat, lon)

require(leaflet)
leaflet() %>%
  addTiles(urlTemplate = "https://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png") %>%
  addMarkers(lng = location_air$lon, lat = location_air$lat)

location_weather <- weather_data %>% 
  group_by(id, lat, lon) %>%
  slice(1) %>%
  ungroup() %>% 
  select(id, lat, lon) %>%
  rename(weather_id = id,
         weather_lat = lat,
         weather_lon = lon)

intersect <- location_air %>%
  merge(location_weather, all = TRUE) %>%
  mutate(distance = spatial_distance(weather_lat, weather_lon, lat, lon)) %>%
  filter(distance > 0, 
         distance < 5000) 

combo <- intersect %>% 
  group_by(id) %>%
  arrange(distance) %>%
  slice(1)


air_weather <- air_data %>%
  mutate(weather_id = combo$weather_id[match(id, combo$id)])

air_weather <- inner_join(
             air_weather %>% 
               select(date_time, id, weather_id, pm25), 
             weather_data %>%
               select(date_time, id, temperature, wind_speed, wind_direction) %>%
               rename(weather_id = id), 
             by = c('date_time', 'weather_id') ) 


res <- air_weather %>%
  mutate(wd = floor(wind_direction / 10) * 10,
         ws = floor(wind_speed/5)*5) %>%
  group_by(wd, ws) %>%
  summarize(pm_avg = mean(pm25, na.rm = TRUE),
            count = n()) %>%
  filter(ws > 0)


ggplot(res) +
  ggstyle + 
  geom_line(aes(wd, pm_avg, color = factor(ws))) 

require(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = 4, lat = 50)

for(index in 1:20) {

weather_id <- combo$weather_id[index]
air_id <- combo$id[index]

sub_air <- air_data %>%
  filter(id == air_id) %>%
  select(date_time, pm25)

sub_weather <- weather_data %>%
  filter(id == weather_id) %>%
  select(date_time, temperature, wind_direction, wind_speed)

air_weather <- inner_join(sub_air, sub_weather, by = 'date_time') %>% 
  filter(!is.na(wind_direction))

df <- air_weather %>%
  mutate(wd = floor(wind_direction / 10)*10,
         ws = floor(wind_speed / 2)*2) %>%
  group_by(wd, ws) %>%
  summarize(pm25 = mean(pm25))

plot(ggplot(df) +
  theme_bw() +
  geom_tile(aes(x = wd, y = ws, fill = pm25)) +
  scale_fill_gradient2('pm25', low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(0, 50)))

}


