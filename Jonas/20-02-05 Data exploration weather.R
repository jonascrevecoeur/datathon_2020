rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(tidyverse)

load(file = "../Data/Weather data.RData")

data$date_time <- as.POSIXct(as.character(data$date_time), "%Y-%m-%d %H:%M:%S", tz = "UTC")

# data <- read.csv('../Data/Weather data.csv')
# 
# data$DATEUTC <- as.POSIXct(as.character(data$DATEUTC), "%Y-%m-%d %H:%M:%S", tz = "UTC")
# data$ID <- as.numeric(substr(as.character(data$ID), 7, 10))
# 
# save(data, file = "../Data/Weather data.RData")

# There are more unique ID than locations -- strange
length(unique(data$ID))
length(unique(paste(data$LAT, '-', data$LON)))

location <- data %>% 
  group_by(ID, LAT, LON) %>%
  slice(1) %>%
  ungroup() %>% 
  select(ID, LAT, LON)

# two weather stations at location: 50.90212
location %>% group_by(LAT) %>%
  summarize(count = n()) %>%
  data.frame

# Weather station 63 and 71 are located at same point
location %>% 
  filter(LAT > 50.902 & LAT  < 50.903) %>%
  data.frame

# also same altitude
data %>% 
  filter(ID %in% c('GARMON063', 'GARMON071')) %>%
  group_by(ID) %>%
  slice(1) 

# Station 63 heeft slechts 8 observaties en staat op dezelfde locatie als station 71
# sation 93 heeft slechts 40 observaties
# station 96 heeft lsechts 1679 observaties, maar dit komt omdat dat station pas later actief is geworden (16 januari)
time_range <- data %>% 
  group_by(ID) %>%
  summarize(num_observations = n(),
            first_obs = min(DATEUTC),
            last_obs = max(DATEUTC)) 

data.frame(time_range)


# All stations have +/- 277 observations per day
data %>%
  filter(as.Date(DATEUTC) == '2019-09-01') %>%
  group_by(ID) %>%
  summarize(num_observations = n()) %>%
  data.frame

# meting tot 23:00:00 (bij sommige ook 23:05:00, maar gebaseerd op slechts één record)
data %>%
  filter(as.Date(DATEUTC) == '2019-09-01') %>%
  group_by(ID) %>%
  filter(ID %in% c('GARMON002', 'GARMON005')) %>%
  group_by(DATEUTC) %>%
  summarize(count = n(),
            records = mean(NRECORDS)) %>%
  arrange(DATEUTC) %>%
  data.frame 

# minder records om 3:15:02 wat zou de reden hiervan kunnen zijn?
data %>%
  filter(as.Date(DATEUTC) == '2019-09-01') %>%
  group_by(ID) %>%
  filter(ID %in% c('GARMON002', 'GARMON006')) %>%
  group_by(DATEUTC) %>%
  summarize(count = n(),
            records = mean(NRECORDS)) %>%
  arrange(DATEUTC) %>%
  data.frame 



location <- data %>% 
  group_by(ID, LAT, LON) %>%
  slice(1) %>%
  ungroup() %>% 
  select(ID, LAT, LON)

require(leaflet)
leaflet(data = location) %>% addTiles() %>%
  addMarkers(~LON, ~LAT, popup = ~as.character(ID), label = ~as.character(ID))

data <- data %>%
  mutate(datum = as.Date(DATEUTC))

data_per_date <- data %>%
  group_by(datum, ID) %>%
  summarize(observations = n()) %>%
  group_by(datum) %>%
  summarize(observations = sum(observations),
            num_station = n(),
            avg_observations = observations / num_station)

ggplot(data_per_date) +
  theme_bw() +
  geom_point(aes(datum, num_station)) +
  ggtitle('number of active stations per date')

ggplot(data_per_date) +
  theme_bw() +
  geom_point(aes(datum, avg_observations)) +
  ggtitle('average number of observations per station per date')


evolution_weather <- data %>%
  group_by(datum) %>%
  summarize(temp = mean(TEMPC),
            humidity = mean(HUMIDITY),
            dew = mean(DEWPTC),
            windchill = mean(WINDCHILLF),
            winddir = mean(WINDDIR, na.rm = TRUE),
            windspeed = mean(WINDSPEEDKMH),
            windgust = mean(WINDGUSTKMH),
            rain = mean(RAININ),
            solar = mean(SOLARRADIATION),
            uv = mean(UV))

head(evolution_weather)  

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(datum, temp)) +
  ggtitle('evolution of avg temperature')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(datum, humidity)) +
  ggtitle('evolution of avg humidity')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(datum, dew)) +
  ggtitle('evolution of avg dew temperature')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(datum, (windchill-32) / 1.8)) +
  ggtitle('evolution of avg windchill')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(datum, windspeed)) +
  ggtitle('evolution of avg windspeed')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(datum, windgust)) +
  ggtitle('evolution of avg windgust')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(datum, rain)) +
  ggtitle('evolution of avg rain')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(datum, solar)) +
  ggtitle('evolution of avg solar')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(datum, uv)) +
  ggtitle('evolution of avg uv')
  
evolution_weather <- data %>%
  group_by(datum) %>%
  summarize(temp = mean(TEMPC),
            humidity = mean(HUMIDITY),
            dew = mean(DEWPTC),
            windchill = mean(WINDCHILLF),
            winddir = mean(WINDDIR, na.rm = TRUE),
            windspeed = mean(WINDSPEEDKMH),
            windgust = mean(WINDGUSTKMH),
            rain = mean(RAININ),
            solar = mean(SOLARRADIATION),
            uv = mean(UV))

require(chron)

hours = times(strftime(t, format="%T", tz = 'UTC'))

t <- data$DATEUTC[1:5]
t



times(st)

data <- data %>%
  mutate(time = times(strftime(DATEUTC, format="%T", tz = 'UTC')),
         WINDCHILLF = (WINDCHILLF-32) / 1.8) 


data <- data %>%
  mutate(date = as.Date(DATEUTC)) %>%
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

library(lubridate)

data <- data %>% 
  mutate(time_c = hour(date_time) + minute(date_time) / 60 + second(date_time) / 3600)


data %>% mutate(time_c = )

evolution_weather <- data %>%
  group_by(time_c) %>%
  summarize(temp = mean(temperature, na.rm = TRUE),
            humidity = mean(humidity, na.rm = TRUE),
            dew = mean(dew_point, na.rm = TRUE),
            windchill = mean(wind_chill, na.rm = TRUE),
            winddir = mean(wind_direction, na.rm = TRUE),
            windspeed = mean(wind_speed, na.rm = TRUE),
            windgust = mean(wind_gust, na.rm = TRUE),
            rain = mean(rain, na.rm = TRUE),
            solar = mean(solar_radiation, na.rm = TRUE),
            uv = mean(uv, na.rm = TRUE),
            count = n(),
            records = mean(num_record)) %>%
  filter(count > 100)

sum(evolution_weather$count)

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(time_c, records)) +
  ggtitle('evolution of avg temperature')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(time_c, temp)) +
  ggtitle('evolution of avg temperature')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(time_c, humidity)) +
  ggtitle('evolution of avg humidity')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(time_c, dew)) +
  ggtitle('evolution of avg dew temperature')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(time_c, (windchill-32) / 1.8)) +
  ggtitle('evolution of avg windchill')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(time_c, windspeed)) +
  ggtitle('evolution of avg windspeed')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(time_c, windgust)) +
  ggtitle('evolution of avg windgust')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(time_c, rain)) +
  ggtitle('evolution of avg rain')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(time_c, solar)) +
  ggtitle('evolution of avg solar')

ggplot(evolution_weather) +
  theme_bw() +
  geom_point(aes(time_c, uv)) +
  ggtitle('evolution of avg uv')
  

data %>% 
  group_by(num_record) %>%
  summarize(count = n()) %>%
  data.frame

data <- data %>% 
  filter(second(date_time) == 0,
         minute(date_time) %% 5 == 0)


station <- c(2, 4)
start <- as.Date("2019-11-01")
end <- as.Date("2019-11-15")
variables <- c('temperature', 'humidity')

visualize_station(c(1:30), start, end, c('wind_speed'))

visualize_station <- function(station, start, end, variables) {
  
  for(variable in variables) {
    df <- data %>%
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

location <- data %>% 
  group_by(id, lat, lon) %>%
  slice(1) %>%
  ungroup() %>% 
  select(id, lat, lon) %>%
  filter(id == 29)

require(leaflet)
leaflet(data = location) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(id), label = ~as.character(id))

time_range <- data %>% 
  group_by(id) %>%
  summarize(num_observations = n(),
            first_obs = min(date_time),
            last_obs = max(date_time)) %>%
  arrange(first_obs)

data.frame(time_range)


sel_id <- time_range %>% filter(first_obs < as.Date('2019-10-01')) %>% pull(id)

avg_temp <- data %>%
  filter(id %in% sel_id,
         date_time >= as.Date("2019-08-01"),
         date_time < as.Date("2019-09-30")) %>%
  group_by(id) %>%
  summarize(avg_temp = mean(temperature),
            lat = lat[1],
            lon = lon[1])

leaflet(data = avg_temp) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(avg_temp), label = ~as.character(avg_temp))



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
    boundary <- c(jump[1] - 1800*24, jump[1] + 1800*24)
  }
  
  
  if(length(jump) > 1) {
    for(i in 2:length(jump)) {
      if(jump[i] - 1800*24 <= boundary[length(boundary)]) {
        boundary[length(boundary)] <- jump[i] + 1800*24
      } else {
        boundary <- c(boundary, jump[i] - 1800*24, jump[i] + 1800*24)
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

