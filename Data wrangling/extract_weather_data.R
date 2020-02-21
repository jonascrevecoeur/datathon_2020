rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(tidyverse)
library(rjson)
json <- fromJSON(file = "../data/downloaded weather data/sites_json.json")

locations <- json$features

siteId <- unlist(map(locations, function(x) x$properties$siteId))
lat <- map_dbl(locations, function(x) x$geometry$coordinates[2])
lon <- map_dbl(locations, function(x) x$geometry$coordinates[1])

subset <- lat > 47.94 & lat < 59.39 & lon > -13.32 & lon < 8.07

points <- data.frame(id = siteId[subset], lat = lat[subset], lon = lon[subset])

require(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lat = lat[subset], lng = lon[subset])

dlatlon <- function(lat1, lon1, lat2, lon2) {
  radius = 6371;
  dLat = deg2rad(lat2-lat1);  
  dLon = deg2rad(lon2-lon1); 
  a = sin(dLat/2)^2 + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLon/2) * sin(dLon/2)
  
  c = 2 * atan2(sqrt(a), sqrt(1-a)); 
  d = radius * c; 
  return(d * 1000);
}

deg2rad <- function(deg){
  return (deg * (pi/180))
}

intersect <- points %>%
  rename(lat_first = lat, 
         lon_first = lon,
         id_first = id) %>%
  merge(points, all = TRUE) %>%
  mutate(distance = dlatlon(lat_first, lon_first, lat, lon)) %>%
  filter(distance > 0, 
         distance < 5000)

leaflet() %>%
  addTiles() %>%
  addMarkers(lat = intersect$lat, lng = intersect$lon)


location <- unique(intersect$id)

location <- as.character(location)

require(RCurl)
for(i in 11:length(location)) {
  url <- paste0("https://wow.metoffice.gov.uk/observations/details/tableview/", location[i], "/export?firstDate=07-10-2019&lastDate=07-12-2019")
  
  out <- tryCatch(
    {
      res <- getURL(url, timeout = 6000)
      file <- read.csv(text = res, skip = 1)
      write.csv(file, file = paste0("../Data/downloaded weather data/", location[i], ".csv"))
    },
    error = function(e) {
      cat("error for id: ", location[i], '\n')
      return(0)
    }
  )
}

downloaded_weather_data <- data.frame();
for( i in 1:length(location)) {
  if(i %% 50 == 0) print(i)
  
  file <- paste0("../Data/downloaded weather data/", location[i], ".csv")
  if(!file.exists(file)) next;
  
  data <- read.csv(file)
  downloaded_weather_data <- rbind(downloaded_weather_data, data)
}

dim(downloaded_weather_data)
res <- map(downloaded_weather_data, ~sum(is.na(.)))
downloaded_weather_data[, names(res)[which(res > 0.8 * nrow(downloaded_weather_data))]] <- NULL

map(downloaded_weather_data, ~sum(is.na(.)))
#save(downloaded_weather_data, file = '../Data/downloaded_weather_data_1.RData')
load('../Data/downloaded_weather_data_1.RData')

downloaded_weather_data <- downloaded_weather_data %>%
  rename(id = Site.Id,
         date_time = Report.Date...Time,
         wind_gust = Wind.Gust,
         humidity = Relative.Humidity,
         dew_point = Dew.Point,
         wind_direction = Wind.Direction,
         wind_speed = Wind.Speed,
         rain = Rainfall.Rate,
         temperature = Air.Temperature)

downloaded_weather_data <- downloaded_weather_data %>%
  select(id, date_time, wind_gust, humidity, dew_point, wind_direction, wind_speed, rain, temperature)

downloaded_weather_data <- downloaded_weather_data %>%
  mutate(wind_speed = as.numeric(wind_speed))


require(chron)
require(lubridate)
downloaded_weather_data$date_time <- as.POSIXct(as.character(downloaded_weather_data$date_time), 
                                                "%Y-%m-%d %H:%M:%S", tz = "UTC")

downloaded_weather_data <- downloaded_weather_data %>%
  mutate(date = as.Date(date_time, tz = "UTC"),
         time_d = hour(date_time))

downloaded_weather_data <- downloaded_weather_data %>%
  filter(temperature > 0)

downloaded_weather_data <- downloaded_weather_data %>%
  group_by(id, date, time_d) %>%
  summarize(num_record = sum(!is.na(temperature)),
            temperature = mean(temperature, na.rm = TRUE),
            wind_gust = mean(wind_gust, na.rm = TRUE),
            humidity = mean(humidity, na.rm = TRUE),
            dew_point = mean(dew_point, na.rm = TRUE),
            wind_direction = mean(wind_direction, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE),
            rain = mean(rain, na.rm = TRUE))

downloaded_weather_data$date_time = as.POSIXct(downloaded_weather_data$date + minutes(downloaded_weather_data$time_d*60))

downloaded_weather_data <- downloaded_weather_data %>%
  ungroup() %>%
  mutate(time_c = time_d,
         time = times(strftime(date_time, format="%T", tz = 'UTC')))


downloaded_weather_data <- downloaded_weather_data %>%
  left_join(points, by = 'id')

dim(downloaded_weather_data %>%
      filter(!is.na(wind_gust),
             !is.na(humidity),
             !is.na(dew_point),
             !is.na(wind_direction),
             !is.na(wind_speed),
             !is.na(rain),
             !is.na(temperature)) %>%
      group_by(id, date) %>%
      mutate(count = n()) %>%
      filter(count == 24) )

downloaded_weather_data <- downloaded_weather_data %>%
      filter(!is.na(temperature)) %>%
      group_by(id, date) %>%
      mutate(count = n()) %>%
      filter(count == 24) 

coord <- downloaded_weather_data %>% 
  group_by(lat, lon) %>%
  slice(1) %>%
  select(id, date, lat, lon) %>%
  ungroup()

error_id <- coord$id[c(30, 40, 50, 54, 207, 196, 197, 178, 189, 162, 169, 
                               248, 242, 243, 232, 223, 224, 213, 295, 282, 264, 324,
                               357, 360, 454, 440, 427, 497, 489, 477, 558, 539,
                               601, 577, 570, 558, 620, 601, 689, 678)]

coord <- coord %>% 
  filter(!(id %in% error_id))


require(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lat = coord$lat, lng = coord$lon)

source("../Data wrangling/generate_tile.R")



create_weather_map <- function(station_id) {
  
  lat <- coord %>% filter(id == station_id) %>% pull(lat)
  lon <- coord %>% filter(id == station_id) %>% pull(lon)
  
  gmap <- generate_tile(lat, lon, 14, size = 64, 'google')
  
  gmap_layer <- image_data(gmap, 'rgb')
  gmap_layer <- apply(gmap_layer, 1:3, as.integer) / 255
  
  image_data <- gmap_layer
  
  save(image_data, file = paste0('../Data/weather map/', station_id, '.RData'))
}

for(i in 1:nrow(coord)) {
  create_weather_map(coord$id[i])
}

ids <- coord$id
weather_map_data <- array(NA, dim = c(length(ids), 3, 64, 64))
weather_map_data_hue <- array(NA, dim = c(length(ids), 1, 64, 64))
weather_map_id <- rep(NA, length(ids))

for(i in 1:length(ids)) {
  cat(i, '/', length(ids), '\n')
  
  file <- paste0('../Data/weather map/', ids[i], '.RData')
  if(file.exists(file)) {
    load(file)
    weather_map_id[i] <- ids[i]
    require(abind)
    require(keras)
    weather_map_data[i,,,] <- image_data
    
    weather_map_data_hue[i,1,,] <- matrix(
      rgb2hsv(r = as.numeric(image_data[1,,]), 
              g = as.numeric(image_data[2,,]), 
              b = as.numeric(image_data[3,,]), maxColorValue = 1)[1, ], nrow = 64)
  } 
}

coord <- downloaded_weather_data %>% 
  group_by(lat, lon, date) %>%
  slice(1) %>%
  select(id, date, lat, lon, date) %>%
  filter(!(id %in% error_id)) %>%
  ungroup()

  


intersect <- coord %>%
  rename(lat_first = lat, 
         lon_first = lon,
         id_first = id,
         date_first = date) %>%
  merge(coord, all = TRUE) %>%
  mutate(distance = dlatlon(lat_first, lon_first, lat, lon)) %>%
  filter(distance > 0, 
         distance < 5000,
         date == date_first) %>%
  arrange(date)

first <- match(intersect$id_first, weather_map_id)
second <- match(intersect$id, weather_map_id)

map_data <- array(NA, dim = c(nrow(intersect), 6, 64, 64))
map_data[,1:3,,] <- weather_map_data[first,,,]
map_data[,4:6,,] <- weather_map_data[second,,,]
map_data <- aperm(map_data, c(1,3,4,2))

map_data_hue <- array(NA, dim = c(nrow(intersect), 2, 64, 64))
map_data_hue[,1,,] <- weather_map_data_hue[first,,,]
map_data_hue[,2,,] <- weather_map_data_hue[first,,,]
map_data_hue <- aperm(map_data_hue, c(1,3,4,2))

temperature <- downloaded_weather_data %>%
  pivot_wider(id_cols = c(id, date), names_prefix = "time_", names_from = time_d, values_from = temperature) %>%
  arrange(id, date)

rain <- downloaded_weather_data %>%
  pivot_wider(id_cols = c(id, date), names_prefix = "time_", names_from = time_d, values_from = rain) %>%
  arrange(id, date)

wind_speed <- downloaded_weather_data %>%
  pivot_wider(id_cols = c(id, date), names_prefix = "time_", names_from = time_d, values_from = wind_speed) %>%
  arrange(id, date)

head(sort(apply(temperature, 1, function(x) sd(x[3:26]))))

weather_ts_id <- temperature$id
weather_ts_date <- temperature$date
weather_ts <- array(0, dim = c(nrow(temperature), 24, 1))
weather_ts[,,1] <- as.matrix(temperature[, 3:26])
weather_ts[,,2] <- as.matrix(rain[, 3:26])
weather_ts[,,3] <- as.matrix(wind_speed[, 3:26])

dates <- sort(unique(weather_ts_date))
ts <- array(NA, dim = c(0, 24, 1))
temperature_difference <- matrix(nrow = 0, ncol = 24)
for(i in 1:length(dates)) {
  sub <- weather_ts[weather_ts_date == dates[i],,,drop=FALSE]
  sub_id <- weather_ts_id[weather_ts_date == dates[i]]
  
  sel_first <- match(intersect$id_first[intersect$date == dates[i]], sub_id)
  sel_second <- match(intersect$id[intersect$date == dates[i]], sub_id)
  
  ts <- abind(ts, sub[sel_first,,,drop=FALSE ], along=1)
  
  temperature_difference <- rbind(temperature_difference, sub[sel_first,,1] - sub[sel_second,,1] - sub[sel_first,1,1] + sub[sel_second,1,1])
}


index <- 2911

map_data[index,,,3]*255
hsv <- rgb2hsv(r = as.numeric(map_data[index,,,1]), 
               g = as.numeric(map_data[index,,,2]), 
               b = as.numeric(map_data[index,,,3]), maxColorValue = 1)

rgb2hsv(r = 0, g = 1, b = 0, maxColorValue = 1)

intersect[index, ]
gmap1 <- generate_tile(intersect[index, 'lat_first'], intersect[index, 'lon_first'], 16, size = 256, 'google')
gmap1
gmap2 <- generate_tile(intersect[index, 'lat'], intersect[index, 'lon'], 16, size = 256, 'google')
gmap2
tiles <- hsv[1, ] < 140/360 & hsv[1, ] > 100/360
x = rep(1:64, 64)
y = rep(64:1, each = 64)
ggplot() +
  geom_tile(aes(x, y, fill = tiles))

mean(tiles)

train_id <- unique(intersect$id)
train_id <- train_id[sample(1:length(train_id))]
train_id <- train_id[1:floor(0.8 * length(train_id))]

training <- which(intersect$id %in% train_id)
validation <- which(!(intersect$id %in% train_id))

require(keras)
input_image <- layer_input(c(64, 64, 6))
input_ts <- layer_input(c(24, 1))

model_image <- input_image %>%
  layer_conv_2d(filters = 24, kernel_size = 3, activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(filters = 24, kernel_size = 3, activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(filters = 24, kernel_size = 3, activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_flatten()

model_ts <- input_ts %>%
  layer_flatten()

model_combined <- layer_concatenate(list(model_image, model_ts)) %>%
  layer_batch_normalization() %>%
  layer_dropout(0.05) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(0.05) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 24)

model <- keras_model(list(input_image, input_ts), model_combined) %>%
  compile(
    optimizer = "adam", 
    metrics = c('mse'), 
    loss = "mae"
  )

model %>% fit(
  list(map_data[training,,,], ts[training,,,drop=FALSE]),
  temperature_difference[training, ], 
  batch_size = 32, 
  epochs = 20, 
  validation_data = list(
    list(map_data[validation,,,,drop=FALSE], ts[validation,,,drop=FALSE]), 
    temperature_difference[validation, ]
  )
)
  
mean(abs(temperature_difference[validation, ]))

model_ts <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(24)) %>%
  layer_dense(64, activation = 'relu') %>%
  layer_dense(64, activation = 'relu') %>%
  layer_dense(24) %>%
  compile(
    optimizer = optimizer_rmsprop(), 
    metrics = c('mse'), 
    loss = "mse"
  )

model_ts %>% 
  fit(ts[training,,],
      temperature_difference[training, ],
      batch_size = 32,
      epochs = 100,
      validation_data = list(ts[validation,,], temperature_difference[validation, ]))

model_img <- keras_model_sequential() %>%
  layer_dropout(0.05) %>%
  layer_conv_2d(filters = 24, kernel_size = 3, input_shape = c(64, 64, 6)) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_dropout(0.05) %>%
  layer_conv_2d(filters = 24, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(0.05) %>%
  layer_dense(units = 24) %>%
  compile(
    optimizer = optimizer_rmsprop(), 
    metrics = c('mse'), 
    loss = "mse"
  )


model_img %>% 
  fit(map_data[training,,,],
      temperature_difference[training, ],
      batch_size = 32,
      epochs = 10,
      validation_data = list(map_data[validation,,,], temperature_difference[validation, ]))

mean(temperature_difference[validation, ]^2)

model_predict <- predict(model, list(map_data, ts))
model_predict <- predict(model, list(map_data_hue, ts))
model_predict <- predict(model_img, map_data)


index <- 220
ggplot() +
  theme_bw() +
  geom_point(aes(1:24, ts[validation[index],,1] + temperature_difference[validation[index], ], color='actual')) +
  geom_point(aes(1:24, ts[validation[index],,1] + model_predict[validation[index], ], color = 'pred')) +
  geom_point(aes(1:24, ts[validation[index],,1], color = 'original'))


index <- 8
ggplot() +
  theme_bw() +
  geom_point(aes(1:24, ts[training[index],,1] + temperature_difference[training[index], ], color='actual')) +
  geom_point(aes(1:24, ts[training[index],,1] + model_predict[training[index], ], color = 'pred')) +
  geom_point(aes(1:24, ts[training[index],,1], color = 'original'))




require(keras)
input_image <- layer_input(c(64, 64, 2))
input_ts <- layer_input(c(24, 3))

model_image <- input_image %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(filters = 24, kernel_size = 3, activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(filters = 8, kernel_size = 3, activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_flatten()

model_ts <- input_ts %>%
  layer_flatten()

model_combined <- layer_concatenate(list(model_image, model_ts)) %>%
  layer_batch_normalization() %>%
  layer_dropout(0.05) %>%
  layer_dense(units = 64, activation = 'tanh') %>%
  layer_dense(units = 24)

model <- keras_model(list(input_image, input_ts), model_combined) %>%
  compile(
    optimizer = "adam", 
    metrics = c('mse'), 
    loss = "mse"
  )

model %>% fit(
  list(map_data_hue[training,,,], ts[training,,]),
  temperature_difference[training, ], 
  batch_size = 32, 
  epochs = 20, 
  validation_data = list(
    list(map_data_hue[validation,,,], ts[validation,,]), 
    temperature_difference[validation, ]
  )
)



for(i in 66:70) {
  index <- 10*i + 1:10
  df <- downloaded_weather_data %>% 
    filter(id %in% ids[index]) %>%
    mutate(code = match(id, ids))
  
  print(ggplot(df) +
    ggstyle +
    geom_line(aes(x = date_time, y = temperature, color = factor(code))))
  
}


  
df <- downloaded_weather_data %>% 
  filter(id %in% ids) %>%
  mutate(code = match(id, unique(df$id)))
ggplot(df) +
  ggstyle +
  geom_line(aes(x = date_time, y = temperature, color = as.factor(code))) +
  theme(legend.position = 'none')
