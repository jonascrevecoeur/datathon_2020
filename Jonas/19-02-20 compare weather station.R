rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(magick)

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

deg2rad <- function(deg){
  return (deg * (pi/180))
}
get_hue <- function(file) {
  img <- image_read(file)
  img_data <- image_data(img, 'rgb')
  matrix(rgb2hsv(r = as.numeric(img_data[1,,]), 
                 g = as.numeric(img_data[2,,]), 
                 b = as.numeric(img_data[3,,]), maxColorValue = 256)[1, ], nrow = 64)
}

require(tidyverse)

load(file = "../Data/Weather data_2.RData")

#16 out of city
#37 iin city

sub37 <- weather_data %>% 
  filter(id == 37) %>%
  filter(date_time >= as.Date("2019-08-21"), date_time <= as.Date("2019-08-26"))

sub16 <- weather_data %>% 
  filter(id == 16) %>%
  filter(date_time >= as.Date("2019-08-21"), date_time <= as.Date("2019-08-26"))

save(sub37, sub16, file = '../presentation/data/station_data.RData')

ggplot() + 
  theme_bw() +
  geom_point(aes(sub37$date_time, sub37$temperature, color = 'Leuven center')) +
  geom_point(aes(sub16$date_time, sub16$temperature, color = 'Leuven suburbs')) +
  xlab('time') + ylab('temperature') + 
  ggtitle('Comparison temperature Leuven center and suburbs')

weather_data %>% 
  group_by(date) %>%
  summarize(count = n()) %>%
  arrange(date) %>% data.frame

weather_data <- weather_data %>%
  group_by(id, date) %>%
  mutate(count = n()) %>%
  filter(count == 288) %>%
  ungroup() %>%
  group_by(date_time) %>%
  mutate(med = median(temperature)) %>%
  ungroup()

weather_data <- weather_data %>% 
  group_by(id, date) %>%
  mutate(num_exceed_3 = sum(abs(temperature - med) > 3))

weather_data <- weather_data %>%
  filter(num_exceed_3 == 0) %>%
  mutate(count = n()) %>%
  filter(count == 288) %>%
  ungroup()

temperature <- weather_data %>%
  mutate(time_i = time_c * 12) %>%
  pivot_wider(id_cols = c(id, date), names_prefix = "time_", names_from = time_i, values_from = temperature) %>%
  arrange(id, date)

rain <- weather_data %>%
  mutate(time_i = time_c * 12) %>%
  pivot_wider(id_cols = c(id, date), names_prefix = "time_", names_from = time_i, values_from = rain) %>%
  arrange(id, date)

wind_speed_x <- weather_data %>%
  mutate(wind_speed_x = cos(wind_direction) * wind_speed) %>%
  mutate(time_i = time_c * 12) %>%
  pivot_wider(id_cols = c(id, date), names_prefix = "time_", names_from = time_i, values_from = wind_speed) %>%
  arrange(id, date)

wind_speed_y <- weather_data %>%
  mutate(wind_speed_x = sin(wind_direction) * wind_speed) %>%
  mutate(time_i = time_c * 12) %>%
  pivot_wider(id_cols = c(id, date), names_prefix = "time_", names_from = time_i, values_from = wind_speed) %>%
  arrange(id, date)

weather_ts <- array(0, dim = c(nrow(temperature), 288, 4))
weather_ts[,,1] <- as.matrix(temperature[, 3:290])
weather_ts[,,2] <- as.matrix(rain[, 3:290])
weather_ts[,,3] <- as.matrix(wind_speed_x[, 3:290])
weather_ts[,,4] <- as.matrix(wind_speed_y[, 3:290])

weather_ts_id <- select(temperature, c(id, date)) %>%
  mutate(index = 1:nrow(temperature))

location <- weather_data %>%
  group_by(id) %>%
  slice(1) %>%
  select(id, lat, lon, altitude)

require(leaflet) 

  leaflet() %>%
    addTiles() %>%
  addMarkers(lng = location$lon, lat = location$lat, label = as.character(location$id))



sub <- weather_data %>% 
  filter(id == 37) %>%
  filter(date_time >= as.Date("2019-08-28"), date_time <= as.Date("2019-08-30"))
dim(sub)

weather_data %>% 
  group_by(date) %>%
  summarize(count = n()) %>%
  arrange(date) %>% data.frame


source("19-02-20 eval nn.R")
location$model_score <- eval_nn2(location$lon, location$lat)

extract <- weather_data %>%
  left_join(location %>% select(id, model_score), by = 'id') %>%
  group_by(date, id) %>%
  mutate(q95 = quantile(temperature, c(273/288)),
         q05 = quantile(temperature, c(14/288)),
         dcold = abs(q05 - temperature),
         dwarm = abs(q95 - temperature),
         mcold = min(dcold),
         mwarm = min(dwarm)) %>%
  filter(dcold == mcold | dwarm == mwarm)



extract <- extract %>%
  mutate(time_cold = date_time[which.min(temperature)],
         time_warm = date_time[which.max(temperature)],
         median_cold = med[which.min(temperature)],
         median_warm = med[which.max(temperature)]) %>%
  slice(1)

save(extract, file = '../Data/extract_score.RData')

  
test %>% select(q05, q95, date, id, time_cold, time_warm)

delta <- weather_data %>% 
  group_by(id) %>%
  summarize(delta = mean(temperature-med))

ggplot() +
  geom_point(aes(location$model_score, delta$delta))

cor(location$model_score, delta$delta, method = 'spearman')

weather_ts_id <- weather_ts_id %>% 
  left_join(location, by = 'id')

intersect <- weather_ts_id %>% 
  rename(idA = id,
         index1A = index,
         lonA = lon,
         latA = lat,
         altitudeA = altitude,
         model_scoreA = model_score) %>%
  merge(weather_ts_id %>%
          rename(idB = id,
                 index1B = index,
                 lonB = lon,
                 latB = lat,
                 altitudeB = altitude,
                 model_scoreB = model_score),
        by = 'date') %>%
  mutate(distance = spatial_distance(latA, lonA, latB, lonB)) %>%
  filter(distance > 0,
         distance < 1000)

intersect <- intersect %>% 
  rename(date1 = date) %>%
  mutate(date2 = date1-1) %>%
  inner_join(weather_ts_id %>%
              select(id, date, index) %>%
              rename(date2 = date,
                     idB = id,
                     index2B = index),
            by = c('idB', 'date2'))



require(abind)
input_data_ts <- abind(weather_ts[intersect$index2B,,], weather_ts[intersect$index1B,,], along = 2)
output <- weather_ts[intersect$index1B,,1] - weather_ts[intersect$index1A,,1]

input_data_cov <- cbind(intersect$distance, 
                   intersect$model_scoreA, 
                   intersect$model_scoreB, 
                   intersect$altitudeA, 
                   intersect$altitudeB, 
                   as.numeric(intersect$date1))

ids <- unique(intersect$idA)
trainingId <- ids[which(runif(1:length(ids)) <= 0.8)]
training <- which(intersect$idA %in% trainingId)
test <- which(!(intersect$idA %in% trainingId))

input_ts <- layer_input(c(576, 4))
input_cov <- layer_input(c(6))

model_ts <- input_ts %>%
  layer_conv_1d(filters = 8, kernel_size = 4) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 16, kernel_size = 4) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 32, kernel_size = 4) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 32, kernel_size = 4) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten()

model_cov <- input_cov %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 32, activation = 'relu')

model <- layer_concatenate(list(model_ts, model_cov)) %>%
  layer_batch_normalization() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dense(units = 288)

model <- keras_model(list(input_ts, input_cov), model) %>%
  compile(loss = 'mae',
          optimizer = optimizer_rmsprop(),
          metrics = c('mse'))

model %>% 
  fit(list(input_data_ts[training,,], input_data_cov[training,]),
      output[training,,drop=FALSE],
      batch_size = 256,
      epoch = 20,
      validation_data = list(list(input_data_ts[test,,], input_data_cov[test,]),
                             output[test,,drop=FALSE]))

predictions <- predict(model, list(input_data_ts[test,,], input_data_cov[test,]))


ggplot() +
  geom_point(aes(1:288, predictions[1, ]), col= 'green') +
  geom_point(aes(1:288, output[1, ], col = 'blue'))



model <- keras_model_sequential() %>%
  layer_batch_normalization() %>%
  layer_dense(units = 32, activation = 'relu', input_shape = 6) %>%
  layer_dense(units = 72, activation = 'relu') %>%
  layer_reshape(c(72, 1)) %>%
  layer_conv_1d(filters = 4, kernel_size = 4, padding = 'same') %>%
  layer_upsampling_1d(size = 2) %>%
  layer_conv_1d(filters = 4, kernel_size = 4, padding = 'same') %>%
  layer_upsampling_1d(size = 2) %>%
  layer_conv_1d(filters = 1, kernel_size = 4, padding = 'same') %>%
  layer_flatten() %>%
  compile(loss = 'mse',
          optimizer = optimizer_rmsprop(),
          metrics = c('mse'))

model %>% 
  fit(input_data_cov[training,],
      output[training,,drop=FALSE],
      batch_size = 256,
      epoch = 10,
      validation_data = list(input_data_cov[test,], output[test,,drop=FALSE]))


predictions <- predict(model, input_data_cov[test,])

ggplot() +
  geom_point(aes(1:288, predictions[1, ], color = 'pred')) +
  geom_point(aes(1:288, output[1, ], color = 'actual'))

