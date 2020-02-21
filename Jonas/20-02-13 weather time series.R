rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(tidyverse)

load(file = "../Data/Weather data_2.RData")

weather_data <- weather_data %>%
  group_by(id, date) %>%
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

wind_speed <- weather_data %>%
  mutate(time_i = time_c * 12) %>%
  pivot_wider(id_cols = c(id, date), names_prefix = "time_", names_from = time_i, values_from = wind_speed) %>%
  arrange(id, date)

weather_ts <- array(0, dim = c(nrow(temperature), 288, 3))
weather_ts[,,1] <- as.matrix(temperature[, 3:290])
weather_ts[,,2] <- as.matrix(rain[, 3:290])
weather_ts[,,3] <- as.matrix(wind_speed[, 3:290])

weather_pt <- weather_data %>%
  group_by(id, date) %>%
  slice(1) %>%
  select(id, lat, lon, altitude, date) %>%
  arrange(id, date)

load(file = "../Data/Air data_4.RData")

air_data <- air_data %>% 
  group_by(id, date) %>%
  mutate(count = sum(!is.na(pm25))) %>%
  filter(count == 288,
         date >= as.Date('2019-06-29'))

pm25 <- air_data %>%
  mutate(time_i = time_c * 12) %>%
  pivot_wider(id_cols = c(id, date), names_prefix = "time_", names_from = time_i, values_from = pm25) %>%
  arrange(id, date)

pm100 <- air_data %>%
  mutate(time_i = time_c * 12) %>%
  pivot_wider(id_cols = c(id, date), names_prefix = "time_", names_from = time_i, values_from = pm100) %>%
  arrange(id, date)

air_ts <- array(0, dim = c(nrow(pm25), 288, 2))
air_ts[,,1] <- as.matrix(pm25[, 3:290])
air_ts[,,2] <- as.matrix(pm100[, 3:290])

air_pt <- air_data %>%
  group_by(id, date) %>%
  slice(1) %>%
  arrange(id, date)

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

intersect <- air_pt %>%
  ungroup() %>%
  rename(lat_air = lat, 
         lon_air = lon,
         id_air = id) %>%
  mutate(index_air = 1:nrow(air_pt)) %>%
  inner_join(weather_pt %>% 
               ungroup() %>%
               mutate(index_weather = 1:nrow(weather_pt)), by = 'date') %>%
  mutate(distance = dlatlon(lat_air, lon_air, lat, lon)) %>%
  filter(distance < 300)


input <- weather_ts[intersect$index_weather,,]
output_p25 <- air_ts[intersect$index_air,,1]

require(keras)
sel <- runif(1:nrow(input)) < 0.8
train <- which(sel)
test <- which(!sel)


model <- keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 3, input_shape = c(288, 3)) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 8, kernel_size = 3) %>%
  layer_flatten() %>%
  layer_dense(512, activation = 'relu') %>%
  layer_dense(128, activation = 'relu') %>%
  layer_dense(1, activation = NULL) %>%
  compile(loss = 'mse',
          optimizer = optimizer_rmsprop(),
          metrics = c('mse'))

model %>% 
  fit(input[train,,,drop=FALSE],
      output_p25[train,288],
      batch_size = 256,
      epoch = 100,
      validation_split = 0.2)

index <- train[40]
prediction <- model %>% 
  predict(input[index,,,drop = FALSE])

ggplot() +
  ggstyle +
  geom_line(aes(x = 1:288, y = as.numeric(prediction), color = 'pred')) +
  geom_line(aes(x = 1:288, y = output_p25[index, ], color = 'actual'))


plot_input <- function(index) {
  ggplot() +
    ggstyle +
    geom_point(aes(1:288, input[index,,1], color = 'temp')) +
    geom_point(aes(1:288, input[index,,2], color = 'rain')) +
    geom_point(aes(1:288, input[index,,3], color = 'wind'))
}


autoencoder_pollution <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = 'relu', input_shape = c(288)) %>%
  layer_dense(units = 32, activation = NULL) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 288, activation = NULL) %>%
  compile(loss = 'mse',
          optimizer = optimizer_rmsprop(),
          metrics = c('mse'))
  
autoencoder_pollution %>% 
  fit(output_p25[train,],
      output_p25[train,],
      batch_size = 256,
      epoch = 100,
      validation_split = 0.2)

autoencoder_pollution <- keras_model_sequential() %>%
  layer_conv_1d(filters = 128, kernel_size = 3, padding = 'same', input_shape = c(288, 1)) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 32, kernel_size = 3, padding = 'same') %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 8, kernel_size = 3, padding = 'same') %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(units = 36) %>%
  layer_reshape(target_shape = c(36, 1)) %>%
  layer_upsampling_1d(size = 2) %>%
  layer_conv_1d(filters = 8, kernel_size = 3, padding = 'same') %>%
  layer_upsampling_1d(size = 2) %>%
  layer_conv_1d(filters = 32, kernel_size = 3, padding = 'same') %>%
  layer_upsampling_1d(size = 2) %>%
  layer_flatten() %>%
  layer_dense(units = 288) %>%
  compile(loss = 'mse',
          optimizer = optimizer_rmsprop(),
          metrics = c('mse'))

autoencoder_pollution %>% 
  fit(k_expand_dims(output_p25[train,], axis = 3),
      output_p25[train,],
      batch_size = 256,
      epoch = 100,
      validation_split = 0.2)

index <- 100
prediction <- autoencoder_pollution %>% 
  predict(k_expand_dims(output_p25[test[index],,drop = FALSE], axis = 3))

ggplot() +
  ggstyle +
  geom_line(aes(x = 1:288, y = as.numeric(prediction), color = 'pred')) +
  geom_line(aes(x = 1:288, y = output_p25[test[index], ], color = 'actual'))
