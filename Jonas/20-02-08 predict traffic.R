rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(tidyverse)

zoom <- 16
load("../Data/Traffic data_2.RData")
load(paste0('../Data/traffic map data', '_', zoom, '.RData'))

sel_id <- traffic_map_id

require(keras)
traffic_typical %>% 
  group_by(hour) %>%
  summarize(count = sum(!is.na(pedestrian)),
            total_count = n())

outcome <- traffic_typical %>%
  group_by(id, hour) %>%
  slice(1) %>%
  filter(hour >= 8,
         hour <= 16,
         id %in% sel_id) %>%
  arrange(hour) %>%
  pivot_wider(id_cols = c(id),
              names_from = hour,
              values_from = c(bike, car)) %>%
  data.frame

# both outcome and traffic data map are already in the same order!
sum(diff(match(sel_id, outcome$id)) != 1)

outcome <- as.matrix(outcome[,2:19])

train <- (runif(1:length(sel_id)) < 0.8)*1
test <- 1 - train

train <- which(train == 1)
test <- which(test == 1)

traffic_map_data <- aperm(traffic_map_data, c(1, 3, 4, 2))

# order images data has to be altered
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = 3, padding = 'same', input_shape = c(256, 256, 7)) %>%
  layer_max_pooling_2d(c(4, 4)) %>%
  layer_conv_2d(filters = 4, kernel_size = 3, padding = 'same', input_shape = c(256, 256, 7)) %>%
  layer_max_pooling_2d(c(4, 4)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = 'sigmoid') %>%
  layer_dense(units = 9, activation = 'sigmoid') %>%
  compile(loss = "binary_crossentropy",
          optimize = optimizer_adam(),
          metrics = c('mse'))

history <- model %>%
  fit(traffic_map_data[train,,,],
      outcome[train,10:18] > 150,
      batch_size = 16, 
      epochs = 10, 
      validation_split = 0.2)

model

prediction <- predict(model, traffic_map_data[test,,,])

