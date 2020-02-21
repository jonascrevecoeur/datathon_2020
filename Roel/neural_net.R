rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

load("../Data/tile_temp.RData")

require(magick)

threshold_na <- 0.01

get_hue <- function(file) {
  img <- image_read(file)
  img_data <- image_data(img, 'rgb')
  matrix(rgb2hsv(r = as.numeric(img_data[1,,]), 
          g = as.numeric(img_data[2,,]), 
          b = as.numeric(img_data[3,,]), maxColorValue = 256)[1, ], nrow = 64)
}

rows <- sum(temperature_na < threshold_na)

osm_map <- array(NA, dim = c(rows, 64, 64, 2))
score <- matrix(NA, nrow = rows, ncol = 1)

coord <- matrix(NA, nrow = rows, ncol = 2)
coord_lonlat <- matrix(NA, nrow = rows, ncol = 2)

polygon <- vector('list', length = rows)

pos = 1;
for(i in 1:nrow(temperature_mean)) {
  cat(i, '/', nrow(temperature_mean), '\n')
  for(j in 1:ncol(temperature_mean)) {
    
    if(temperature_na[i, j] >= 0.1) next;
    if(pos > rows) next;
    
    if(pos %% 10 == 0) {
      cat(pos, '/', rows, '\n')
    }
    
    x <- startX + i
    y <- startY + j
    
    file_small <- paste0('../Data/osm/16s/',x,'/',y,'.png')
    file_large <- paste0('../Data/osm/16x14s/',x,'/',y,'.png')
    
    osm_map[pos,,,1] <- get_hue(file_small)
    osm_map[pos,,,2] <- get_hue(file_large)
    
    score[pos, 1] <- temperature_mean[i, j]
    coord[pos, ] <- c(x, y)
    coord_lonlat[pos, ] <- as.numeric(XY2LonLat(x, y, zoom))
    
    a <- as.numeric(XY2LonLat(x, y, zoom))
    b <- as.numeric(XY2LonLat(x+1, y+1, zoom))
    
    require(sp)
    polygon[[pos]] <- Polygons(list(Polygon(cbind(c(a[1], a[1], b[1], b[1]), c(a[2], b[2], b[2], a[2])))), pos)
    
    pos <- pos+1
  }
}
polygon_nl <- SpatialPolygons(polygon)
proj4string(polygon_nl) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# save(osm_map, score, coord, coord_lonlat, pos, file = '../Data/img_dataset_nl.RData')
# load(file = '../Data/img_dataset_nl.RData')

startX <- 33605
startY <- 21968
countX <- 27
countY <- 15
osm_map_predict <- array(NA, dim = c(countX * countY, 64, 64, 2))
coord_predict <- matrix(NA, nrow = countX * countY, ncol = 2)
coord_lonlat_predict <- matrix(NA, nrow = countX * countY, ncol = 2)

polygon_predict <- vector('list', length = countX * countY)

pos = 1;
for(i in 1:countX) {
  for(j in 1:countY) {
    
    x <- startX + i
    y <- startY + j
    
    file_small <- paste0('../Data/osm/16s/',x,'/',y,'.png')
    file_large <- paste0('../Data/osm/16x14s/',x,'/',y,'.png')
    
    osm_map_predict[pos,,,1] <- get_hue(file_small)
    osm_map_predict[pos,,,2] <- get_hue(file_large)

    coord_predict[pos, ] <- c(x, y)
    coord_lonlat_predict[pos, ] <- as.numeric(XY2LonLat(x, y, zoom))
    
    a <- as.numeric(XY2LonLat(x, y, zoom))
    b <- as.numeric(XY2LonLat(x+1, y+1, zoom))
    
    require(sp)
    polygon_predict[[pos]] <- Polygons(list(Polygon(cbind(c(a[1], a[1], b[1], b[1]), c(a[2], b[2], b[2], a[2])))), pos)
    
    pos <- pos+1
  }
}

polygon_leuven <- SpatialPolygons(polygon_predict)
proj4string(polygon_leuven) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#save(osm_map_predict, polygon_leuven, coord_lonlat_predict, coord_predict, file = '../Data/img_dataset_be.RData')
load(file = '../Data/img_dataset_be.RData')

require(keras)
category <- to_categorical(floor(score*2))
# category[, 5] <- category[, 5] + category[, 6]
# category[, 4] <- category[, 4] + category[, 5]
# category[, 3] <- category[, 3] + category[, 4]
# category[, 2] <- category[, 2] + category[, 3]
# category[, 1] <- category[, 1] + category[, 2]
# category <- category[, 2:6]

ggplot() +
  ggstyle +
  geom_density(aes(score))

require(keras)

model <- keras_model_sequential() %>%
  layer_dropout(0.05, input_shape = c(64, 64, 2)) %>%
  layer_conv_2d(filters = 16, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_dropout(0.05) %>%
  layer_conv_2d(filters = 16, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(filters = 32, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_batch_normalization() %>%
  layer_dense(16, activation = 'sigmoid') %>%
  layer_dropout(0.01) %>%
  layer_dense(6, activation = 'softmax') %>%
  compile(loss = 'categorical_crossentropy',
          metrics = c('accuracy'),
          optimizer = optimizer_rmsprop())

# 

training <- which(runif(rows) <= 0.8)
test <- which(!(1:rows %in% training))

model %>% 
  fit(x = osm_map[training,,,],
      y = category[training,],
      batch_size = 512,
      epochs = 20,
      validation_data = list(osm_map[test,,,], category[test,]))

model %>% evaluate(osm_map[test,,,], verbose = FALSE)

colSums(category)

predictions <- predict(model, osm_map[test,,,])
colSums(predictions)

colSums(category[test,])

predicted_category <- apply(predictions, 1, which.max)
actual_category <- apply(category[test,], 1, which.max)

table(predicted_category, actual_category)

colSums(category[901:1000, ])

ggplot() +
  theme_bw() +
  geom_point(aes(score[901:1000], res/2))

colSums(predictions)

ggplot() +
  ggstyle +
  geom_density(aes(score, color = 'score')) + 
  geom_density(aes(predictions, color = 'pred'))

ggplot() +
  ggstyle +
  geom_point(aes(score, predictions))

summary(predictions)

mean(score)
mean((score-mean(score))^2)

summary(model)

require(EnvStats)
ggplot() +
  ggstyle +
  geom_density(aes(score[score > 0, 1]^(1/8)))

model_cont <- keras_model_sequential() %>%
  layer_dropout(0.05, input_shape = c(64, 64, 2)) %>%
  layer_conv_2d(filters = 16, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_dropout(0.05) %>%
  layer_conv_2d(filters = 16, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(filters = 32, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_batch_normalization() %>%
  layer_dense(16, activation = 'relu') %>%
  layer_dropout(0.01) %>%
  layer_dense(1, activation = 'relu') %>%
  compile(loss = 'mse',
          metrics = c('mae'),
          optimizer = optimizer_rmsprop())

model_cont %>% 
  fit(x = osm_map[training,,,],
      y = score[training,],
      batch_size = 512,
      epochs = 10,
      validation_data = list(osm_map[test,,,], score[test,]))

predictions <- predict(model_cont, osm_map[test,,,])

actual_score <- score[test, 1]

ggplot() +
  theme_bw() + 
  geom_point(aes(predictions[, 1], actual_score))

ggplot() +
  ggstyle + 
  geom_density(aes(score, color = 'score')) +
  geom_density(aes(predictions))

cor(predictions, actual_score, method = 'spearman')


prediction_leuven <- predict(model_cont, osm_map_predict)

require(leaflet)
require(leaflet.extras)

pal <- colorNumeric(
  palette = colorRampPalette(c('green', 'red'))(length(prediction_leuven)), 
  domain = prediction_leuven)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lat = coord_lonlat_predict[, 2], lng = coord_lonlat_predict[, 1],
                   color = pal(prediction_leuven))

leaflet() %>%
  addTiles() %>%
  addPolygons(data = sps, fillColor = pal(prediction_leuven), weight = 0, fillOpacity = 0.7)

prediction_nl <- predict(model_cont, osm_map)

pal <- colorNumeric(
  palette = colorRampPalette(c('green', 'red'))(length(prediction_nl)), 
  domain = prediction_nl)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lat = coord_lonlat[, 2], lng = coord_lonlat[, 1],
                   color = pal(prediction_nl))

#### add neighbours info ####
source("create_nbh_data.R")

require(keras)
input_image <- layer_input(c(64, 64, 2))
input_nbh <- layer_input(c(5, 5, 1))

model_image <- input_image %>%
  layer_dropout(0.05, input_shape = c(64, 64, 2)) %>%
  layer_conv_2d(filters = 16, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_dropout(0.05) %>%
  layer_conv_2d(filters = 16, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(filters = 32, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_flatten() 
  
model_nbh <- input_nbh %>%
  layer_conv_2d(filters = 8, kernel_size = 3) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_flatten()

model_combined <- layer_concatenate(list(model_image, model_nbh)) %>%
  layer_batch_normalization() %>%
  layer_dense(16, activation = 'relu') %>%
  layer_dropout(0.01) %>%
  layer_dense(1, activation = 'relu')

model2 <- keras_model(list(input_image, input_nbh), model_combined) %>%
  compile(
    loss = 'mse',
    metrics = c('mae'),
    optimizer = optimizer_rmsprop()
  )

hasnbh <- which(neighbour_count == 25)
training2 <- intersect(hasnbh, training)
test2 <- intersect(hasnbh, test)

model2 %>% 
  fit(x = list(osm_map[training2,,,], neighbour_map[training2,,,,drop=FALSE]),
      y = score[training2,],
      batch_size = 512,
      epochs = 10,
      validation_data = list(list(osm_map[test2,,,], neighbour_map[test2,,,,drop=FALSE]) , score[test2,]))


predictions <- predict(model2, list(osm_map[test2,,,], neighbour_map[test2,,,,drop=FALSE]))

actual_score <- score[test2, 1]

ggplot() +
  theme_bw() + 
  geom_point(aes(predictions[, 1], actual_score))

cor(predictions, actual_score, method = 'spearman')


hasnbh_leuven <- which(neighbour_count_leuven == 25)
predict_leuven2 <- predict(model2, list(osm_map_predict[hasnbh_leuven,,,], neighbour_map_leuven[hasnbh_leuven,,,,drop=FALSE]))

pal <- colorNumeric(
  palette = colorRampPalette(c('green', 'red'))(length(predict_leuven2)), 
  domain = predict_leuven2)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = sps[hasnbh_leuven], fillColor = pal(predict_leuven2), weight = 0, fillOpacity = 0.7)
