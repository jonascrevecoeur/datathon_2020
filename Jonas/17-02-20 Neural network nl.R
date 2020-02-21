rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

load("../Data/tile_temp.RData")

require(magick)

156543.03392 * cos(50 * pi / 180) / 2^16 * 256/4

threshold_na <- 0.01

get_hue <- function(file) {
  img <- image_read(file)
  img_data <- image_data(img, 'rgb')
  matrix(rgb2hsv(r = as.numeric(img_data[1,,]), 
          g = as.numeric(img_data[2,,]), 
          b = as.numeric(img_data[3,,]), maxColorValue = 256)[1, ], nrow = 64)
}

rows <- sum(temperature_na < threshold_na)

osm_map <- array(NA, dim = c(rows, 64, 64, 4))
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
    
    file_small <- paste0('../Data/google/16s/',x,'/',y,'.png')
    file_large <- paste0('../Data/google/16x14s/',x,'/',y,'.png')
    
    osm_map[pos,,,3] <- get_hue(file_small)
    osm_map[pos,,,4] <- get_hue(file_large)
    
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

# save(osm_map, score, coord, polygon_nl, coord_lonlat, pos, file = '../Data/img_dataset_nl_google_only.RData')
# load(file = '../Data/img_dataset_nl_google.RData')

startX <- 33559
startY <- 21945
countX <- 101
countY <- 58
osm_map_predict <- array(NA, dim = c(countX * countY, 64, 64, 2))
coord_predict <- matrix(NA, nrow = countX * countY, ncol = 2)
coord_lonlat_predict <- matrix(NA, nrow = countX * countY, ncol = 2)

polygon_predict <- vector('list', length = countX * countY)

pos = 1;
for(i in 1:countX) {
  cat(i, '/', countX, '\n')
  for(j in 1:countY) {
    
    x <- startX + i
    y <- startY + j
    
    file_small <- paste0('../Data/osm/16s/',x,'/',y,'.png')
    file_large <- paste0('../Data/osm/16x14s/',x,'/',y,'.png')
    
    osm_map_predict[pos,,,1] <- get_hue(file_small)
    osm_map_predict[pos,,,2] <- get_hue(file_large)
    
    file_small <- paste0('../Data/google/16s/',x,'/',y,'.png')
    file_large <- paste0('../Data/google/16x14s/',x,'/',y,'.png')
    
    osm_map_predict[pos,,,3] <- get_hue(file_small)
    osm_map_predict[pos,,,4] <- get_hue(file_large)
    
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

#save(osm_map_predict, polygon_leuven, coord_lonlat_predict, coord_predict, file = '../Data/img_dataset_be_google_only.RData')
#load(file = '../Data/img_dataset_be_google.RData')

require(keras)

training <- which(runif(rows) <= 0.8)
test <- which(!(1:rows %in% training))

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
      epochs = 20,
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

pal <- colorNumeric(
  palette = colorRampPalette(c('blue', 'red'))(6), 
  domain = 0:5/2)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lat = coord_lonlat_predict[, 2], lng = coord_lonlat_predict[, 1],
                   color = pal(prediction_leuven))

pal <- colorNumeric(
  palette = colorRampPalette(c('white', 'green'))(8), 
  domain = seq(0, 0.35, by =0.05))

fig <- ggplot() +
  geom_point(aes(0:5, 0:5, color = (0:5)/2)) + 
  scale_colour_gradientn('', colours = pal(seq(0, 0.35, 0.05)), limits=c(0, 0.35), guide = guide_colourbar(barwidth=20, barheight=2)) +
  theme(legend.position = 'bottom')


extractLegend <- function(gg) {
  grobs <- ggplot_gtable(ggplot_build(gg))
  foo <- which(sapply(grobs$grobs, function(x) x$name) == "guide-box")
  grobs$grobs[[foo]]
}

plot(extractLegend(fig))

  scale_fill_continuous(guide = "colourbar", co)


leaflet() %>%
  addTiles() %>%
  addPolygons(data = polygon_leuven, fillColor = pal(pmin(prediction_leuven, 2.5)), weight = 0, fillOpacity = 0.7)

#save(polygon_leuven, pal, prediction_leuven, file = '../presentation/data/leaflet_model_step1.RData')

prediction_nl <- predict(model_cont, osm_map)

pal <- colorNumeric(
  palette = colorRampPalette(c('green', 'red'))(length(prediction_nl)), 
  domain = prediction_nl)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = polygon_nl, fillColor = pal(prediction_nl), weight = 0, fillOpacity = 0.7)

#### add neighbours info ####
source("create_nbh_data.R")

require(keras)
input_image <- layer_input(c(64, 64, 4))
input_nbh <- layer_input(c(5, 5, 1))

model_image <- input_image %>%
  layer_dropout(0.05) %>%
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

save(predictions, actual_score, file = '../presentation/data/nn_score.RData')

ggplot() +
  theme_bw() + 
  geom_point(aes(predictions[, 1], actual_score))

cor(predictions, actual_score, method = 'spearman')


hasnbh_leuven <- which(neighbour_count_leuven == 25)
predict_leuven2 <- predict(model2, list(osm_map_predict[hasnbh_leuven,,,], neighbour_map_leuven[hasnbh_leuven,,,,drop=FALSE]))

pal <- colorNumeric(
  palette = colorRampPalette(c('green', 'red'))(length(predict_leuven2)), 
  domain = predict_leuven2)

pal <- colorNumeric(
  palette = colorRampPalette(c('blue', 'red'))(6), 
  domain = 0:5/2)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = polygon_leuven[hasnbh_leuven], fillColor = pal(pmin(predict_leuven2, 2.5)), weight = 0, fillOpacity = 0.7)

polygon <- polygon_leuven[hasnbh_leuven]


save_model_tf(model_cont, '../Data/modeltf/model_2_cont')
save_model_tf(model2, '../Data/modeltf/model_2_nbh')
