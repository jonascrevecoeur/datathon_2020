source("../data wrangling/generate_tile.R")

get_hue <- function(lon, lat, size, source = 'osm') {
  
  if(size == 'large') {
    img <- generate_tile(lat, lon, 12, size = 64, source = source)
  } else {
    img <- generate_tile(lat, lon, 14, size = 64, source = source)
  }
  
  img_data <- image_data(img, 'rgb')
  matrix(rgb2hsv(r = as.numeric(img_data[1,,]), 
                 g = as.numeric(img_data[2,,]), 
                 b = as.numeric(img_data[3,,]), maxColorValue = 256)[1, ], nrow = 64)
}

eval_nn <- function(lon, lat) {
  require(ggmap)
  
  df <- data.frame()
  for(i in 1:length(lat)) {
    tile <- as.numeric(LonLat2XY(lon[i], lat[i], 16)[1:2])
    
    x <- rep(tile[1] + -2:2, 5)
    y <- rep(tile[2] + -2:2, each = 5)
    
    base <- as.numeric(XY2LonLat(tile[1], tile[2], 16))
    
    longitude <- rep(NA, 25)
    latitude <- rep(NA, 25)
    
    for(j in 1:25) {
      longitude[j] = XY2LonLat(x[j], y[j], 16)$lon - base[1] + lon[i]
      latitude[j] = XY2LonLat(x[j], y[j], 16)$lat - base[2] + lat[i]
    }
    
    df <- rbind(df, 
                data.frame(index = i,
                           lon = longitude,
                           lat = latitude,
                           row = rep(1:5, each = 5),
                           col = rep(1:5, 5)))  
  }
  
  map_data <- array(NA, dim = c(nrow(df), 64, 64, 2))
  
  for(i in 1:nrow(df)) {
    if(i %% 10 == 0) cat(i, '/', nrow(df), '\n')
  
    map_data[i,,,1] <- get_hue(df$lon[i], df$lat[i], 'small')
    map_data[i,,,2] <- get_hue(df$lon[i], df$lat[i], 'large')
  }
  
  require(keras)
  model_cont <- load_model_tf('../Data/modeltf/model_2_cont')
  model_nbh <- load_model_tf('../Data/modeltf/model_2_nbh')
  
  df$prediction <- predict(model_cont, map_data)
  
  nbh_map <- array(NA, dim = c(length(lat), 5, 5, 1))
  nbh_map[cbind(df$index, df$row, df$col, 1)] <- df$prediction
  
  center <- which(df$row == 3 & df$col == 3)
  result <- as.numeric(predict(model_nbh, list(map_data[center,,,], nbh_map)))
  
  return(result)
}

eval_nn2 <- function(lon, lat) {
  require(ggmap)
  
  df <- data.frame()
  for(i in 1:length(lat)) {
    tile <- as.numeric(LonLat2XY(lon[i], lat[i], 16)[1:2])
    
    x <- rep(tile[1] + -2:2, 5)
    y <- rep(tile[2] + -2:2, each = 5)
    
    base <- as.numeric(XY2LonLat(tile[1], tile[2], 16))
    
    longitude <- rep(NA, 25)
    latitude <- rep(NA, 25)
    
    for(j in 1:25) {
      longitude[j] = XY2LonLat(x[j], y[j], 16)$lon - base[1] + lon[i]
      latitude[j] = XY2LonLat(x[j], y[j], 16)$lat - base[2] + lat[i]
    }
    
    df <- rbind(df, 
                data.frame(index = i,
                           lon = longitude,
                           lat = latitude,
                           row = rep(1:5, each = 5),
                           col = rep(1:5, 5)))  
  }
  
  map_data <- array(NA, dim = c(nrow(df), 64, 64, 4))
  
  for(i in 1:nrow(df)) {
    if(i %% 10 == 0) cat(i, '/', nrow(df), '\n')
    
    map_data[i,,,1] <- get_hue(df$lon[i], df$lat[i], 'small')
    map_data[i,,,2] <- get_hue(df$lon[i], df$lat[i], 'large')
    map_data[i,,,3] <- get_hue(df$lon[i], df$lat[i], 'small', 'google')
    map_data[i,,,4] <- get_hue(df$lon[i], df$lat[i], 'large', 'google')
  }
  
  require(keras)
  model_cont <- load_model_tf('../Data/modeltf/model_2_cont')
  model_nbh <- load_model_tf('../Data/modeltf/model_2_nbh')
  
  df$prediction <- predict(model_cont, map_data)
  
  nbh_map <- array(NA, dim = c(length(lat), 5, 5, 1))
  nbh_map[cbind(df$index, df$row, df$col, 1)] <- df$prediction
  
  center <- which(df$row == 3 & df$col == 3)
  result <- as.numeric(predict(model_nbh, list(map_data[center,,,], nbh_map)))
  
  return(result)
}

eval_change_nn <- function(lon, lat) {
  require(ggmap)
  
  df <- data.frame()
  for(i in 1:length(lat)) {
    tile <- as.numeric(LonLat2XY(lon[i], lat[i], 16)[1:2])
    
    x <- rep(tile[1] + -2:2, 5)
    y <- rep(tile[2] + -2:2, each = 5)
    
    base <- as.numeric(XY2LonLat(tile[1], tile[2], 16))
    
    longitude <- rep(NA, 25)
    latitude <- rep(NA, 25)
    
    for(j in 1:25) {
      longitude[j] = XY2LonLat(x[j], y[j], 16)$lon - base[1] + lon[i]
      latitude[j] = XY2LonLat(x[j], y[j], 16)$lat - base[2] + lat[i]
    }
    
    df <- rbind(df, 
                data.frame(index = i,
                           lon = longitude,
                           lat = latitude,
                           row = rep(1:5, each = 5),
                           col = rep(1:5, 5)))  
  }
  
  map_data <- array(NA, dim = c(nrow(df), 64, 64, 4))
  
  for(i in 1:nrow(df)) {
    if(i %% 10 == 0) cat(i, '/', nrow(df), '\n')
    
    map_data[i,,,1] <- get_hue(df$lon[i], df$lat[i], 'small')
    map_data[i,,,2] <- get_hue(df$lon[i], df$lat[i], 'large')
    map_data[i,,,3] <- get_hue(df$lon[i], df$lat[i], 'small', 'google')
    map_data[i,,,4] <- get_hue(df$lon[i], df$lat[i], 'large', 'google')
  }
  
  require(keras)
  model_cont <- load_model_tf('../Data/modeltf/model_2_cont')
  model_nbh <- load_model_tf('../Data/modeltf/model_2_nbh')
  
  df$prediction <- predict(model_cont, map_data)
  
  nbh_map <- array(NA, dim = c(length(lat), 5, 5, 1))
  nbh_map[cbind(df$index, df$row, df$col, 1)] <- df$prediction
  
  center <- which(df$row == 3 & df$col == 3)
  original_result <- as.numeric(predict(model_nbh, list(map_data[center,,,], nbh_map)))
  
  map_data[center, 24:40, 24:40, 1] <- 92/360
  
  df$prediction <- predict(model_cont, map_data)
  
  nbh_map <- array(NA, dim = c(length(lat), 5, 5, 1))
  nbh_map[cbind(df$index, df$row, df$col, 1)] <- df$prediction
  
  updated_result <- as.numeric(predict(model_nbh, list(map_data[center,,,], nbh_map)))
  
  result <- data.frame(original = original_result, updated = updated_result)
  
  return(result)
}


