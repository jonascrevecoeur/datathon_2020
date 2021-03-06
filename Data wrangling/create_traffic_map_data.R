rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

library('magick')

all_tile_exist <- function(tileX, tileY, zoom, source = 'osm') {
  if(source == 'osm') {
    for(i in 1:length(tileX)) {
      file <- paste("../Data/osm/", zoom, "/", tileX[i], "/", tileY[i], ".png", sep = "")
      if(!file.exists(file)) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

get_tile <- function(x, y, zoom, source = 'osm') {
  require(magick)
  
  if(source == 'osm') {
    file <- paste("../Data/osm/", zoom, "/", x, "/", y, ".png", sep = "")
    if(file.exists(file)) {
      return(image_read(file))
    } else {
      return(NA)
    }
    
  } else if(source == 'google') {
    
    if(!dir.exists(paste0("../Data/google/", zoom))) {
      dir.create(paste0("../Data/google/", zoom))
    }
    
    if(!dir.exists(paste0("../Data/google/", zoom, "/", x))) {
      dir.create(paste0("../Data/google/", zoom, "/", x))
    }
    
    file <- paste0("../Data/google/", zoom, "/", x, "/", y, ".jpg")
    if(!file.exists(file)) {
      download.file(url = paste0("http://mt1.google.com/vt/lyrs=s&x=", x, "&y=", y, "&z=", zoom), 
                    destfile = file,
                    mode = 'wb')
    } 
    
    return(image_read(file))
  }
}

create_map <- function(lat, lon, zoom, source = 'osm') {
  require(ggmap)
  result <- LonLat2XY(lon, lat, zoom)
  
  tileX <- c()
  tileY <- c()
  a <- 0; b <- 0;
  
  if(result$x > 128 & result$y > 128) {
    tileX <- c(result$X, result$X+1, result$X, result$X+1)
    tileY <- c(result$Y, result$Y, result$Y+1, result$Y+1)
    a = floor(result$x)
    b = floor(result$y)
  } else if(result$x > 128 & result$y <= 128) {
    tileX <- c(result$X, result$X+1, result$X, result$X+1)
    tileY <- c(result$Y-1, result$Y-1, result$Y, result$Y)
    a = floor(result$x)
    b = 256 + floor(result$y)
  } else if(result$x <= 128 & result$y > 128) {
    tileX <- c(result$X-1, result$X, result$X-1, result$X)
    tileY <- c(result$Y, result$Y, result$Y+1, result$Y+1)
    a = 256 + floor(result$x)
    b = floor(result$y)
  } else {
    tileX <- c(result$X-1, result$X, result$X-1, result$X)
    tileY <- c(result$Y-1, result$Y-1, result$Y, result$Y)
    a = 256 + floor(result$x)
    b = 256 + floor(result$y)
  }
  
  require(magick)
  
  if(!all_tile_exist(tileX, tileY, zoom, source)) {
    return(NULL)
  }
  
  tile <- c(get_tile(tileX[1], tileY[1], zoom, source),
            get_tile(tileX[2], tileY[2], zoom, source),
            get_tile(tileX[3], tileY[3], zoom, source),
            get_tile(tileX[4], tileY[4], zoom, source))
  
  big_tile <- image_append(c(image_append(tile[1:2]), image_append(tile[3:4])), 
                           stack = TRUE)
  
  return(image_crop(big_tile, paste("256x256+", (a-128), "+", (b-128), sep='')))
}

require(tidyverse)

create_traffic_map <- function(segment_id, zoom) {
  
  lat <- traffic_segment_geom %>% filter(id == segment_id) %>% pull(lat)
  lon <- traffic_segment_geom %>% filter(id == segment_id) %>% pull(lon)
  
  lat_center = mean(lat)
  lon_center = mean(lon)
  
  require(ggmap)
  result <- LonLat2XY(lon, lat, zoom) 
  result <- cbind(result$X * 256 + result$x, result$Y * 256 + result$y)
  
  result[, 1] <- result[, 1] - mean(result[, 1]) + 128
  result[, 2] <- result[, 2] - mean(result[, 2]) + 128
  
  osm <- create_map(lat_center, lon_center, zoom, 'osm')
  if(is.null(osm)) {
    cat('osm tiles not available for: ', segment_id, '\n')
    return();
  }
  gmap <- create_map(lat_center, lon_center, zoom, 'google')
  
  img <- image_draw(osm)
  rect(0, 0, 255, 255, col = 'black')
  lines(result[, 1], result[, 2], col = 'red', lwd = 3)
  dev.off()
  
  road_layer <- floor(apply(image_data(img, 'rgb')[1,,,drop=FALSE], 1:3, as.integer) / 255)
  
  osm_layer <- image_data(osm, 'rgb')
  osm_layer <- apply(osm_layer, 1:3, as.integer) / 255
  
  gmap_layer <- image_data(gmap, 'rgb')
  gmap_layer <- apply(gmap_layer, 1:3, as.integer) / 255
  
  require(abind)
  image_data <- abind(osm_layer, gmap_layer, road_layer, along = 1)
  
  save(image_data, file = paste0('../Data/traffic map/', segment_id, '_', zoom, '.RData'))
}

load("../Data/Traffic data_2.RData")

traffic_segment_geom <- traffic_segment_geom %>%
  group_by(id, part) %>%
  slice(1)

ids <- unique(traffic_segment_geom$id)

for(i in 1:length(ids)) {
  cat(i, '/', length(ids), '\n')
  create_traffic_map(ids[i], 16)
}