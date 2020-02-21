all_tile_exist <- function(tileX, tileY, zoom, source = 'osm') {
  if(source == 'osm') {
    for(i in 1:length(tileX)) {
      file <- paste("../Data/osm/", zoom, "/", tileX[i], "/", tileY[i], ".png", sep = "")
      if(!file.exists(file)) {
        return(FALSE)
      }
    }
  } else {
    for(i in 1:length(tileX)) {
      if(!dir.exists(paste0("../Data/google/", zoom))) {
        dir.create(paste0("../Data/google/", zoom))
      }
      
      if(!dir.exists(paste0("../Data/google/", zoom, "/", tileX[i]))) {
        dir.create(paste0("../Data/google/", zoom, "/", tileX[i]))
      }
      
      file <- paste0("../Data/google/", zoom, "/", tileX[i], "/", tileY[i], ".jpg")
      if(!file.exists(file)) {
        tryCatch(download.file(url = paste0("http://mt1.google.com/vt/lyrs=s&x=", tileX[i], "&y=", tileY[i], "&z=", zoom), 
                      destfile = file,
                      mode = 'wb'), error = function(e){})
        
      }
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


generate_tile <- function(lat, lon, zoom, size = 256, source = 'osm') {
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
  
  return(image_crop(big_tile, paste0(size, "x", size, "+", (a-size/2), "+", (b-size/2))))
}