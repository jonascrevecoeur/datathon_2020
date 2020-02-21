rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(sp)

source("generate_tile.R")

large_zoom <- 14
original_zoom <- 16

location <- 'leuven'

if(location == 'nl') {
  left_corner <- c(3.933471, 52.482197)
  right_corner <- c(5.099232, 52.017551)
} else if(location == 'leuven') {
  left_corner <- c(4.35, 50.98)
  right_corner <- c(4.90, 50.78)
}

left_corner <- c(4.65, 50.95)
right_corner <- c(4.75, 50.85)

require(ggmap)
startX = as.numeric(LonLat2XY(left_corner[1], left_corner[2], original_zoom)[1])
startY = as.numeric(LonLat2XY(left_corner[1], left_corner[2], original_zoom)[2])
endX = as.numeric(LonLat2XY(right_corner[1], right_corner[2], original_zoom)[1])
endY = as.numeric(LonLat2XY(right_corner[1], right_corner[2], original_zoom)[2])

countX <- endX - startX
countY <- endY - startY

get_bounding_box <- function(x, y, zoom) {
  points <- rbind(XY2LonLat(x, y, zoom),
                  XY2LonLat(x+1, y+1, zoom))
  
  return(points)
}

for(i in 1:countX) {
  for(j in 1:countY) {
    if(j %% 10 == 0) {
      cat(i, '/', countX, ' - ', j, '/', countY, '\n')
    }
    
    x <- startX + i
    y <- startY + j
    
    center <- colMeans(get_bounding_box(x, y, original_zoom))
    
    tile_img <- generate_tile(center[2], center[1], large_zoom-2, size = 64, source = 'osm')
    dir <- paste0('../Data/osm/', original_zoom, 'x', large_zoom, 's/', x)
    if(!dir.exists(dir)) {
      dir.create(dir)
    }
    image_write(tile_img, path = paste0(dir, '/', y, '.png'))
    
    
    tile_img <- generate_tile(center[2], center[1], large_zoom-2, size = 64, source = 'google')
    if(!is.null(tile_img)) {
      dir <- paste0('../Data/google/', original_zoom, 'x', large_zoom, 's/', x)
      if(!dir.exists(dir)) {
        dir.create(dir)
      }
      image_write(tile_img, path = paste0(dir, '/', y, '.png'))
    }
    
    tile_img <- generate_tile(center[2], center[1], original_zoom-2, size = 64, source = 'osm')
    dir <- paste0('../Data/osm/', original_zoom, 's/', x)
    if(!dir.exists(dir)) {
      dir.create(dir)
    }
    image_write(tile_img, path = paste0(dir, '/', y, '.png'))
    

    tile_img <- generate_tile(center[2], center[1], original_zoom-2, size = 64, source = 'google')
    if(!is.null(tile_img)) {
      dir <- paste0('../Data/google/', original_zoom, 's/', x)
      if(!dir.exists(dir)) {
        dir.create(dir)
      }
      image_write(tile_img, path = paste0(dir, '/', y, '.png'))
    }
  }
}
