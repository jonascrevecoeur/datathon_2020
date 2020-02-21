rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

library('magick')

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

require(tidyverse)
require(lubridate)

load(file = '../Data/Traffic data_2.RData')

ids <- unique(traffic_segment_geom$id)

traffic_segment_geom <- traffic_segment_geom %>%
  group_by(id, part) %>%
  slice(1)

distance <- c();
for(i in 1:length(ids)) {
  df <- traffic_segment_geom %>% 
    filter(id == ids[i])
  
  lat <- df %>% pull(lat)
  lon <- df %>% pull(lon)
  
  dist <- 0;
  for(j in 2:length(lat)) {
    dist = dist + dlatlon(lat[j], lon[j], lat[j-1], lon[j-1])
  }
  distance <- c(distance, dist)
}

#### create traffic map ####

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

#### shape file for new roads ####

require(rgdal)
library(raster) 
shapefile <- readOGR( 
  dsn= "C:/Users/u0110176/Downloads/CRAB_Adressenlijst_Shapefile/Shapefile" , 
  layer="CrabAdr",
  verbose=TRUE
)

gem <- unique(shapefile$GEMEENTE)

save(shapefile, file='../Data/addres.RData')

sub <- crop(shapefile, extent(4.64, 4.77, 50.85, 50.94))
save(sub, file = '../data/shapefiles/leuven.RData')

leuven <- subset(shapefile, GEMEENTE=="Leuven")

test <- subset(leuven, STRAATNM=="Gemeentestraat")
coord <- coordinates(spTransform(leuven, CRS("+proj=longlat +datum=WGS84")))
colnames(coord) <- c('lon', 'lat')
df <- data.frame(coord, straat = leuven$STRAATNM, nr = leuven$HUISNR, postcode = leuven$POSTCODE)

leuven_adres <- df %>% 
  group_by(straat, nr, postcode) %>%
  slice(1) %>%
  ungroup()

plot(shapefile[1, ])

shp <- shapefile[1, ]

file.exists("../Data/shapefiles/gis_osm_roads_free_1.shp")
my_spdf$layer

library(raster)
sub <- crop(shapefile, extent(4.64, 4.77, 50.85, 50.94))



small <- crop(sub, extent(4.68, 4.70, 50.87, 50.88))

library(shp2graph)
test <- readshpnw(sub, ELComputed=TRUE)
graph <- nel2igraph(test[[2]], test[[3]], weight=test[[4]])

start <- c(50.87500, 4.699572)
end <- c(50.884339, 4.694543)

lon_all <- map_dbl(test[[2]][, 2], function(x)x[1])
lat_all <- map_dbl(test[[2]][, 2], function(x)x[2])

start_pt <- order((lat_all - start[1])^2 + (lon_all - start[2])^2)
end_pt <- order((lat_all - end[1])^2 + (lon_all - end[2])^2)

testPath <- get.shortest.paths(graph, from=start_pt[3], to=end_pt[4])

edges <- as.numeric(testPath$vpath[[1]])

points <- test[[2]][edges, 2]
lon <- map_dbl(points, function(x)x[1])
lat <- map_dbl(points, function(x)x[2])



require(leaflet)
leaflet() %>% 
  addTiles() %>% 
  addPolylines(lat = lat, lng = lon)

#### osm file to sql to R? ####

require(readr)
file = read_file("../Data/osm maps/at/at_2po_4pgr.sql")

library(DBI)
# Connect to the default postgres database
con <- dbConnect(RPostgres::Postgres(), user = 'postgres', password = 'admin')

graph <- read.csv("../Data/osm maps/export.csv")
require(igraph)
?igraph

map <- graph_from_edgelist(matrix(c(graph$source, graph$target), ncol = 2))

E(map)$weight <- graph$cost
is_weighted(map)

map <- as.undirected(map)

get.shortest.paths(map, from=74008, to=78180)

start <- c(50.87500, 4.699572)
end <- c(50.879145, 4.701022)

lon_all <- graph$x1
lat_all <- graph$y1

start_pt <- graph$source[order((lat_all - start[1])^2 + (lon_all - start[2])^2)]
end_pt <- graph$source[order((lat_all - end[1])^2 + (lon_all - end[2])^2)]

result <- get.shortest.paths(map, from=start_pt[1], to=end_pt[1])
index <- match(as.numeric(result$vpath[[1]]), graph$source)

edges <- as.numeric(E(map))

require(leaflet)
leaflet() %>% 
  addTiles() %>% 
  addPolylines(lat = graph$y1[index], lng = graph$x1[index])


subset <- lon_all > 4.64 & lon_all < 4.77 & lat_all > 50.85 & lat_all < 50.94
subset <- lon_all > 4.68 & lon_all < 4.70 & lat_all > 50.87 & lat_all < 50.88
sum(subset)

lon <- c(graph$x1[subset], graph$x2[subset])
lat <- c(graph$y1[subset], graph$y2[subset])
group <- rep(1:sum(subset), 2)

sel <- c(1)


library(maptools)
library(sp)

df.sp <- data.frame(id = group, lon = lon, lat = lat )

#make df.sp a spatialdataframe
coordinates( df.sp ) <- c( "lon", "lat" )

#create a list per id
id.list <- sp::split( df.sp, df.sp[["id"]] )

#initialisation of counter
id <- 1

#for each id, create a line that connects all points with that id
for ( i in 2794:length(id.list )) {
  if(i %% 10 == 0) print(i)
  event.lines <- SpatialLines( list( Lines( Line( id.list[[i]][1]@coords ), ID = id ) ),
                               proj4string = CRS( "+init=epsg:4326" ) )
  if ( id == 1 ) {
    sp_lines  <- event.lines
  } else {
    sp_lines  <- spRbind( sp_lines, event.lines )
  }
  id <- id + 1
}

require(leaflet)
leaflet() %>% 
  addTiles() %>%
  addPolylines(data = sp_lines)
