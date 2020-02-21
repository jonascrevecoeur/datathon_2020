rm(list = ls())
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

startX <- 134458
startY <- 87817

countX <- 72
countY <- 116

get_bounding_box <- function(x, y, zoom) {
  points <- rbind(XY2LonLat(x, y, zoom),
                  XY2LonLat(x+1, y+1, zoom))
  
  return(points)
}

LonLat2XY(4.700210, 50.878075, 16)

source("19-02-20 eval nn.R")
require(ggmap)

for(i in 1:countX) {
  coord <- matrix(nrow = countY, ncol = 2)
  for(j in 1:countY) {
    x <- startX + i
    y <- startY + j
    
    center <- colMeans(get_bounding_box(x, y, 18))
    
    coord[j, ] <- as.numeric(center)
  }
  result <- eval_change_nn(coord[, 1], coord[, 2])
  save(result, file = paste0("../Data/deltaPark/slice_", i, ".RData"))
}

for(i in 1:countX) {
  coord <- matrix(nrow = countY, ncol = 2)
  for(j in 1:countY) {
    x <- startX + i
    y <- startY + j
    
    center <- colMeans(get_bounding_box(x, y, 18))
    
    coord[j, ] <- as.numeric(center)
  }
  result <- eval_change_nn(coord[, 1], coord[, 2])
  save(result, file = paste0("../Data/deltaPark/slice_", i, ".RData"))
}

rows <- countX * countY
polygon <- vector('list', length = rows)
original <- rep(NA, rows)
change <- rep(NA, rows)
pct_green <- rep(NA, rows)

pos <- 1
for(i in 1:countX) {
  cat(i, '/', countX, '\n')
  load(file = paste0("../Data/deltaPark/slice_", i, ".RData"))
  for(j in 1:countY) {
    x <- startX + i
    y <- startY + j
    
    a <- as.numeric(XY2LonLat(x, y, 18))
    b <- as.numeric(XY2LonLat(x+1, y+1, 18))
    
    center <- colMeans(get_bounding_box(x, y, 18))
    
    require(sp)
    polygon[[pos]] <- Polygons(list(Polygon(cbind(c(a[1], a[1], b[1], b[1]), c(a[2], b[2], b[2], a[2])))), pos)
    
    original[pos] <- result$original[j]
    change[pos] <- result$updated[j] - result$original[j]
    hue <- get_hue(center[1], center[2], 'small')[24:40, 24:40]
    pct_green[pos] <- sum(hue > 0.26 & hue < 0.28) / 289
    pos <- pos + 1
  }
}

subset <- pct_green < 0.6
calibrated_change <- change/(1-pct_green)

polygon_change <- SpatialPolygons(polygon)
proj4string(polygon_change) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

save(original, change, pct_green, polygon_change, file = '../Data/change_model.RData')


col <- c(colorRampPalette(c('red', 'white'))(sum(change > 0)),
  colorRampPalette(c('green', 'white'))(sum(change < 0)))

require(leaflet)
pal <- colorNumeric(
  palette = col, 
  domain = change)


leaflet() %>%
  addTiles() %>%
  addPolygons(data = polygon_change, fillColor = pal(change), weight = 0, fillOpacity = 0.7)




pal <- colorNumeric(
  palette = colorRampPalette(c('green', 'white'))(8), 
  domain = seq(0, 0.35, by =0.05))


leaflet() %>%
  addTiles() %>%
  addPolygons(data = polygon_change[subset], fillColor = pal(pmin(calibrated_change[subset], 0)), weight = 0, fillOpacity = 0.7)

polygon <- polygon_change[subset]
pal <- colorNumeric(
  palette = colorRampPalette(c('green', 'white'))(8), 
  domain = seq(-0.35, 0, by =0.05))
delta <- -pmin(calibrated_change[subset], 0) * 0.9

save(polygon, pal, delta, file = '../presentation/data/leaflet_change.RData')
