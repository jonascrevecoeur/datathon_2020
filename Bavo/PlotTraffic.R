#%%%%%%%%%%%%%%%%%%%%#
# Plots traffic data #
#%%%%%%%%%%%%%%%%%%%%#

#### 0. Settings ####
load("Traffic data_2.Rdata")
DfTraffic       = traffic_data[, ]
DfTraffic$lon   = traffic_segment$lon[match(DfTraffic$id, traffic_segment$id)] 
DfTraffic$lat   = traffic_segment$lat[match(DfTraffic$id, traffic_segment$id)] 
DfTraffic$Month = format(as.yearmon(DfTraffic$date_time), "%B")
DfTraffic$Weekday = weekdays(DfTraffic$date_time)

DfTraffic = DfTraffic[which(DfTraffic$lon >= 4.6 & DfTraffic$lat >= 50.86), ]
DfTraffic = DfTraffic[which(DfTraffic$lon <= 4.72 & DfTraffic$lat <= 50.89), ]
DfTraffic = ddply(DfTraffic, .(id), function(x) {
  xUn = x[1, ]
  GetAddr = revgeo(xUn$lon, xUn$lat)
  x$Address = rep(GetAddr[[1]], nrow(x))
  return(x)
})
DfTraffic = DfTraffic[!sapply(DfTraffic$Address, function(x) grepl("House Number Not Found Street Not Found", x)), ]
DfTraffic = DfTraffic[which(DfTraffic$Weekday == "Saturday"), ]

DfTrafficJune = DfTraffic[which(DfTraffic$Month == "June"), ]
DfTrafficDec  = DfTraffic[which(DfTraffic$Month == "December"), ]

TrafficJune = ddply(DfTrafficJune, .(id), function(x) {
  xNew = data.frame(TotalNrCars = sum(x$car) / nrow(x), Address = 
                      gsub(paste0(", Leuven, Flanders, 3000, Belgium|, Leuven, Flanders, 3001, Belgium",
                                  "|, Leuven, Flanders, 3010, Belgium"), "", unique(x$A),
                           perl = T))
  xNew
  })
TrafficDec = ddply(DfTrafficDec, .(id), function(x) {
  xNew = data.frame(TotalNrCars = sum(x$car) / nrow(x), Address = 
                      gsub(paste0(", Leuven, Flanders, 3000, Belgium|, Leuven, Flanders, 3001, Belgium",
                                  "|, Leuven, Flanders, 3010, Belgium"), "", unique(x$A),
                           perl = T))
  xNew
})

TrafficJune = TrafficJune[TrafficJune$id %in% intersect(TrafficJune$id, TrafficDec$id), ]
TrafficDec  = TrafficDec[TrafficDec$id %in% TrafficJune$id, ]
TrafficJune$Month = "July"
TrafficDec$Month  = "December"
TrafficAll = rbind.data.frame(TrafficJune, TrafficDec)

ggplot(TrafficAll, aes(fill = Month, y = TotalNrCars, x = Address)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Total number of cars")


barplot(TrafficJune$TotalNrCars, names.arg = TrafficJune$Address, las = 2)



url = list(hostname = "telraam-api.net/v0/segments/active", 
           scheme = "https", query = list(request = "GetFeature", outputFormat = "application/json")) %>% 
  setattr("class", "url")
request = httr::build_url(url)
active = st_read(request)

MapLeuven = Spatial

DfSpat = st_as_sf(traffic_segment, coords = c("lon", "lat"))
DfSpat = SpatialPolygons(DfSpat)
DfSpat = fortify(DfSpat)

DfSpat = SpatialPointsDataFrame(traffic_segment[, c("lon", "lat")], traffic_segment)
mapview(DfSpat)


ggplot(DfSpat)
leaflet() %>% 
  addTiles(group = "CartoDB") %>% 
  setView(lng = 4.70, lat = 50.88, zoom = 11.5) %>% 
  addCircleMarkers(lng = traffic_segment$lon, lat = traffic_segment$lat, radius = 5,
                   color = 'red')
tm_shape(DfSpat) %>%
  tm_lines(col = "car") 
