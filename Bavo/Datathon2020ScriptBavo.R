#%%%%%%%%%%%%%%%%%%%%%%%#
# Datathon: Script Bavo #
#%%%%%%%%%%%%%%%%%%%%%%%#

#### 0. Settings and functions ####

#### 0.1 Load in data ####
pathData   = "C:/Users/u0095171/Dropbox/Datathon 2020/Data/"
RdataFiles = list.files(pathData, pattern = ".RData", full.names = T)
RdataFiles = RdataFiles[grepl("Air data_4|Weather data_2", RdataFiles, perl = T)]
lapply(RdataFiles, load, envir = .GlobalEnv)


#### 0.2 Costum functions, load packages ####
sumNArows <- function(Df) sum(apply(Df, 1, anyNA))
IQRN      <- function(x) IQR(x, na.rm = T) / (2 * qnorm(0.75))
RobStd    <- function(x) (x - median(x, na.rm = T)) / IQRN(x)

LibraryM(zoo, plyr, tmap)

#### 1. Data exploration ####

#### 1.1 Air data ####
CompleteObs = F

#### 1.1.1 Info from https://data.leuvenair.be/data-l.html ####
shell("start https://data.leuvenair.be/data-l.html")


# The PM2.5 measurements of the SDS011 sensor are much more reliable than the PM10 measurements. For long-term analyses,
# stick to the PM2.5 measurements. PM10 measurement can be used for peak detection only.
# The sensor will overestimate the PM concentration in humid conditions (relative humidty RH > ~75%) 
# and will understimate the PM concentrations in dry conditions (RH < ~50%).
# The measurements of the DHT sensor (temperature + humidty) are not so realiable. Try to use an external source
# for temperature and relative humidity.
# We saw some cases where the SDS011 sensor freaked out for several hours reporting very high values, and then 
# gradually returned to "normal" values. We believe this effect could be caused by some dirt inside the sensor.
# There is some intrinsic sensor-to-sensor variability present in the sensors, which might even change over time. 
# We have three sensors on exact the same location (8765, 13526 and 13528) that can be used to quantify this effect.
# Don't look for 1-to-1 relations with traffic: you won't find one! Traffic is not the main source of (primary) fine dust;
# woodburning is! Use the NO2 data from Curieuzeneuzen (also in metadata table below) if you want to study traffic relations.
# For reference values, use the RIO-model of IRCEL. More information here and here.
# Take a look at the available literature on the SDS011 sensor in this open dir before you reinvent the wheel.
# Take a look at the available analysis scripts provided by Influencair on Github.
# All timestamps are in UTC.
# For a crash course on air pollution, take a look at the introductory presentation of Frans Fierens (VMM/IRCEL).
# For some background on the Leuvenair project, take a look at this powerpoint presentation.

DfAir = air_data
DfAir = DfAir[, !colnames(DfAir) %in% c("pm100", "temperature", "humidity")]
if(CompletObs)
  DfAir = DfAir[complete.cases(DfAir), ]

DfAir = DfAir[!is.na(DfAir$pm25), ]
DfAir = DfAir[DfAir$pm25 <= 5e2, ]
DfAir$Month = format(as.yearmon(DfAir$date_time), "%B")
DfAir$Year  = format(as.yearmon(DfAir$date_time), "%Y")


lapply(DfAir, summary)
par(mfrow = c(3, 3))
Hist = dlply(DfAir, .(id), function(x) hist(x$pm25, main = unique(x$id)))
Hist = dlply(DfAir, .(id), function(x) plot(x$date_time, x$pm25, main = unique(x$id),
                                            xlab = "Date", ylab = "PM2.5", type = "l"))
par(op)

(PerID = ddply(DfAir, .(id), function(x) DescrContinuous(x$pm25, Type = "MedianAll")))

# Quite some observationss with Max = 999.900

#### 1.1.2 Construct maps ####
LibraryM(sp, leaflet, magrittr, leaflet.extras)
AirStat = jsonlite::fromJSON("https://data.leuvenair.be/data/LEUVENAIRmeta_final.json")
AirStat = sf::st_as_sf(AirStat, coords = c("LON", "LAT"))
AirStat2  = jsonlite::fromJSON ("https://data.leuvenair.be/data/LEUVENAIRmeta_final.json")
AirStat2$LAT %<>% as.numeric()
AirStat2$LON %<>% as.numeric()

map <- leaflet() %>%
  addTiles(urlTemplate = "https://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png") %>%
  addMarkers(lng = 4.9, lat = 50.87) %>% 
  leafem::addMouseCoordinates()
leaflet() %>% 
  addTiles(group = "CartoDB") %>% 
  addCircleMarkers(lng = AirStat2$LON, lat = AirStat2$LAT, radius = 5,
                   color = 'red')

MostMeas   = table(DfAir$date_time)
MostMeas   = names(MostMeas)[which.max(MostMeas)]
TestSample = DfAir[which(DfAir$date_time == MostMeas), ]  
  
AirStat2$pm25 = TestSample$pm25[match(AirStat2$SDS011ID, TestSample$id)]

leaflet() %>% 
  addTiles(group = "CartoDB") %>% 
  addHeatmap(lng = AirStat2$LON, lat = AirStat2$LAT, radius = 5, intensity = AirStat2$pm25,
             gradient = colorNumeric(c("blue", "red"), domain = range(AirStat2$pm25, na.rm = T)))


#### 1.1.2.1 Shiny for animation ####
CheckThis = cbind.data.frame(time = rep(SampleDates, unlist(lapply(TestSamples, nrow))),
                             do.call("rbind", TestSamples))
data_a1 = CheckThis
data_a1$Date = as.POSIXct(strptime(data_a1$time, "%Y-%m-%d %H:%M:%S"))

ui <- fluidPage(
  titlePanel("Evolution of pm 25 over time"),
  sliderInput("time", "Select Week Starting", 
              min(data_a1$Date), 
              max(data_a1$Date),
              value = min(data_a1$Date),
              step = 1,
              animate = animationOptions(interval = 1e3, loop = TRUE)),
  leafletOutput("mymap")
)



server <- function(input, output, session) {
  points <- reactive({
    data_a1 %>% 
      filter(Date == input$time)
  })
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles(group = "CartoDB") %>% 
      setView(lng = 4.70, lat = 50.88, zoom = 11.5) %>% 
      addHeatmap(data = points(), lng = ~ LON, lat = ~ LAT, 
                 radius = 5, intensity = ~ pm25)
  })
}

shinyApp(ui, server)


#### 1.2  Weather data ####
shell("start https://data.leuvenair.be/data-g.html")

#### 1.2.1 Data exploration ####
RmIDs        = c("GARMON038", "GARMON039")
weather_data = weather_data[!weather_data$id %in% RmIDs, ] 
lapply(weather_data, summary)

(PercNArows = sumNArows(weather_data) / nrow(weather_data))
sumNA(weather_data$wind_direction) / nrow(weather_data)

NrUnique(weather_data$id)


Df = weather_data
Df$Month = format(as.yearmon(weather_data$date_time), "%B")
Df$Year  = format(as.yearmon(weather_data$date_time), "%Y")

#### 1.2.2 Heatmap ####
GradColor = cbind.data.frame(Temp = sort(unique(Df$temperature)), 
                             Col  = colorRamps::blue2red(NrUnique(Df$temperature)), 
                             stringsAsFactors = F)

MostMeasures = table(Df$date_time)
MaxMeasures  = names(MostMeasures)[which.max(MostMeasures)]
DfSample = Df[which(Df$date_time == MaxMeasures), ]
leaflet() %>% 
  addTiles(group = "CartoDB") %>% 
  setView(lng = 4.70, lat = 50.88, zoom = 11.5) %>% 
  addHeatmap(data = DfSample, lng = ~ lon, lat = ~ lat, 
             radius = 5, intensity = ~ temperature,
             gradient = colorRamps::blue2red(NrUnique(Df$temperature)))

DfJuly = Df[which(Df$Month == "July"),]
MostMeasures = table(DfJuly$date_time)
MaxMeasures  = names(MostMeasures)[which.max(MostMeasures)]
DfSample = DfJuly[which(DfJuly$date_time == MaxMeasures), ]
leaflet() %>% 
  addTiles(group = "CartoDB") %>% 
  setView(lng = 4.70, lat = 50.88, zoom = 11.5) %>% 
  addHeatmap(data = DfSample, lng = ~ lon, lat = ~ lat, 
             radius = 5, intensity = ~ temperature,
             gradient = colorRamps::blue2red(NrUnique(DfSample$temperature)))

## Hottest day ##
HottestDay    = "2019-07-25 14:45:00"
DfSample      = DfJuly[which(DfJuly$date_time == HottestDay), ]

if(anyNA(DfSample$temperature))
  DfSample = DfSample[!is.na(DfSample$temperature), ]

DfSample      = DfSample[order(DfSample$temperature), ]
DfSample$Grad = GradColor[match(DfSample$temperature, GradColor$Temp), "Col"] 
leaflet() %>% 
  addTiles(group = "CartoDB") %>% 
  setView(lng = 4.70, lat = 50.88, zoom = 11.5) %>% 
  addHeatmap(data = DfSample, lng = ~ lon, lat = ~ lat, 
             radius = 5, intensity = ~ temperature,
             gradient = DfSample$Grad)

#### Even more dirty code ####
Df$Month = format(as.yearmon(weather_data$date_time), "%B")
Df$Year  = format(as.yearmon(weather_data$date_time), "%Y")

library(doSNOW)
Cl = makeCluster(8, type = "SOCK")
registerDoSNOW(Cl)
pb       = txtProgressBar(max = NrUnique(Df$id), style = 3)
progress = function(n) setTxtProgressBar(pb, n)
opts     = list(progress = progress)
AllRes = dlply(Df, .(id), 
               function(x) {
                 ddply(x, .(Year, Month), function(y) range(y$temperature))
               }, .parallel = T, .paropts = list(.options.snow = opts))
stopCluster(Cl)
close(pb)
AllRes


Gvd = Df[Df$id == unique(Df$id)[1], ]

par(mfrow = c(3, 3))

m = rbind(c(rep(1, 6)),
          rep(2:4, each = 2),
          rep(2:4, each = 2),
          rep(5:7, each = 2), 
          rep(5:7, each = 2), 
          rep(8:10, each = 2), 
          rep(8:10, each = 2))
layout(m)
tsFake <- barplot(0,0, axes=FALSE)
mtext(unique(x$Month), side = 1, cex = 2)
ddply(Gvd, .(Month), function(x) plot(x$date_time, x$temperature, type = "l",
                                      xlab = "Date", ylab = "Temperature (degrees celsius)", 
                                      main = unique(x$Month)))
par(op)


ggplot(Gvd, aes(x = date_time, y = temperature))
+ facet_wrap( ~ YEAR ) + 
  labs(title = paste0("Temperature id = ", unique(x$id)),
       y = "Temperature (degrees celsius))",
       x = "Date") + theme_bw(base_size = 15) +
  # adjust the x axis breaks
  scale_x_date(date_breaks = "5 years", date_labels = "%m-%Y")









