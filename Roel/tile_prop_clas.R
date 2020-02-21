setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
osm_data <- '../Data/osm/'
osm_file <- '21864.png' 

use2hex <- hash::hash(
  'background' = '#F2EFE9',
  'aeroway' = '#9D9595',
  'motorway' = '#FD923A',
  'motorway_link' = '#FFE068',
  'major_road' = '#FFFD8B',
  'minor_road' = '#FFFFFF',
  'path' = ' #F8F6EF',
  'water' = '#A5BFDD',
  'sea' = '#99B3CC',
  'park' = ' #B5D29C',
  'forest' = '#CBD8C3',
  'industrial' = '#D1D0CD',
  'hospital' = '#E5C6C3',
  'sport' = '#D5E1DC')
hex2use <- hash::invert(use2hex)


require(magick)
require(tidyverse)


# Read the image
img <- image_read(osm_file)
print(img)

#' Count colors in a OSM tile
#' @param image image as read by \code{magick::image_read}
#' @param hex_color string vector of colors to count in hexadecimal format
#' @return tibble with columns 'hex', 'count' and 'freq'
count_colors <- function(image, hex_color){
  data <- image %>% image_data(channels = 'rgb') %>%
    apply(2:3, paste, collapse= '') %>% as.vector %>% 
    table() %>%  as_tibble() %>% setNames(c('hex', 'count'))
  data$hex <- paste0('#',toupper(data$hex))
  data <- merge(tibble(hex = hex_color),data, all.x = TRUE)
  data[is.na(data)] <- 0
  data <- data %>% mutate(freq = count/sum(count)) 
  return(data)
}

img_col <- img %>% count_colors(hex_color = hash::values(use2hex))


#' Create histogram of colors for a tile
#' @param data tibble as returned by the function \code{count_colors}
#' @return ggplot2 object
plot_colors <- function(data){
  ggplot(data) + geom_bar(aes(x = hex, y = freq, fill = I(hex)), stat = 'identity') +
    theme(axis.title = element_blank(), panel.background = element_rect(fill = 'thistle1'))
}

plot_colors(img_col %>% filter(count > 0))






# Change tiles using image_fill
img %>% image_fill(use2hex[['water']], point = '+100+200', fuzz = 5)
img %>% image_fill(use2hex[['park']], point = '+50+150', fuzz = 5)

img %>% image_annotate('h', location = '+50+150')




