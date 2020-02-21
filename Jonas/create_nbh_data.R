neighbour_map <- array(NA, dim = c(rows, 5, 5, 1))
neighbour_count <- rep(NA, rows)

score_coord <- data.frame(cbind(coord, prediction_nl))
colnames(score_coord) <- c('x', 'y', 'pred')

require(tidyverse)
for(i in 1:rows) {
  
  if(i %% 100 == 0) cat(i, '\n')
  
  df <- data.frame(x = rep(coord[i, 1] + -2:2, 5), y = rep(coord[i, 2] + -2:2, each = 5))
  
  df <- df %>%
    left_join(score_coord, by = c('x', 'y'))
  
  neighbour_map[i,,,1] <- matrix(df$pred, nrow = 5, byrow = TRUE)
  neighbour_count[i] <- sum(!is.na(df$pred))
}

neighbour_map_leuven <- array(NA, dim = c(length(prediction_leuven), 5, 5, 1))
neighbour_count_leuven <- rep(NA, length(prediction_leuven))

score_coord <- data.frame(cbind(coord_predict, prediction_leuven))
colnames(score_coord) <- c('x', 'y', 'pred')

require(tidyverse)
for(i in 1:nrow(score_coord)) {
  
  if(i %% 100 == 0) cat(i, '\n')
  
  df <- data.frame(x = rep(coord_predict[i, 1] + -2:2, 5), y = rep(coord_predict[i, 2] + -2:2, each = 5))
  
  df <- df %>%
    left_join(score_coord, by = c('x', 'y'))
  
  neighbour_map_leuven[i,,,1] <- matrix(df$pred, nrow = 5, byrow = TRUE)
  neighbour_count_leuven[i] <- sum(!is.na(df$pred))
}