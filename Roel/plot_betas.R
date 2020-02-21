setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(tidyverse)

betas <- readRDS('lm_betas.rds')
smooth_fit <- loess(beta ~ as.numeric(day), data = betas, span = 0.5)
betas <- betas %>% mutate(smooth = predict(smooth_fit))

betas %>% ggplot(aes(x = day, y = smooth)) + geom_line() + theme_bw()


betas %>% filter(day >= '2019-07-01', day <= '2019-09-01') %>% summarise(mean_beta = mean(smooth))
