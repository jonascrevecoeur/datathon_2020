setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(tidyverse)


load('extract_score.RData')


extract <- extract %>% mutate(diff_warm = q95 - median_warm,
                              diff_cold = q05 - median_cold,
                              day = factor(date))

summer <- extract %>% filter(date_time >= '2019-07-01', date_time <= '2019-10-01')



lm_warm <- lm(diff_warm ~ model_score:day, data = extract)
lm_warm

lm_cold <- lm(diff_cold ~ model_score:day, data = extract)
lm_cold


coef <- coefficients(lm_cold)
betas <- tibble(day = as.Date(gsub('model_score:day','',names(coef)[-1])),
                beta = coef[-1])

saveRDS(betas, file = 'lm_betas.rds')

betas %>% ggplot(aes(x = day, y = beta)) + geom_line()


betas <- readRDS('lm_betas.rds')
smooth_fit <- loess(beta ~ as.numeric(day), data = betas, span = 0.5)
betas <- betas %>% mutate(smooth = predict(smooth_fit))

betas %>% ggplot(aes(x = day, y = smooth)) + geom_line() + theme_bw()


tmp <- extract %>% filter(id == 2)
ggplot(tmp, aes(x = date_time, y = q05)) + geom_line()
