library(tidyverse)
library(haven)
library(estimatr)
library(lfe)
library(stargazer)
library(stats)


read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

lmb_data <- read_data("lmb-data.dta")

lmb_subset <- lmb_data %>% 
  filter(lagdemvoteshare>.48 & lagdemvoteshare<.52)

# lm_1 <- lm_robust(score ~ lagdemocrat, data = lmb_subset, clusters = id)
# lm_2 <- lm_robust(score ~ democrat, data = lmb_subset, clusters = id)
# lm_3 <- lm_robust(democrat ~ lagdemocrat, data = lmb_subset, clusters = id)

summary(lmb_subset %>% select(score,lagdemocrat,democrat,id))

lm_1 <- felm(score ~ lagdemocrat|0|0|id, data = lmb_subset)
lm_2 <- felm(score ~ democrat|0|0|id, data = lmb_subset)
lm_3 <- felm(democrat ~ lagdemocrat|0|0|id, data = lmb_subset)

summary(lm_1)
summary(lm_2)
summary(lm_3)

stargazer(lm_1,lm_2,lm_3,type="text")

# summary(lm_3,vcov=sandwich,diagnostics=TRUE)

# using all data----

#using all data (note data used is lmb_data, not lmb_subset)
lm_1d <- felm(score ~ lagdemocrat|0|0|id, data = lmb_data)
lm_2d <- felm(score ~ democrat|0|0|id, data = lmb_data)
lm_3d <- felm(democrat ~ lagdemocrat|0|0|id, data = lmb_data)

stargazer(lm_1d,lm_2d,lm_3d,type="text")

# adding centered cutoff ----
lmb_data <- lmb_data %>% 
  mutate(demvoteshare_c = demvoteshare - 0.5)

lm_1c <- felm(score ~ lagdemocrat+demvoteshare_c|0|0|id, data = lmb_data)
lm_2c <- felm(score ~ democrat + demvoteshare_c|0|0|id, data = lmb_data)
lm_3c <- felm(democrat ~ lagdemocrat + demvoteshare_c|0|0|id, data = lmb_data)

stargazer(lm_1c,lm_2c,lm_3c,type="text")

# add interactions and own ----
lm_1i <- felm(score ~ lagdemocrat * demvoteshare_c|0|0|id, data = lmb_data)
lm_2i <- felm(score ~ democrat * demvoteshare_c|0|0|id, data = lmb_data)
lm_3i <- felm(democrat ~ lagdemocrat * demvoteshare_c|0|0|id, data = lmb_data)

stargazer(lm_1i,lm_2i,lm_3i,type="text")

# add square interactions -----
lmb_data <- lmb_data %>% mutate(demvoteshare_sq = demvoteshare_c^2)

lm_1sq <- felm(score ~ lagdemocrat * demvoteshare_c + lagdemocrat * demvoteshare_sq|0|0|id, 
                  data = lmb_data)
lm_2sq <- felm(score ~ democrat * demvoteshare_c + democrat * demvoteshare_sq|0|0|id, 
                  data = lmb_data)
lm_3sq <- felm(democrat ~ lagdemocrat * demvoteshare_c + lagdemocrat*demvoteshare_sq|0|0|id, 
                  data = lmb_data)

stargazer(lm_1sq,lm_2sq,lm_3sq,type="text")

# 6. Do it in the filtered data ----
lmb_subset2 <- lmb_data %>% 
  filter(demvoteshare > .45 & demvoteshare < .55) %>%
  mutate(demvoteshare_sq = demvoteshare_c^2)

lm_1sq.f <- felm(score ~ lagdemocrat * demvoteshare_c + lagdemocrat * demvoteshare_sq|0|0|id, 
                 data = lmb_subset2)
lm_2sq.f <- felm(score ~ democrat * demvoteshare_c + democrat * demvoteshare_sq|0|0|id, 
                 data = lmb_subset2)
lm_3sq.f <- felm(democrat ~ lagdemocrat * demvoteshare_c + lagdemocrat*demvoteshare_sq|0|0|id, 
                 data = lmb_subset2)

stargazer(lm_1sq.f,lm_2sq.f,lm_3sq.f,type="text")

# 7.  lmb_data vs lmb_subset2 -----
#aggregating the data
categories <- lmb_subset2$lagdemvoteshare

demmeans <- split(lmb_subset2$score, cut(lmb_subset2$lagdemvoteshare, 100)) %>% 
  lapply(mean) %>% 
  unlist()

agg_lmb_subset2 <- data.frame(score = demmeans, lagdemvoteshare = seq(0.01,1, by = 0.01))

# plotting
lmb_subset2 <- lmb_subset2 %>% 
  mutate(gg_group = case_when(lagdemvoteshare > 0.5 ~ 1, TRUE ~ 0))

ggplot(lmb_subset2) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_subset2) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "lm", 
              formula = y ~ x + I(x^2)) +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)

ggplot(lmb_subset2) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_subset2) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "loess") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)

ggplot(lmb_subset2) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_subset2) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "lm") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)

# smooth kernels ----
smooth_dem0 <- lmb_subset2 %>% 
  filter(democrat == 0) %>% 
  select(score, demvoteshare)

smooth_dem0 <- as_tibble(ksmooth(smooth_dem0$demvoteshare, smooth_dem0$score, 
                                 kernel = "box", bandwidth = 0.1))

smooth_dem1 <- lmb_subset2 %>% 
  filter(democrat == 1) %>% 
  select(score, demvoteshare) %>% 
  na.omit()

smooth_dem1 <- as_tibble(ksmooth(smooth_dem1$demvoteshare, smooth_dem1$score, 
                                 kernel = "box", bandwidth = 0.1))

ggplot() + 
  # geom_smooth(aes(x, y), data = smooth_dem0) +
  # geom_smooth(aes(x, y), data = smooth_dem1) +
  geom_point(aes(x, y), data = smooth_dem0) +
  geom_point(aes(x, y), data = smooth_dem1) +
  geom_vline(xintercept = 0.5)

# regression discontinuty  ----
rdr <- rdrobust::rdrobust(y = lmb_subset2$score,
                x = lmb_subset2$demvoteshare, c = 0.5)
summary(rdr)

# McCrary test
rdd::DCdensity(lmb_subset2$demvoteshare, cutpoint = 0.5)

density <- rddensity::rddensity(lmb_data$demvoteshare, c = 0.5)
rddensity::rdplotdensity(density, lmb_data$demvoteshare)


