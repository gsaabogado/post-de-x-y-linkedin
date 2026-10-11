# 05_numbers.R ----------------------------------------------------------------
# Every number quoted in the tweets, the LinkedIn text and the figure titles,
# printed from the fields of 01 so none is typed by hand. Shares are shares of
# the territory (true cell areas). Rainfall volume is rainfall times area.
#
# Run from the post root: Rscript scripts/05_numbers.R
# OUTPUT: data/derived/numbers.rds (and the same numbers on screen)

library(dplyr)

FLD <- readRDS("data/derived/fields.rds")
land <- !is.na(FLD$tmean) & !is.na(FLD$ppt)
tm <- FLD$tmean[land]; pp <- FLD$ppt[land]; el <- FLD$elev[land]; a <- FLD$area[land]
stopifnot(!anyNA(a), abs(sum(a) - FLD$area_km2) < 1)
# A few coastal cells have a climate value and no terrain value; they count in
# every share and simply never meet a condition on altitude.
no_el <- sum(a[is.na(el)]) / sum(a)
message(sprintf("territory without a terrain value: %.4f%%", 100 * no_el))
stopifnot(no_el < 0.005)
share <- function(cond) sum(a[which(cond)]) / sum(a)
wmean <- function(v, cond = rep(TRUE, length(v))) weighted.mean(v[which(cond)], a[which(cond)])
q <- function(s, tab) approx(FLD$probs, tab, xout = s)$y

# Latitude of each cell, to compare with altitude as a predictor of temperature.
xs <- FLD$ext[1] + (col(land)[land] - 0.5) / ncol(land) * (FLD$ext[2] - FLD$ext[1])
ys <- FLD$ext[4] - (row(land)[land] - 0.5) / nrow(land) * (FLD$ext[4] - FLD$ext[3])
ll <- sf::sf_project(FLD$crs, "EPSG:4326", cbind(xs, ys))
lat <- ll[, 2]
set.seed(1848)
i <- sample(which(!is.na(el)), 4e5)     # a sample is plenty for three regressions
d <- data.frame(tm = tm[i], el = el[i] / 1000, lat = lat[i], a = a[i])
fit_el <- lm(tm ~ el, d, weights = a); fit_lat <- lm(tm ~ lat, d, weights = a)
fit_both <- lm(tm ~ el + lat, d, weights = a)

# Rainfall volume by territory, wettest land first.
o <- order(pp, decreasing = TRUE)
cum_area <- cumsum(a[o]) / sum(a); cum_vol <- cumsum((pp * a)[o]) / sum(pp * a)
vol_of_wettest <- function(s) approx(cum_area, cum_vol, xout = s)$y

num <- list(
  t_mean = wmean(tm), p_mean = wmean(pp),
  t_q = setNames(q(c(.05, .25, .5, .75, .95), FLD$q_t), c("p05", "p25", "p50", "p75", "p95")),
  p_q = setNames(q(c(.05, .25, .5, .75, .95), FLD$q_p), c("p05", "p25", "p50", "p75", "p95")),
  t_low = wmean(tm, el < 200), t_high = wmean(tm, el >= 2000),
  share_low = share(el < 200), share_high = share(el >= 2000), share_1000 = share(el >= 1000),
  r2_el = summary(fit_el)$r.squared, r2_lat = summary(fit_lat)$r.squared,
  r2_both = summary(fit_both)$r.squared,
  slope_el = unname(coef(fit_both)["el"]), slope_lat = unname(coef(fit_both)["lat"]),
  share_t_under18 = share(tm < 18), share_t_over24 = share(tm >= 24),
  share_p_under500 = share(pp < 500), share_p_under250 = share(pp < 250),
  share_p_over1500 = share(pp >= 1500), share_p_over2000 = share(pp >= 2000),
  vol_wettest10 = vol_of_wettest(.10), vol_wettest25 = vol_of_wettest(.25),
  vol_driest50 = 1 - vol_of_wettest(.50),
  # the four kinds of climate, split at the country's own medians
  hot_dry = share(tm >= q(.5, FLD$q_t) & pp < q(.5, FLD$q_p)),
  hot_wet = share(tm >= q(.5, FLD$q_t) & pp >= q(.5, FLD$q_p)),
  cool_dry = share(tm < q(.5, FLD$q_t) & pp < q(.5, FLD$q_p)),
  cool_wet = share(tm < q(.5, FLD$q_t) & pp >= q(.5, FLD$q_p)))

# World cities: how much of Mexico has their yearly temperature and rainfall.
# "Like" = within 1.5 degrees and within 20% of the rainfall, in the same data.
wc <- readRDS("in/world_cities_pool.rds")[, c("name", "tmean", "ppt")]
wc$like_share <- vapply(seq_len(nrow(wc)), function(k)
  share(abs(tm - wc$tmean[k]) <= 1.5 & abs(pp / wc$ppt[k] - 1) <= 0.20), 0)
wc$like_km2 <- wc$like_share * sum(a)
num$world <- arrange(wc, desc(like_share))

saveRDS(num, "data/derived/numbers.rds")
pr <- num; pr$world <- NULL
str(pr, digits.d = 4)
print(as.data.frame(num$world), digits = 4)
message("DONE!")
