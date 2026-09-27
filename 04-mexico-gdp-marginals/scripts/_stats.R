# _stats.R --------------------------------------------------------------------
# Headline numbers for the income gradient. Sourced by the figures and by the
# post text so the graphic and the copy quote the same values.

# Population-weighted regression of log GDP per head on latitude. Weighting
# matters: an unweighted fit lets a band holding 60,000 people count as much as
# the one holding 18.7 million around Mexico City.
gradient_fit <- function(prof_lat) {
  d <- prof_lat[prof_lat$solid, ]
  m <- stats::lm(log(pc) ~ mid, data = d, weights = d$pop)
  b <- unname(stats::coef(m)[2])
  span <- max(d$mid) - min(d$mid)
  list(
    per_degree = exp(b) - 1,
    across = exp(b * span),
    span = span,
    t = unname(summary(m)$coefficients[2, 3]),
    rho = unname(stats::cor(d$mid, d$pc, method = "spearman")),
    n = nrow(d)
  )
}

headline_stats <- function(prof_lat, meta) {
  d <- prof_lat[prof_lat$solid, ]
  tot_gdp <- sum(prof_lat$gdp)
  tot_pop <- sum(prof_lat$pop)
  north <- prof_lat$mid >= 23.4367

  list(
    national = tot_gdp / tot_pop,
    grad = gradient_fit(prof_lat),
    rich = d[which.max(d$pc), ],
    poor = d[which.min(d$pc), ],
    extreme_ratio = max(d$pc) / min(d$pc),
    # Either side of the Tropic of Cancer, which cuts Mexico roughly in half by
    # area and is the line the whole gradient story turns on.
    north_pc = sum(prof_lat$gdp[north]) / sum(prof_lat$pop[north]),
    south_pc = sum(prof_lat$gdp[!north]) / sum(prof_lat$pop[!north]),
    north_gdp_share = sum(prof_lat$gdp[north]) / tot_gdp,
    north_pop_share = sum(prof_lat$pop[north]) / tot_pop
  )
}

# Income of each called-out city, over a metro-sized box rather than the single
# cell it sits in. The downscaling in this series puts a metro's people in the
# core cell and spreads its output over the neighbours, so the core cell alone
# badly understates a city: Guadalajara's centre cell reads $12,051 a head while
# every cell around it reads $33,000 to $40,000 and the metro box reads $18,749.
# Population-weighting over +/- `radius` degrees, about 55 km, fixes that.
city_income <- function(cities, grid_map, radius = 0.5) {
  vals <- vapply(seq_len(nrow(cities)), function(i) {
    hit <- which(abs(grid_map$x - cities$lon[i]) <= radius &
                 abs(grid_map$y - cities$lat[i]) <= radius)
    if (!length(hit)) return(NA_real_)
    sum(grid_map$gdp[hit]) / sum(grid_map$pop[hit])
  }, numeric(1))
  cities$pc <- vals
  cities
}

# Narrowest contiguous run of bands holding at least `target` of a column's total.
narrow_window <- function(p, col = "gdp", target = 0.5) {
  v <- p[[col]]; tot <- sum(v); n <- length(v)
  bin <- p$band[2] - p$band[1]
  best <- n + 1L; lo <- hi <- NA_integer_; j <- 1L; run <- 0
  for (i in seq_len(n)) {
    if (j < i) { j <- i; run <- 0 }
    while (j <= n && run < target * tot) { run <- run + v[j]; j <- j + 1L }
    if (run >= target * tot && (j - i) < best) { best <- j - i; lo <- i; hi <- j - 1L }
    run <- run - v[i]
  }
  list(lo = p$band[lo], hi = p$band[hi] + bin,
       width = p$band[hi] + bin - p$band[lo],
       share = sum(v[lo:hi]) / tot, total = sum(v[lo:hi]))
}
