# 00a_climatology_cluster.R -----------------------------------------------------
# Acquisition step 1 of 3, on a machine that holds ERA5-Land as daily files
# (it ran on the CMCC cluster in July 2026). Average temperature and yearly
# rainfall for a box around Mexico, over the 1991-2020 climate normal, from
# 0.1 degree ERA5-Land daily fields. It reads only the box from each global
# file and keeps a sum and a count of valid days per cell, so a missing day
# does not bias the average. The output is a small table (about 29,000 cells)
# that the repository carries as in/mx_climatology_1991_2020.rds.
#
# Expected files, one per month: <ERA5_T_DIR>/era5_2m_temperature_YYYY_MM.nc
# with the daily mean `t_mean` in Celsius, and
# <ERA5_P_DIR>/era5_total_precipitation_YYYY_MM.nc with the daily total `tp`
# in metres. Set ERA5_T_DIR, ERA5_P_DIR and CLIM_OUT_DIR in the environment.
#
# Run: Rscript scripts/00a_climatology_cluster.R

rm(list = ls()); gc()
set.seed(42)

library(tidyverse)
library(RNetCDF)

NORMAL   <- 1991:2020                     # WMO-style 30-year normal
need_env <- function(v) {
  x <- Sys.getenv(v)
  if (!nzchar(x)) stop("set ", v, " in the environment")
  x
}
T_SRC    <- need_env("ERA5_T_DIR")
P_SRC    <- need_env("ERA5_P_DIR")
OUT_DIR  <- need_env("CLIM_OUT_DIR")
OUT_RDS  <- file.path(OUT_DIR, "mx_climatology_1991_2020.rds")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Mexico bounding box in 0-360 longitude convention -----------
LON_MIN <- 241.0; LON_MAX <- 274.5        # -119.0 .. -85.5
LAT_MIN <- 14.0;  LAT_MAX <- 33.5

# ---- 1. Axes + window indices from the first temp file ------
f1  <- file.path(T_SRC, "era5_2m_temperature_1991_01.nc")
nc  <- open.nc(f1)
lon_axis <- as.numeric(var.get.nc(nc, "longitude"))   # 0 .. 359.9
lat_axis <- as.numeric(var.get.nc(nc, "latitude"))    # 90 .. -90 (decreasing)
close.nc(nc)

lon_idx <- which(lon_axis >= LON_MIN & lon_axis <= LON_MAX)
lat_idx <- which(lat_axis >= LAT_MIN & lat_axis <= LAT_MAX)
ilon0 <- min(lon_idx); nlon <- length(lon_idx)
ilat0 <- min(lat_idx); nlat <- length(lat_idx)
lon_w <- lon_axis[lon_idx]; lat_w <- lat_axis[lat_idx]
cat(sprintf("window: %d lon x %d lat = %d cells\n", nlon, nlat, nlon * nlat))

read_slab <- function(path, varname) {
  nc <- open.nc(path)
  on.exit(close.nc(nc))
  # file dim order (valid_time, lat, lon) -> RNetCDF returns (lon, lat, time)
  var.get.nc(nc, varname, start = c(ilon0, ilat0, 1),
             count = c(nlon, nlat, NA), collapse = FALSE, unpack = TRUE)
}

# ---- 2. Accumulate over the normal period -------------------
sumT <- matrix(0, nlon, nlat); nT <- matrix(0L, nlon, nlat)
sumP <- matrix(0, nlon, nlat); nP <- matrix(0L, nlon, nlat)

for (yr in NORMAL) {
  for (mo in sprintf("%02d", 1:12)) {
    tf <- file.path(T_SRC, sprintf("era5_2m_temperature_%d_%s.nc", yr, mo))
    pf <- file.path(P_SRC, sprintf("era5_total_precipitation_%d_%s.nc", yr, mo))
    if (file.exists(tf)) {
      a <- read_slab(tf, "t_mean")
      sumT <- sumT + rowSums(a, dims = 2, na.rm = TRUE)
      nT   <- nT   + rowSums(!is.na(a), dims = 2)
    }
    if (file.exists(pf)) {
      b <- read_slab(pf, "tp")
      sumP <- sumP + rowSums(b, dims = 2, na.rm = TRUE)
      nP   <- nP   + rowSums(!is.na(b), dims = 2)
    }
  }
  cat("done", yr, "\n"); flush.console()
}

# ---- 3. Climatologies ---------------------------------------
tmean <- sumT / pmax(nT, 1L)                         # C (auto-K fix below)
if (median(tmean, na.rm = TRUE) > 100) tmean <- tmean - 273.15
tmean[nT == 0] <- NA

ppt <- (sumP / pmax(nP, 1L)) * 365.25 * 1000          # m/day -> mm/yr
ppt[nP == 0] <- NA

cat(sprintf("tmean range: %.1f .. %.1f C\n", min(tmean, na.rm=TRUE), max(tmean, na.rm=TRUE)))
cat(sprintf("ppt   range: %.0f .. %.0f mm\n", min(ppt,  na.rm=TRUE), max(ppt,  na.rm=TRUE)))

# ---- 4. Tidy xyz output (lon in -180..180) ------------------
grid <- expand_grid(lat = lat_w, lon = lon_w) %>%     # lat outer, lon inner
  arrange(lat, lon)
# matrices are [lon, lat]; flatten in lon-fastest order to match
out <- tibble(
  lon   = ifelse(rep(lon_w, times = nlat) > 180,
                 rep(lon_w, times = nlat) - 360, rep(lon_w, times = nlat)),
  lat   = rep(lat_w, each = nlon),
  tmean = as.vector(tmean),
  ppt   = as.vector(ppt)
) %>% filter(!is.na(tmean) | !is.na(ppt))

saveRDS(list(data = out, normal = NORMAL,
             bbox = c(LON_MIN, LON_MAX, LAT_MIN, LAT_MAX)), OUT_RDS)
cat("WROTE", OUT_RDS, "rows:", nrow(out), "\n")
