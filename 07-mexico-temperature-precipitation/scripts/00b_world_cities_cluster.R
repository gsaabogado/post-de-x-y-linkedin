# 00b_world_cities_cluster.R ----------------------------------------------------
# Acquisition step 2 of 3, on the same machine as 00a. The 1991-2020 average
# temperature and yearly rainfall of 53 large cities of the world, from the
# same ERA5-Land daily files, each as the mean of the 5 x 5 cells around the
# city centre (which skips sea cells on the coast). The figures place nine of
# them next to Mexico's own values. The output is carried by the repository as
# in/world_cities_pool.rds.
#
# These are model values for a 50 km window, not station records. For most
# cities they are close to the station normal; for some they are not (Lima
# comes out at 243 mm of rain a year against about 6 mm at its airport, which
# is why the figures do not use it).
#
# Set ERA5_T_DIR, ERA5_P_DIR and CLIM_OUT_DIR as in 00a.
# Run: Rscript scripts/00b_world_cities_cluster.R

rm(list=ls()); gc(); set.seed(42)
library(tidyverse); library(RNetCDF)

NORMAL <- 1991:2020
need_env <- function(v) {
  x <- Sys.getenv(v)
  if (!nzchar(x)) stop("set ", v, " in the environment")
  x
}
T_SRC <- need_env("ERA5_T_DIR")
P_SRC <- need_env("ERA5_P_DIR")
OUT   <- file.path(need_env("CLIM_OUT_DIR"), "world_cities_pool.rds")

# candidate pool spanning the climate square (hot/cold x dry/wet,
# plus mild-moderate for edges and center)
cities <- tibble::tribble(
  ~name,             ~lat,     ~lon,
  # hot-wet corner
  "Singapore",        1.35,   103.82,
  "Bangkok",         13.76,   100.50,
  "Jakarta",         -6.21,   106.85,
  "Mumbai",          19.08,    72.88,
  "Manila",          14.60,   120.98,
  "Miami",           25.76,   -80.19,
  # hot-dry corner
  "Cairo",           30.04,    31.24,
  "Riyadh",          24.71,    46.68,
  "Dubai",           25.20,    55.27,
  "Phoenix",         33.45,  -112.07,
  "Karachi",         24.86,    67.01,
  # cold-dry corner
  "Moscow",          55.76,    37.62,
  "Denver",          39.74,  -104.99,
  "Calgary",         51.05,  -114.07,
  "Beijing",         39.90,   116.41,
  "Almaty",          43.24,    76.89,
  "Ulaanbaatar",     47.89,   106.91,
  # cold-wet corner
  "London",          51.51,    -0.13,
  "Vancouver",       49.28,  -123.12,
  "Seattle",         47.61,  -122.33,
  "Oslo",            59.91,    10.75,
  "Dublin",          53.35,    -6.26,
  "Amsterdam",       52.37,     4.90,
  # hot edge (hottest, moderate precip)
  "New Delhi",       28.61,    77.21,
  "Dakar",           14.72,   -17.47,
  "Chennai",         13.08,    80.27,
  # cold edge (coldest, moderate precip)
  "Toronto",         43.65,   -79.38,
  "Stockholm",       59.33,    18.07,
  "Zurich",          47.38,     8.54,
  "Chicago",         41.88,   -87.63,
  # dry edge (mild temp, driest)
  "Lima",           -12.05,   -77.04,
  "Santiago",       -33.45,   -70.67,
  "Madrid",          40.42,    -3.70,
  "Tehran",          35.69,    51.39,
  # wet edge (mild temp, wettest)
  "Hong Kong",       22.32,   114.17,
  "Taipei",          25.03,   121.57,
  "Auckland",       -36.85,   174.76,
  "Tokyo",           35.68,   139.65,
  # center (mild temp, moderate precip)
  "Sydney",         -33.87,   151.21,
  "Buenos Aires",   -34.60,   -58.38,
  "Nairobi",         -1.29,    36.82,
  "Johannesburg",   -26.20,    28.05,
  "Sao Paulo",      -23.55,   -46.63,
  "Rio de Janeiro", -22.91,   -43.17,
  "Rome",            41.90,    12.50,
  "Istanbul",        41.01,    28.98,
  "Houston",         29.76,   -95.37,
  "Brisbane",       -27.47,   153.03,
  "Los Angeles",     34.05,  -118.24,
  "Paris",           48.86,     2.35,
  "Barcelona",       41.39,     2.17,
  "Cape Town",      -33.92,    18.42,
  "Atlanta",         33.75,   -84.39)
cities$lon360 <- ifelse(cities$lon < 0, cities$lon + 360, cities$lon)

# axes from first file
nc <- open.nc(file.path(T_SRC,"era5_2m_temperature_1991_01.nc"))
lon_axis <- as.numeric(var.get.nc(nc,"longitude"))
lat_axis <- as.numeric(var.get.nc(nc,"latitude"))
close.nc(nc)
R <- 2  # half-window (5x5) to dodge ocean NaN at coasts
NLON <- length(lon_axis); NLAT <- length(lat_axis)
cities$ilon <- sapply(cities$lon360, function(v) which.min(abs(lon_axis - v)))
cities$ilat <- sapply(cities$lat,    function(v) which.min(abs(lat_axis - v)))
# keep the 5x5 window inside the grid (edge/seam cities like London)
cities$ilon <- pmin(pmax(cities$ilon, 1L+R), NLON-R)
cities$ilat <- pmin(pmax(cities$ilat, 1L+R), NLAT-R)
read_win <- function(path, var, ilon, ilat){
  nc <- open.nc(path); on.exit(close.nc(nc))
  a <- var.get.nc(nc, var, start=c(ilon-R, ilat-R, 1),
                  count=c(2*R+1, 2*R+1, NA), collapse=FALSE, unpack=TRUE)
  mean(a, na.rm=TRUE)              # mean over window & days (per-file)
}

n <- nrow(cities)
sumT<-rep(0,n); nT<-rep(0L,n); sumP<-rep(0,n); nP<-rep(0L,n)
for (yr in NORMAL) for (mo in sprintf("%02d",1:12)) {
  tf <- file.path(T_SRC, sprintf("era5_2m_temperature_%d_%s.nc",yr,mo))
  pf <- file.path(P_SRC, sprintf("era5_total_precipitation_%d_%s.nc",yr,mo))
  if (file.exists(tf)) for (i in 1:n){ v<-read_win(tf,"t_mean",cities$ilon[i],cities$ilat[i])
    if(is.finite(v)){sumT[i]<-sumT[i]+v; nT[i]<-nT[i]+1L} }
  if (file.exists(pf)) for (i in 1:n){ v<-read_win(pf,"tp",cities$ilon[i],cities$ilat[i])
    if(is.finite(v)){sumP[i]<-sumP[i]+v; nP[i]<-nP[i]+1L} }
  cat("done",yr,mo,"\n"); flush.console()
}
cities$tmean <- sumT/pmax(nT,1)
if (median(cities$tmean,na.rm=TRUE) > 100) cities$tmean <- cities$tmean - 273.15
cities$ppt <- (sumP/pmax(nP,1)) * 365.25 * 1000   # mean daily m -> annual mm
print(cities[,c("name","tmean","ppt")])
saveRDS(cities, OUT); cat("WROTE",OUT,"\n")
