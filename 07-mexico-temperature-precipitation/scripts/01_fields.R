# 01_fields.R -----------------------------------------------------------------
# The one heavy step of the post (about a minute): relief shading and the two
# climate fields on ONE grid, saved apart so the three figures only colour and
# compose. The first version of this post (July 2026, scripts kept out of the
# public repository) baked colour and relief into one array, so every palette
# change cost a full shading pass.
#
# Same steps as that version's code/03_build_relief.R, with the relief
# exaggeration fixed at the approved value (ZSCALE = 16):
#   1. ERA5-Land climatology 1991-2020 (0.1 degrees) -> rasters
#   2. AWS terrain (despiked), projected to a Lambert conformal conic for Mexico
#   3. climate grown offshore and resampled onto the terrain grid, so the coast
#      follows the national polygon and not the 11 km climate cells
#   4. relief as two blended sun angles, stretched to 0-1
#
# Also saved: the share of the territory below each temperature and rainfall
# value (true cell areas, not cell counts), which the bivariate map and its
# world-city legend both read, and the country outline.
#
# Checked on 2026-10-11: colouring these fields as code/03 does gives exactly the
# array that code/03 itself writes today (largest difference 0). Neither matches
# the stored July array in/colored_2d.rds (differences up to 0.12): rayshader
# was updated on 2026-07-20 and shades flat ground darker than it did. So
# in/colored_2d.rds is a frozen input of the July figure and cannot be rebuilt;
# the v2 figures use the relief below and lighten or darken around flat ground
# (`gray_flat`), so flat land shows the legend's own colour.
#
# Run from the post root: Rscript scripts/01_fields.R
# INPUT:  in/mx_climatology_1991_2020.rds (00a), in/dem/mex_dem_aws.tif and
#         in/gadm/gadm41_MEX_0_pk.rds (00c)
# OUTPUT: data/derived/fields.rds

library(terra)
library(sf)
library(rayshader)

set.seed(42)
options(rgl.useNULL = TRUE)

say <- function(...) { message(format(Sys.time(), "%H:%M:%S "), ...); flush.console() }

LCC <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs"
ZSCALE <- 16                      # relief exaggeration approved in July; lower = more drama
AREA_MX <- 1964375                # km2, INEGI continental + insular; a sanity check only

# --- 1. climatology ---------------------------------------------------------
cl <- readRDS("in/mx_climatology_1991_2020.rds")
stopifnot(identical(range(cl$normal), c(1991L, 2020L)), !anyNA(cl$data$tmean), !anyNA(cl$data$ppt))
xyz <- as.data.frame(cl$data)   # rast() takes a plain data frame, not a tibble
clim <- c(rast(xyz[, c("lon", "lat", "tmean")], type = "xyz", crs = "EPSG:4326"),
          rast(xyz[, c("lon", "lat", "ppt")], type = "xyz", crs = "EPSG:4326"))
names(clim) <- c("tmean", "ppt")

# --- 2. boundary and terrain -------------------------------------------------
mxv <- terra::unwrap(readRDS("in/gadm/gadm41_MEX_0_pk.rds"))
dem <- rast("in/dem/mex_dem_aws.tif")
med <- focal(dem, w = 5, fun = median, na.policy = "omit", na.rm = TRUE)
spk <- abs(dem - med) > 400       # more than 400 m from the local median = a spike
dem <- cover(mask(dem, spk, maskvalue = TRUE), med)
dem <- mask(crop(dem, mxv), mxv)
say("terrain despiked")

# --- 3. one grid --------------------------------------------------------------
grow <- function(r, n) {
  for (i in seq_len(n)) r <- focal(r, w = 5, fun = mean, na.policy = "only", na.rm = TRUE)
  r
}
clim <- grow(clim, 4)             # about 44 km offshore
mx_p <- project(mxv, LCC)
dem_p <- mask(project(dem, LCC), mx_p)
clim_p <- mask(resample(project(clim, LCC), dem_p, method = "bilinear"), mx_p)
say("projected, grid ", ncol(dem_p), " x ", nrow(dem_p))

# --- 4. relief ----------------------------------------------------------------
# raster_to_matrix() returns [x, y]; sphere_shade() returns [y, x]. Everything
# saved below is [y, x] with north in row 1, the orientation of the picture.
elmat <- raster_to_matrix(dem_p)
g1 <- sphere_shade(elmat, texture = "bw", sunangle = 315, zscale = ZSCALE)[, , 1]
g2 <- sphere_shade(elmat, texture = "bw", sunangle = 270, zscale = ZSCALE)[, , 1]
gray <- 0.78 * g1 + 0.22 * g2
lo <- quantile(gray, .02, na.rm = TRUE)
hi <- quantile(gray, .99, na.rm = TRUE)
gray <- pmin(pmax((gray - lo) / (hi - lo), 0), 1)
gray[is.na(gray)] <- 1
# The grey of flat ground on the same stretch: the figures shade around it.
flat <- matrix(0, 21, 21)
gray_flat <- unname(
  (0.78 * sphere_shade(flat, texture = "bw", sunangle = 315, zscale = ZSCALE)[11, 11, 1] +
     0.22 * sphere_shade(flat, texture = "bw", sunangle = 270, zscale = ZSCALE)[11, 11, 1] - lo) / (hi - lo))
stopifnot(gray_flat > 0.05, gray_flat < 0.95)
rm(g1, g2, elmat, flat); gc()
say(sprintf("relief shaded, flat ground at %.3f", gray_flat))

tmean <- t(raster_to_matrix(clim_p$tmean))
ppt <- t(raster_to_matrix(clim_p$ppt))
elev <- t(raster_to_matrix(dem_p))   # metres, for the numbers quoted in the texts
land <- !is.na(tmean) & !is.na(ppt)
stopifnot(identical(dim(gray), dim(tmean)), identical(dim(gray), dim(ppt)))

# --- 5. share of the territory below each value (true cell areas) ---------------
area <- t(raster_to_matrix(cellSize(dem_p, unit = "km")))
a_land <- sum(area[land])
say(sprintf("land area on the grid: %.0f km2 (INEGI %.0f)", a_land, AREA_MX))
stopifnot(abs(a_land / AREA_MX - 1) < 0.02)

PROBS <- seq(0, 1, length.out = 2001)
wquant <- function(v, w) {
  o <- order(v)
  cw <- cumsum(w[o]) / sum(w)
  approx(cw, v[o], xout = PROBS, rule = 2, ties = "ordered")$y
}
q_t <- wquant(tmean[land], area[land])
q_p <- wquant(ppt[land], area[land])
nat <- c(tmean = weighted.mean(tmean[land], area[land]),
         ppt = weighted.mean(ppt[land], area[land]))
say(sprintf("national mean: %.2f C, %.0f mm a year", nat["tmean"], nat["ppt"]))

outline <- st_simplify(st_as_sf(mx_p), dTolerance = 1500)

fields <- list(gray = gray, gray_flat = gray_flat, tmean = tmean, ppt = ppt, elev = elev, area = area, ext = as.vector(ext(dem_p)),
               probs = PROBS, q_t = q_t, q_p = q_p, nat = nat, area_km2 = a_land,
               outline = outline, crs = LCC, zscale = ZSCALE)
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)
saveRDS(fields, "data/derived/fields.rds")
say("wrote data/derived/fields.rds")

say("DONE")
