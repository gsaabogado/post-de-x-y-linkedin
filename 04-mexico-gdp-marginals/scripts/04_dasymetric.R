# 04_dasymetric.R -------------------------------------------------------------
# Push the 0.25 degree GDP grid down onto a 1 km grid, using population as the
# weight. Dasymetric disaggregation.
#
# The problem it solves: the GDP series is 25 km, so the map is either a mosaic
# of visible tiles or, if smoothed, mush. Neither is what a 1 km population map
# looks like. No kernel can fix this, because the detail simply is not in the
# GDP data.
#
# The detail IS in the population data, though, and it is the right auxiliary
# variable: this series is built by allocating output to people in the first
# place. So for every 1 km cell i inside a 0.25 degree cell C,
#
#     gdp_1km(i) = gdp(C) * pop_1km(i) / sum_{j in C} pop_1km(j)
#
# Every C keeps its GDP exactly. Nothing is invented BETWEEN 25 km cells, where
# the source data actually speaks. What is assumed is only that WITHIN a 25 km
# cell, output sits where the people are, which is the same assumption the
# source makes at its own scale. The assumption is stated on the figure.
#
# Note this buys nothing for GDP PER HEAD: dividing the result by pop_1km(i)
# returns gdp(C) / pop(C), constant across the cell. Dasymetric detail exists
# for the extensive quantity only, which is why post 03 smooths instead.
#
# Run from the project root: Rscript scripts/04_dasymetric.R

library(dplyr)
library(terra)
library(sf)

POP_1KM <- "../02-mexico-population-latitude/data/mex_pop_2025_CN_1km_R2025A_UA_v1.tif"

grid_map <- readRDS("data/derived/grid_map.rds")
meta <- readRDS("data/derived/meta.rds")
mex_poly <- readRDS("data/derived/mex_poly.rds")

g <- filter(grid_map, x >= meta$x_min_display)
box <- ext(meta$x_min_display, meta$ext[["xmax"]],
           meta$ext[["ymin"]], meta$ext[["ymax"]])

# --- the two inputs ---------------------------------------------------------
pop <- crop(rast(POP_1KM), box)
pop[is.na(pop)] <- 0
names(pop) <- "pop"

gdp25 <- rast(g[, c("x", "y", "gdp")], type = "xyz", crs = "EPSG:4326")
gdp25[is.na(gdp25)] <- 0
gdp25 <- crop(gdp25, box)

# --- the weights ------------------------------------------------------------
# resample() handles the fact that the two grids do not share an origin, so no
# assumption about alignment is needed anywhere here.
pop_C <- resample(pop, gdp25, method = "sum")      # people per 0.25 degree cell
denom <- resample(pop_C, pop, method = "near")     # that total, back on 1 km
numer <- resample(gdp25, pop, method = "near")     # the cell's GDP, on 1 km

# A cell holding no people cannot receive its GDP by this rule. There are a few,
# and they hold a trivial amount; the check below says how much is lost.
share <- ifel(denom > 0, pop / denom, NA)
gdp1km <- numer * share

# --- mass check -------------------------------------------------------------
# Dasymetric disaggregation is mass preserving by construction. Anything the
# check finds is GDP in cells that hold no population, plus edge effects from
# the two grids not sharing an origin.
tot_src <- sum(g$gdp)
tot_out <- global(gdp1km, "sum", na.rm = TRUE)[1, 1]
message(sprintf("source $%.1f bn -> disaggregated $%.1f bn  (%+.3f%%)",
                tot_src / 1e9, tot_out / 1e9, 100 * (tot_out / tot_src - 1)))
if (abs(tot_out / tot_src - 1) > 0.01) stop("dasymetric step lost more than 1% of GDP")

gdp1km <- mask(gdp1km, vect(st_as_sf(mex_poly)))
writeRaster(gdp1km, "data/derived/mex_gdp_1km.tif", overwrite = TRUE)

v <- values(gdp1km, mat = FALSE)
v <- v[!is.na(v) & v > 0]
message(sprintf("%s populated 1 km cells; GDP per cell p50 $%s, p99 $%s, max $%s",
                format(length(v), big.mark = ","),
                format(round(quantile(v, .50)), big.mark = ","),
                format(round(quantile(v, .99)), big.mark = ","),
                format(round(max(v)), big.mark = ",")))
message("wrote data/derived/mex_gdp_1km.tif")
message("DONE!")
