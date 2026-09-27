# 01_build_grids.R ------------------------------------------------------------
# Pull Mexico out of the Rossi-Hansberg gridded GDP series and build the tables
# the figures need. GDP and population come from the SAME grid cells, so any
# comparison between the two is like for like.
#
# Source: Rossi-Hansberg gridded GDP, V2, 0.25 degrees, held in the
# "Pollution and GDP" project. `gdp*` columns are BILLIONS of USD; `id` carries
# an ISO3 prefix, so Mexico selects exactly with no spatial masking.
# Run from the project root: Rscript scripts/01_build_grids.R

library(arrow)
library(dplyr)
library(tidyr)
library(terra)
library(sf)
library(rnaturalearth)

set.seed(20260830)

SRC <- path.expand(Sys.getenv("RH_GDP_FEATHER"))  # cleaned Rossi-Hansberg GDP grid; set in .Renviron
if (!nzchar(SRC)) stop("Set RH_GDP_FEATHER (path to rh_gdp_v2_clean.feather) in .Renviron; see README")
YEAR <- 2021
RES <- 0.25          # the grid's own cell size, kept for the dots and the map
# Profiles bin two cells together. A 0.25 degree band often holds too few people
# for its ratio to mean anything and the average line jumps around. The fitted
# gradient barely notices the choice: +5.70% per degree at 0.25, +5.66% at 0.50,
# +5.81% at 1.00, so this is a readability choice, not a result. One degree also
# averages over the sub-metro noise the downscaling introduces, which is why it
# wins over the finer options.
BIN <- 1.00

# GDP per head is a ratio, so a cell or band holding almost nobody can post an
# absurd value. 849 cells hold under 1,000 people between them, 0.23% of the
# country, and one of them reads $151,780 a head on a population of 2. They stay
# in every total, and are weighted down to nothing in every display.
POP_FLOOR_BAND <- 50000   # a latitude band needs this many people to be drawn
PC_AXIS_MAX <- 70000      # income axis stops here, near the weighted 99th pct

# Dropbox keeps this file online-only. Reading it cold is a serial download at
# read time, so hydrate first and say so.
blocks <- as.numeric(system2("stat", c("-f", "%b", shQuote(SRC)), stdout = TRUE))
if (isTRUE(blocks == 0)) {
  message("hydrating ", basename(SRC), " from Dropbox ...")
  system2("cat", shQuote(SRC), stdout = NULL)
}

raw <- open_dataset(SRC, format = "feather") |>
  filter(substr(id, 1, 3) == "MEX", year == YEAR) |>
  select(id, lat, lon, pop, gdp, gdp_ppp, gdp_pc_ppp) |>
  collect()

# Billions of USD -> USD, so every total below is in plain dollars.
grid_map <- raw |>
  transmute(x = lon, y = lat,
            pop = pop,
            gdp = gdp_ppp * 1e9,
            gdp_pc = gdp_ppp * 1e9 / pmax(pop, 1)) |>
  filter(gdp > 0) |>
  as_tibble()

total_gdp <- sum(grid_map$gdp)
total_pop <- sum(grid_map$pop)

message(sprintf("%s cells, %s, GDP(PPP) %.0f bn USD, population %.1f M, %s per head",
                format(nrow(grid_map), big.mark = ","), YEAR, total_gdp / 1e9,
                total_pop / 1e6,
                format(round(total_gdp / total_pop), big.mark = ",")))

# Sanity gates. These are the published figures for Mexico in 2021; if the
# source file ever changes shape, the run should stop rather than draw a
# plausible-looking wrong picture.
stopifnot(nrow(grid_map) > 3000,
          abs(total_gdp / 1e12 - 2.685) < 0.05,
          abs(total_pop / 1e6 - 129) < 2)

# --- marginal profiles ------------------------------------------------------
# Reported per DEGREE so latitude and longitude sit on the same footing.
bin_floor <- function(v, w) round(floor(v / w) * w, 4)

profile <- function(d, axis) {
  v <- if (axis == "lat") d$y else d$x
  d |>
    mutate(band = bin_floor(v, BIN)) |>
    group_by(band) |>
    summarise(gdp = sum(gdp), pop = sum(pop), .groups = "drop") |>
    complete(band = seq(min(band), max(band), by = BIN),
             fill = list(gdp = 0, pop = 0)) |>
    mutate(mid = band + BIN / 2,
           per_degree = gdp / BIN,
           pop_per_degree = pop / BIN,
           # Population-weighted GDP per head. A band needs real population
           # behind it before its ratio means anything, so thin bands are
           # flagged rather than drawn.
           pc = gdp / pmax(pop, 1),
           solid = pop >= POP_FLOOR_BAND) |>
    arrange(mid)
}

prof_lat <- profile(grid_map, "lat")
prof_lon <- profile(grid_map, "lon")

# The grid's own bands, unaggregated, for figures that want the raw histogram
# rather than the smoothed average line.
BIN_SAVE <- BIN
BIN <- RES
prof_lat_fine <- profile(grid_map, "lat")
prof_lon_fine <- profile(grid_map, "lon")
BIN <- BIN_SAVE

stopifnot(abs(sum(prof_lat$gdp) - total_gdp) < 1,
          abs(sum(prof_lon$gdp) - total_gdp) < 1)

peak_lat <- slice_max(prof_lat, per_degree, n = 1)
peak_lon <- slice_max(prof_lon, per_degree, n = 1)
message(sprintf("peak latitude  %.2f N -> %.0f bn USD per degree",
                peak_lat$mid, peak_lat$per_degree / 1e9))
message(sprintf("bands at %.2f deg, drawn: %d of %d (rest hold under %s people)", BIN,
                sum(prof_lat$solid), nrow(prof_lat), format(POP_FLOOR_BAND, big.mark = ",")))
rich <- prof_lat |> filter(solid) |> slice_max(pc, n = 1)
poor <- prof_lat |> filter(solid) |> slice_min(pc, n = 1)
message(sprintf("richest band %.2fN $%s   poorest band %.2fN $%s   ratio %.2f",
                rich$mid, format(round(rich$pc), big.mark = ","),
                poor$mid, format(round(poor$pc), big.mark = ","), rich$pc / poor$pc))

# --- raster for the map panel -----------------------------------------------
# Built on the grid's own 0.25 degree cells. Nothing is interpolated, because
# 25 km is genuinely the resolution of this series.
r <- rast(grid_map |> select(x, y, gdp_pc), type = "xyz", crs = "EPSG:4326")
ext_v <- as.vector(ext(r))
writeRaster(r, "data/derived/mex_gdp_pc_2021.tif", overwrite = TRUE)

# --- national outline -------------------------------------------------------
# Kept so the figures can crop the 0.25 degree grid to Mexico's actual border
# instead of leaving a staircase of 25 km steps along every coast.
sf_use_s2(FALSE)
mex_poly <- ne_countries(country = "Mexico", scale = "large", returnclass = "sf") |>
  st_geometry()
saveRDS(mex_poly, "data/derived/mex_poly.rds")

# Isla Guadalupe sits 240 km off Baja at 118.4W with 38 people between two
# cells. It leaves four empty longitude bands of open Pacific between itself and
# Tijuana, so the figures start their x range at the mainland instead. The two
# cells stay in every total: they are 0.00003% of the population and 0.00004% of
# GDP.
X_MIN_DISPLAY <- -117.25

meta <- list(
  x_min_display = X_MIN_DISPLAY,
  source = SRC,
  year = YEAR,
  bin = BIN,
  res = RES,
  raster = "data/derived/mex_gdp_pc_2021.tif",
  pop_floor_band = POP_FLOOR_BAND,
  pc_axis_max = PC_AXIS_MAX,
  gdp_pc_national = total_gdp / total_pop,
  total_gdp = total_gdp,
  total_pop = total_pop,
  ext = ext_v,
  peak_lat = peak_lat,
  peak_lon = peak_lon
)

saveRDS(grid_map, "data/derived/grid_map.rds")
saveRDS(prof_lat, "data/derived/prof_lat.rds")
saveRDS(prof_lon, "data/derived/prof_lon.rds")
saveRDS(prof_lat_fine, "data/derived/prof_lat_fine.rds")
saveRDS(prof_lon_fine, "data/derived/prof_lon_fine.rds")
saveRDS(meta, "data/derived/meta.rds")

message("DONE!")
