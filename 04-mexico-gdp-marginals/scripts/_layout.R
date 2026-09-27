# _layout.R -------------------------------------------------------------------
# Shared canvas geometry and palette.
#
# This post reads differently from the population one. GDP per head is a RATIO,
# so there is nothing to stack into a queue. Instead each cell keeps its
# latitude and slides sideways to its own income, and the map becomes a scatter
# of income against latitude. The canvas is therefore map on the left, income
# axis on the right, both spanning the same latitudes.

FONT <- "Avenir Next"

PAL <- list(
  bg       = "#080A10",
  # Diverging around the national average. Cool blue below it, warm gold above.
  ramp     = c("#0E2A4A", "#1A5480", "#3391A8", "#8FC9BE", "#EFE3B4",
               "#F2A93B", "#E2621F", "#B8330C"),
  profile  = "#FFFFFF",
  accent   = "#FF6B4A",
  tropic   = "#7FD4C1",
  border   = "#5C6780",
  city     = "#E8EDF7",
  grid     = "#2A3145",
  grid_txt = "#6B7690",
  title    = "#FFFFFF",
  sub      = "#A9B4C9"
)

# Sequential ramp, dark for poor and bright for rich, matching the population
# post's language. Used on a log scale by the surface map, the scatter and the
# video, so a colour means the same income everywhere in the project.
RAMP_SEQ <- c("#0B1026", "#1B1F4B", "#3A2A6E", "#6B2A8F", "#A8327E",
              "#D84C4C", "#EE7B3A", "#F8B13F", "#FFE27A", "#FFF8E0")
LIM_SEQ <- log10(c(6000, 48000))
# Colour for an EXTENSIVE quantity: log10 of dollars per degree, spanning a
# billion to nearly two trillion, which is the range these histograms cover.
LIM_BN <- log10(c(1e9, 1.9e12))

# Per-degree value of the band a coordinate falls in, for parking a city label
# at the tip of its own bar.
band_val <- function(prof, v, bin) {
  prof$per_degree[match(round(floor(v / bin) * bin, 4), round(prof$band, 4))]
}
lg <- function(v) log10(pmax(v, 1))

# Geometry for the marginal plate: a map region with a latitude histogram down
# its right and a longitude histogram along its bottom. `peak` is the largest
# per-degree value on either axis, so both share one scale; s_y divides by the
# ratio, which is what makes an equal visual length mean equal money on either
# axis despite coord_fixed stretching y.
marginal_layout <- function(meta, peak, lat_w = 9.0, gap = 0.9) {
  e <- meta$ext
  map <- list(xmin = meta$x_min_display, xmax = e[["xmax"]],
              ymin = e[["ymin"]], ymax = e[["ymax"]])
  ratio <- 1 / cos(mean(c(map$ymin, map$ymax)) * pi / 180)
  s_x <- lat_w / peak
  s_y <- s_x / ratio
  lon_h <- peak * s_y
  lat_base <- map$xmax + gap
  lon_base <- map$ymin - gap - lon_h
  list(map = map, ratio = ratio, gap = gap, lat_w = lat_w, lon_h = lon_h,
       s_x = s_x, s_y = s_y, lat_base = lat_base, lon_base = lon_base,
       atx = function(v) lat_base + v * s_x,
       aty = function(v) lon_base + v * s_y,
       canvas = list(xmin = map$xmin - 3.4, xmax = lat_base + lat_w + 3.4,
                     ymin = lon_base - 6.6, ymax = map$ymax + 3.2))
}

# layout_specs() returns every coordinate the figures need.
#
# `s_pc` converts dollars of GDP per head into degrees of longitude, so a
# horizontal distance in the right-hand panel reads as an income.
layout_specs <- function(meta, scat_width = 17, gap = 0.9) {
  e <- meta$ext
  # Start at the mainland: see meta$x_min_display, which cuts Isla Guadalupe and
  # the four empty longitude bands of open Pacific behind it.
  map <- list(xmin = if (is.null(meta$x_min_display)) e[["xmin"]] else meta$x_min_display,
              xmax = e[["xmax"]], ymin = e[["ymin"]], ymax = e[["ymax"]])

  # 1 degree of latitude is longer on the ground than 1 degree of longitude;
  # stretch y by this factor so Mexico keeps its shape.
  ratio <- 1 / cos(mean(c(map$ymin, map$ymax)) * pi / 180)

  scat_base <- map$xmax + gap
  s_pc <- scat_width / meta$pc_axis_max

  list(
    map = map,
    ratio = ratio,
    gap = gap,
    scat_base = scat_base,
    scat_width = scat_width,
    s_pc = s_pc,
    # dollars -> canvas x
    at = function(v) scat_base + pmin(v, meta$pc_axis_max) * s_pc,
    stat_y = map$ymin - 2.1,        # row of headline numbers, under the map
    canvas = list(xmin = map$xmin - gap, xmax = scat_base + scat_width + 1.4,
                  ymin = map$ymin - 8.6, ymax = map$ymax + 2.8)
  )
}
