# _map.R ----------------------------------------------------------------------
# Shared by the three figures of the post (02 temperature, 03 rainfall, 04 both):
# the fields from 01, the colours, the relief blend, the cities and the frame.
#
# One colour system for the whole thread, built from the series roles in
# ../_style/style.R: temperature runs from a pale neutral to the brick red,
# rainfall from the same pale neutral to the slate blue, and the third map
# mixes the two, so a colour means the same thing in every tweet.
#
# Coordinates are metres in the Lambert projection of 01 (FLD$crs), so
# coord_fixed() needs no ratio and a halo offset is the same in x and y.

library(ggplot2)
library(dplyr)
library(sf)

source("../_style/style.R")

dir.create("images", showWarnings = FALSE)
FLD <- readRDS("data/derived/fields.rds")
LAND <- !is.na(FLD$tmean) & !is.na(FLD$ppt)
EXT <- FLD$ext                               # xmin xmax ymin ymax
MAP_W <- EXT[2] - EXT[1]
MAP_H <- EXT[4] - EXT[3]
KM <- 1000

# Relief strength. Flat ground keeps the legend's exact colour; slopes turned
# away from the sun darken by up to SHADE_DARK and slopes facing it lighten
# toward the page by up to SHADE_LIGHT.
SHADE_DARK <- 0.50
SHADE_LIGHT <- 0.45

# --- colours ---------------------------------------------------------------------
lab_mix <- function(a, b, w = 0.5) {
  grDevices::colorRampPalette(c(a, b), space = "Lab")(201)[1 + round(200 * w)]
}
# The four corners of the two-variable square. `pale` must differ from the
# page, or cold dry land would vanish into it.
climate_pal <- function(p) {
  list(pale = lab_mix(p$bg, p$div[4], 0.55),         # cold and dry
       hot = p$accent,                               # hot and dry: the brick red
       wet = lab_mix(p$div[3], p$div[2], 0.55),      # cold and wet: a light slate blue
       both = p$primary)                             # hot and wet: the dark slate blue
}
to_rgb <- function(hex) grDevices::col2rgb(hex)[, 1] / 255

# Share of the territory below a value, from the area-weighted tables of 01.
share_below <- function(v, q) approx(q, FLD$probs, xout = v, rule = 2, ties = "ordered")$y
value_at_share <- function(s, q) approx(FLD$probs, q, xout = s, rule = 2)$y

# One-variable colouring: t01 in [0, 1] per land cell -> three channels.
ramp_channels <- function(t01, colours) {
  lut <- grDevices::col2rgb(grDevices::colorRampPalette(colours, space = "Lab")(1024)) / 255
  idx <- 1 + round(pmin(pmax(t01, 0), 1) * 1023)
  list(lut[1, idx], lut[2, idx], lut[3, idx])
}
# Two-variable colouring: bilinear mix of the four corners.
square_channels <- function(tx, py, cp) {
  c00 <- to_rgb(cp$pale); c10 <- to_rgb(cp$hot); c01 <- to_rgb(cp$wet); c11 <- to_rgb(cp$both)
  lapply(1:3, function(k) c00[k] * (1 - tx) * (1 - py) + c10[k] * tx * (1 - py) +
           c01[k] * (1 - tx) * py + c11[k] * tx * py)
}
# Colour shaded by the relief, transparent outside the country.
relief_image <- function(channels) {
  g <- FLD$gray[LAND]
  g0 <- as.numeric(FLD$gray_flat)
  dark <- 1 - SHADE_DARK * pmax(g0 - g, 0) / g0
  lite <- SHADE_LIGHT * pmax(g - g0, 0) / (1 - g0)
  a4 <- array(0, dim = c(dim(LAND), 4))
  for (k in 1:3) {
    v <- channels[[k]] * dark
    ch <- matrix(0, nrow(LAND), ncol(LAND))
    ch[LAND] <- v + (1 - v) * lite
    a4[, , k] <- ch
  }
  a4[, , 4] <- LAND
  grDevices::as.raster(a4)
}

# --- cities ----------------------------------------------------------------------
# City-centre points, used to place a marker and to read the map's own value
# there. dx, dy (km) and hj place the label next to the dot.
CITIES <- tibble::tribble(
  ~es,                ~en,           ~lon,      ~lat,     ~dx,  ~dy, ~hj,
  "Tijuana",          "Tijuana",     -117.0382, 32.5149,   45,  -55, 0,
  "Hermosillo",       "Hermosillo",  -110.9559, 29.0729,    0,   38, 0.5,
  "Chihuahua",        "Chihuahua",   -106.0889, 28.6353,    0,   38, 0.5,
  "Monterrey",        "Monterrey",   -100.3161, 25.6866,    0,   38, 0.5,
  "Guadalajara",      "Guadalajara", -103.3496, 20.6597,    0,   38, 0.5,
  "Ciudad de México", "Mexico City",  -99.1332, 19.4326,    0,   38, 0.5,
  "Villahermosa",     "Villahermosa", -92.9475, 17.9892,    0,   38, 0.5,
  "Mérida",           "Mérida",       -89.5926, 20.9674,    0,   38, 0.5)
cities_xy <- function() {
  pts <- st_as_sf(CITIES, coords = c("lon", "lat"), crs = 4326) |> st_transform(FLD$crs)
  xy <- st_coordinates(pts)
  out <- mutate(CITIES, X = xy[, 1], Y = xy[, 2], name = es_en(es, en),
                col = 1 + floor((X - EXT[1]) / MAP_W * ncol(LAND)),
                row = 1 + floor((EXT[4] - Y) / MAP_H * nrow(LAND)))
  out$tmean <- FLD$tmean[cbind(out$row, out$col)]
  out$ppt <- FLD$ppt[cbind(out$row, out$col)]
  stopifnot(!anyNA(out$tmean), !anyNA(out$ppt))   # every city falls on land
  out
}
# A label (column `code`) offset dx, dy km from its point, with a page-coloured
# halo so the text survives dark land.
city_text <- function(cities, p, size = 2.9, halo = 7) {
  cities <- mutate(cities, lx = X + dx * KM, ly = Y + dy * KM)
  off <- expand.grid(ox = c(-1, 0, 1), oy = c(-1, 0, 1))
  off <- off[!(off$ox == 0 & off$oy == 0), ]
  ring <- bind_rows(lapply(seq_len(nrow(off)), function(k)
    mutate(cities, lx = lx + off$ox[k] * halo * KM, ly = ly + off$oy[k] * halo * KM)))
  list(
    geom_text(data = ring, aes(lx, ly, label = code, hjust = hj), colour = p$bg,
              family = STYLE_FONT, size = size, vjust = 0),
    geom_text(data = cities, aes(lx, ly, label = code, hjust = hj), colour = p$ink,
              family = STYLE_FONT, size = size, vjust = 0))
}
# Marker and label for the Mexican cities on the map.
city_layers <- function(cities, p, size = 2.9) {
  c(list(
    geom_point(data = cities, aes(X, Y), shape = 16, size = 2.8, colour = p$bg),
    geom_point(data = cities, aes(X, Y), shape = 21, size = 1.6, stroke = 0.55,
               colour = p$ink, fill = NA)),
    city_text(cities, p, size = size))
}

# The nine world cities, with their own 1991-2020 averages in the same data
# (in/world_cities_pool.rds, CMCC job 702777). Eight are the ones approved in
# July. Madrid replaced Lima on 2026-10-11 (Luis): the data give Lima 243 mm of
# rain a year against a station normal of 6.4 mm, about 40 times too wet.
# Madrid is 14.3 C and 466 mm here against 15.4 C and 416 mm at Retiro (AEMET,
# 1991-2020). The other eight have NOT been checked against station normals. `side` is where
# the label sits next to its dot in the square of 04 (r right, l left, b below,
# a above).
WORLD <- tibble::tribble(
  ~pool,         ~es,           ~en,           ~side,
  "Los Angeles", "Los Ángeles", "Los Angeles", "r",
  "Paris",       "París",       "Paris",       "r",
  "Tokyo",       "Tokio",       "Tokyo",       "r",
  "Madrid",      "Madrid",      "Madrid",      "r",
  "Brisbane",    "Brisbane",    "Brisbane",    "b",
  "Sao Paulo",   "São Paulo",   "São Paulo",   "b",
  "Phoenix",     "Phoenix",     "Phoenix",     "l",
  "Miami",       "Miami",       "Miami",       "l",
  "Hong Kong",   "Hong Kong",   "Hong Kong",   "r")
world_cities <- function() {
  pool <- readRDS("in/world_cities_pool.rds")
  out <- WORLD |>
    left_join(pool[, c("name", "tmean", "ppt")], by = c("pool" = "name")) |>
    mutate(code = es_en(es, en))
  stopifnot(!anyNA(out$tmean), !anyNA(out$ppt))
  out
}

# --- frame -------------------------------------------------------------------------
# Canvas 12 x 8 in (3:2), as post 01: on 16:9 the title and the numbered notes
# leave a panel 4 in tall and the country comes out 6 in wide on a 12 in image.
# The map sits at the left; the Gulf and the space east of it hold the legend.
# PANEL is the panel's width over its height once the title and the notes are
# in place, measured on a render.
FIG_W <- 12
FIG_H <- 8
PANEL <- 2.15
YLIM <- c(EXT[3] - 0.035 * MAP_H, EXT[4] + 0.035 * MAP_H)
XLIM <- c(EXT[1] - 50 * KM, EXT[1] - 50 * KM + PANEL * diff(YLIM))

outline_df <- local({
  xy <- as.data.frame(st_coordinates(st_cast(st_geometry(FLD$outline), "MULTILINESTRING")))
  mutate(xy, grp = interaction(L1, L2, drop = TRUE))
})

base_map <- function(img, p) {
  ggplot() +
    annotation_raster(img, xmin = EXT[1], xmax = EXT[2], ymin = EXT[3], ymax = EXT[4],
                      interpolate = TRUE) +
    geom_path(data = outline_df, aes(X, Y, group = grp), colour = p$axis,
              linewidth = 0.12, alpha = 0.55)
}
map_frame <- function(mode) {
  list(coord_fixed(xlim = XLIM, ylim = YLIM, expand = FALSE),
       theme_ls(mode),
       theme(axis.line = element_blank(), axis.ticks = element_blank(),
             axis.text = element_blank(), axis.title.x = element_blank(),
             axis.title.y = element_blank(), panel.grid.major.y = element_blank(),
             plot.caption = element_text(lineheight = 1.1),
             plot.margin = margin(22, 22, 14, 22)))
}

# The legend column, east of the Yucatán peninsula, shared by the three figures.
LEG <- list(w = 1120 * KM)
LEG$x0 <- XLIM[2] - 110 * KM - LEG$w
LEG$top <- YLIM[2] - 300 * KM
leg_text <- function(x, y, label, ...) annotate("text", x = x, y = y, label = label,
                                                family = STYLE_FONT, ...)
leg_title <- function(p, title, sub, sub_y = 50) {
  list(leg_text(LEG$x0, LEG$top + 190 * KM, title, colour = p$ink, size = 3.3,
                fontface = "bold", hjust = 0, vjust = 0),
       leg_text(LEG$x0, LEG$top + sub_y * KM, sub, colour = p$sub, size = 2.8, hjust = 0,
                vjust = 0, lineheight = 0.95))
}

# Horizontal colour bar for the one-variable maps. `pos` maps a value to [0, 1]
# along the bar, `ticks` are values, `mark` a value to point at from above
# with `mark_lab` (one line), and `facts` lines of plain text under the bar.
# `world` (optional) lists world cities between the bar and the facts: columns
# `code` (name), `value` and `value_lab`, one row per city, each with a chip of
# the colour the city would have on the map; `world_head` is the list's heading.
BAR_H <- 70 * KM
ROW_H <- 62 * KM
bar_layers <- function(p, colours, pos, ticks, tick_lab, title, sub, mark, mark_lab, facts,
                       world = NULL, world_head = NULL) {
  lut <- grDevices::colorRampPalette(colours, space = "Lab")(400)
  strip <- grDevices::as.raster(matrix(lut, nrow = 1))
  bx <- function(v) LEG$x0 + pos(v) * LEG$w
  top <- LEG$top; bot <- LEG$top - BAR_H
  list_layers <- NULL
  facts_y <- bot - 150 * KM
  if (!is.null(world)) {
    world <- world[order(world$value), ]
    world$y <- bot - 235 * KM - (seq_len(nrow(world)) - 1) * ROW_H
    world$chip <- lut[1 + round(pos(world$value) * 399)]
    chip <- 42 * KM
    list_layers <- list(
      leg_text(LEG$x0, bot - 150 * KM, world_head, colour = p$ink, size = 2.8, hjust = 0, vjust = 0.5),
      annotate("rect", xmin = LEG$x0, xmax = LEG$x0 + chip, ymin = world$y - chip / 2,
               ymax = world$y + chip / 2, fill = world$chip, colour = NA),
      leg_text(LEG$x0 + 75 * KM, world$y, world$code, colour = p$sub, size = 2.7, hjust = 0, vjust = 0.5),
      leg_text(LEG$x0 + 520 * KM, world$y, world$value_lab, colour = p$sub, size = 2.7, hjust = 1,
               vjust = 0.5))
    facts_y <- min(world$y) - 105 * KM
  }
  c(leg_title(p, title, sub, sub_y = 118), list_layers, list(
    annotation_raster(strip, xmin = LEG$x0, xmax = LEG$x0 + LEG$w, ymin = bot, ymax = top),
    annotate("segment", x = bx(ticks), xend = bx(ticks), y = bot, yend = bot - 22 * KM,
             colour = p$axis, linewidth = 0.3),
    leg_text(bx(ticks), bot - 38 * KM, tick_lab, colour = p$sub, size = 2.6, vjust = 1),
    annotate("segment", x = bx(mark), xend = bx(mark), y = top + 24 * KM, yend = bot,
             colour = p$ink, linewidth = 0.35),
    leg_text(bx(mark), top + 34 * KM, mark_lab, colour = p$ink, size = 2.7, vjust = 0),
    leg_text(LEG$x0, facts_y, paste(facts, collapse = "\n"), colour = p$sub, size = 2.8,
             hjust = 0, vjust = 1, lineheight = 1.15)))
}

# Notes shared by the three figures, in plain words.
NOTE_GRID <- es_en(
  "El color cambia cada 11 kilómetros, que es el detalle de los datos del clima. El relieve es más fino y solo ayuda a ubicar sierras, valles y costas.",
  "Colour changes every 11 kilometres, the detail of the climate data. The relief is finer and only helps to place mountains, valleys and coasts.")
# Luis's wording (doc, 2026-10-11): without "pueden diferir de ellas".
NOTE_MODEL <- es_en(
  "Los datos combinan mediciones con un modelo del clima (un reanálisis). No son lecturas directas de estaciones.",
  "The data combine measurements with a climate model (a reanalysis). They are not direct station readings.")
SOURCE <- es_en(
  "ERA5-Land, Servicio de Cambio Climático de Copernicus (promedios 1991-2020). Relieve: Terrain Tiles (Mapzen, datos abiertos en AWS). Límite nacional: GADM 4.1.",
  "ERA5-Land, Copernicus Climate Change Service (1991-2020 averages). Relief: Terrain Tiles (Mapzen, AWS open data). National boundary: GADM 4.1.")
