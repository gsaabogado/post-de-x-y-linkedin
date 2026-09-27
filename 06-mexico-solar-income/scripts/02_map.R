# 02_map.R --------------------------------------------------------------------
# Figure F1: where Mexican homes have solar panels, 2025, in the series style
# (../_style/style.R), light and dark.
#
# Two layers of information in one image:
#   colour     = the municipal rate against the national one (1.53%), on a
#                log2 scale, slate blue below and brick red above. A diverging
#                scale reads the same on white and on dark; a light-to-dark
#                ramp on white would make "low rate" and "few homes" both pale.
#   brightness = how many homes there are, blended toward the background.
#                Without it the map is dominated by the Sierra Tarahumara,
#                where a handful of homes in a huge municipality report rates
#                of 20-40%, and the eye reads empty mountains as the story.
#
# Smoothing: numerator and denominator are blurred SEPARATELY by the same
# separable Gaussian and divided at the end. Blurring the rate itself would
# average percentages and let an empty cell pull its neighbours around.
#
# Run from the post root: Rscript scripts/02_map.R

library(dplyr)
library(terra)
library(sf)
library(ggplot2)

source("../_style/style.R")
source("scripts/_places.R")

SIGMA <- 0.06                     # degrees, about 6.5 km
SPAN <- 2                         # colour limits: national rate x 2^-SPAN .. x 2^SPAN
DENS <- log10(c(2, 1500))         # dwellings per 1 km cell, faded -> full strength
B_MIN <- 0.04                     # strength floor; state lines carry the shape

r <- rast("data/derived/dwellings_panel_1km.tif")
country <- readRDS("data/derived/mex_poly.rds")
state_lines <- readRDS("data/derived/state_lines.rds")
loc <- readRDS("data/derived/localities_50k.rds")
states <- readRDS("data/derived/states_2025.rds")
nat <- readRDS("data/derived/national_series.rds")

# --- smooth, divide, mask ----------------------------------------------------
gauss_1d <- function(sigma, res) {
  k <- seq(-ceiling(3 * sigma / res), ceiling(3 * sigma / res))
  w <- exp(-0.5 * (k * res / sigma)^2)
  w / sum(w)
}
# The grid's northern edge runs through Tijuana and Mexicali, so it is padded
# with zeros first; otherwise the kernel spills their homes off the raster.
blur <- function(x) {
  k <- gauss_1d(SIGMA, res(x)[1])
  x <- extend(x, length(k), fill = 0)
  x <- focal(x, matrix(k, nrow = 1), fun = "sum", na.rm = TRUE)
  focal(x, matrix(k, ncol = 1), fun = "sum", na.rm = TRUE)
}
den_s <- blur(r$den)
num_s <- blur(r$num)

# Blurring moves homes around; it must not create or destroy them.
mass <- c(global(den_s, "sum", na.rm = TRUE)[1, 1] / global(r$den, "sum")[1, 1],
          global(num_s, "sum", na.rm = TRUE)[1, 1] / global(r$num, "sum")[1, 1])
message(sprintf("mass after blur: dwellings %.4f, with panel %.4f", mass[1], mass[2]))
stopifnot(all(abs(mass - 1) < 1e-4))

msk <- vect(st_as_sf(country))
rate <- mask(100 * num_s / ifel(den_s < 1e-3, NA, den_s), msk)
den_m <- mask(den_s, msk)

nat25 <- nat$pct[nat$year == 2025]
to_mat <- function(x) matrix(values(x, mat = FALSE), nrow(x), ncol(x), byrow = TRUE)
z <- to_mat(rate)
d <- to_mat(den_m)
t <- (pmin(pmax(log2(pmax(z, 1e-6) / nat25), -SPAN), SPAN) + SPAN) / (2 * SPAN)
b <- (pmin(pmax(log10(pmax(d, 1e-6)), DENS[1]), DENS[2]) - DENS[1]) / diff(DENS)
b <- B_MIN + (1 - B_MIN) * b
e <- ext(rate)

# Colour image for one mode: ramp colour blended toward the background by b.
map_img <- function(p) {
  ramp <- grDevices::colorRampPalette(p$div, space = "Lab")(512)
  rgb_r <- grDevices::col2rgb(ramp) / 255
  rgb_bg <- as.vector(grDevices::col2rgb(p$bg) / 255)
  idx <- 1 + round(t * 511)
  mix <- function(ch) {
    v <- rgb_bg[ch] + (rgb_r[ch, idx] - rgb_bg[ch]) * b
    v[is.na(v)] <- 0
    v
  }
  img <- matrix(grDevices::rgb(mix(1), mix(2), mix(3)), nrow(z), ncol(z))
  img[is.na(z)] <- "#00000000"
  list(img = img, ramp = ramp)
}

# --- geometry ---------------------------------------------------------------------
map <- list(xmin = -117.3, xmax = e$xmax, ymin = e$ymin, ymax = e$ymax)
ratio <- 1 / cos(mean(c(map$ymin, map$ymax)) * pi / 180)
cities <- city_rates(CITIES, loc, states)

# Inset in the open Pacific, south-west of Baja California: national series.
ins <- list(x0 = -116.4, x1 = -110.6, y0 = 15.4, y1 = 18.6)
yr_x <- function(y) ins$x0 + (y - 2015) / 10 * (ins$x1 - ins$x0)
pc_y <- function(v) ins$y0 + v / 2 * (ins$y1 - ins$y0)            # axis 0-2%
nat <- nat |> mutate(x = yr_x(year), y = pc_y(pct),
                     lo = pc_y(pct * exp(-1.645 * se / pct)),
                     hi = pc_y(pct * exp(1.645 * se / pct)),
                     lab = sprintf("%s%%", format(round(pct, 2), nsmall = 2)))

# Colour legend in the Gulf of Mexico, ticks at the rates they stand for.
leg <- list(x0 = -96.4, x1 = -89.4, y = 27.3, h = 0.42)
k <- -SPAN:SPAN
leg_x <- function(kk) leg$x0 + (kk + SPAN) / (2 * SPAN) * (leg$x1 - leg$x0)
leg_lab <- sprintf("%.2f%%", nat25 * 2^k)

make <- function(mode) {
  p <- pal(mode)
  m <- map_img(p)
  leg_df <- tibble(x = seq(leg$x0, leg$x1, length.out = 200)) |>
    mutate(fill = m$ramp[1 + round((x - leg$x0) / (leg$x1 - leg$x0) * 511)])
  dx <- diff(leg_df$x[1:2])

  ggplot() +
    annotation_raster(m$img, xmin = e$xmin, xmax = e$xmax, ymin = e$ymin, ymax = e$ymax,
                      interpolate = TRUE) +
    geom_path(data = state_lines, aes(x = x, y = y, group = grp),
              colour = p$faint, linewidth = 0.15, alpha = 0.45) +
    city_layers(cities, ratio, STYLE_FONT, p$ink, p$bg, size = 2.9) +
    annotate("text", x = -107.6, y = 26.35, label = "Sierra Tarahumara",
             colour = p$sub, family = STYLE_FONT, size = 2.7, fontface = "italic") +
    # inset: national share of homes with a panel
    annotate("text", x = ins$x0, y = ins$y1 + 1.35, label = es_en("Nacional", "National"),
             colour = p$ink, family = STYLE_FONT, size = 3.4, fontface = "bold", hjust = 0) +
    annotate("text", x = ins$x0, y = ins$y1 + 0.85, label = es_en("Viviendas con panel solar", "Homes with a solar panel"),
             colour = p$sub, family = STYLE_FONT, size = 2.8, hjust = 0) +
    annotate("segment", x = ins$x0, xend = ins$x1, y = pc_y(0), yend = pc_y(0),
             colour = p$axis, linewidth = 0.35) +
    geom_line(data = nat, aes(x = x, y = y), colour = p$accent, linewidth = 0.7) +
    geom_errorbar(data = nat, aes(x = x, ymin = lo, ymax = hi), colour = p$accent,
                  width = 0.25, linewidth = 0.4) +
    geom_point(data = nat, aes(x = x, y = y), colour = p$accent, size = 1.9) +
    geom_text(data = nat, aes(x = x, y = hi + 0.3, label = lab), colour = p$ink,
              family = STYLE_FONT, size = 2.9, vjust = 0) +
    geom_text(data = nat, aes(x = x, y = pc_y(0) - 0.3, label = year), colour = p$sub,
              family = STYLE_FONT, size = 2.7, vjust = 1) +
    # legend
    geom_tile(data = leg_df, aes(x = x, y = leg$y, fill = fill),
              width = dx * 1.05, height = leg$h) +
    scale_fill_identity() +
    annotate("segment", x = leg_x(k), xend = leg_x(k), y = leg$y - leg$h / 2,
             yend = leg$y - leg$h / 2 - 0.15, colour = p$axis, linewidth = 0.3) +
    annotate("text", x = leg_x(k), y = leg$y - leg$h / 2 - 0.3, label = leg_lab,
             colour = p$sub, family = STYLE_FONT, size = 2.6, vjust = 1) +
    annotate("text", x = leg$x0, y = leg$y + leg$h / 2 + 0.9,
             label = es_en("Viviendas con panel solar, 2025", "Homes with a solar panel, 2025"), colour = p$ink,
             family = STYLE_FONT, size = 3.1, fontface = "bold", hjust = 0) +
    annotate("text", x = leg$x0, y = leg$y + leg$h / 2 + 0.4,
             label = sprintf(es_en("Azul, debajo del promedio nacional (%.2f%%). Rojo, arriba.", "Blue, below the national average (%.2f%%). Red, above."), nat25),
             colour = p$sub, family = STYLE_FONT, size = 2.6, hjust = 0) +
    annotate("text", x = leg$x0, y = leg$y - leg$h / 2 - 1.15,
             label = es_en("Más tenue, menos viviendas", "Fainter, fewer homes"), colour = p$sub,
             family = STYLE_FONT, size = 2.6, hjust = 0) +
    coord_fixed(ratio = ratio, xlim = c(map$xmin - 0.4, map$xmax + 0.3),
                ylim = c(map$ymin - 0.3, map$ymax + 0.3), expand = FALSE) +
    labs(title = es_en("Los hogares con paneles solares casi se duplicaron en cinco años", "Mexican homes with solar panels nearly doubled in five years"),
         subtitle = es_en("Mexican homes with solar panels nearly doubled in five years · México, 2015-2025", "Mexico, 2015-2025"),
         caption = caption_ls(
           notes = es_en("Datos municipales de la Encuesta Intercensal 2025, asignados a una malla de población de 1 km (WorldPop 2025) dentro de cada municipio y suavizados. En la Sierra Tarahumara, muchos paneles son probablemente sistemas aislados de la red.",
                         "Municipal data from the 2025 Intercensal Survey, spread over a 1 km population grid (WorldPop 2025) within each municipality and smoothed. In the Sierra Tarahumara, many panels are probably off-grid systems."),
           source = es_en("INEGI, Encuesta Intercensal 2025 (municipios), Intercensal 2015 y Censo 2020 (microdatos). WorldPop 2025.",
                          "INEGI, 2025 Intercensal Survey (municipalities), 2015 Intercensal Survey and 2020 Census (microdata). WorldPop 2025."),
           width = 175)) +
    theme_ls(mode) +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = margin(22, 22, 14, 22))
}

save_ls(make, "images/f1_map_2025", width = 12, height = 9.3)
message("DONE!")
