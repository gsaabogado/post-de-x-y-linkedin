# 05_fig_v2.R -----------------------------------------------------------------
# Figure F1 in the series style v2 (../_style/style.R), light, with the byline.
# Same geometry and data as 02_figure.R, which stays as the dark v1 version.
#
# What changed against v1:
#   - bars in one slate blue; colour no longer repeats the length
#   - the three hero numbers became two shaded strips, labelled where they sit:
#     the narrowest latitude and the narrowest longitude strip each holding
#     half of GDP (Luis, 2026-09-25: both at 50%, so the two read alike)
#   - the Tropic of Cancer carries its own share as a small label
#   - Spanish only on the canvas; the English line is the subtitle
#
# GDP is EXTENSIVE, so bars are sums and grow from zero. Both histograms share
# one billions-per-degree scale, corrected for coord_fixed's y stretch, so a bar
# of a given visual length means the same money on either axis.
#
# Run from the post root: Rscript scripts/05_fig_v2.R

library(dplyr)
library(ggplot2)
library(sf)

source("../_style/style.R")
source("scripts/_layout.R")
source("scripts/_stats.R")
source("scripts/_places.R")
source("scripts/_money.R")
FX <- readRDS("data/derived/fx_2026_08.rds")$mxn_per_usd   # 10_fx.R, pesos per dollar

prof_lat <- readRDS("data/derived/prof_lat_fine.rds")
prof_lon <- readRDS("data/derived/prof_lon_fine.rds")
meta <- readRDS("data/derived/meta.rds")
mex_poly <- readRDS("data/derived/mex_poly.rds")

TROPIC <- 23.4367
LAT_W <- 9.0          # degrees of longitude for the tallest bar on either axis
BINF <- meta$res
KM_DEG <- 111.2       # km per degree of latitude

pl <- filter(prof_lat, gdp > 0)
po <- filter(prof_lon, gdp > 0, band >= meta$x_min_display)
stopifnot(abs(sum(pl$gdp) / meta$total_gdp - 1) < 1e-4,
          abs(sum(po$gdp) / meta$total_gdp - 1) < 1e-4)

# PPP dollars of 2021 -> pesos of August 2026 (_money.R). Shares do not move.
K <- mxn_factor(meta$total_gdp)
pl <- mutate(pl, gdp = gdp * K, per_degree = per_degree * K)
po <- mutate(po, gdp = gdp * K, per_degree = per_degree * K)
total_mxn <- meta$total_gdp * K
message(sprintf("%.3f pesos of Aug 2026 per PPP dollar; total %.2f billones", K, total_mxn / 1e12))

# --- geometry ---------------------------------------------------------------
peak <- max(pl$per_degree, po$per_degree)
L <- marginal_layout(meta, peak, lat_w = LAT_W)
map <- L$map; ratio <- L$ratio
lat_base <- L$lat_base; lon_base <- L$lon_base
atx <- L$atx; aty <- L$aty

lat_band <- pl |> mutate(xmin = lat_base, xmax = atx(per_degree),
                         ymin = band, ymax = band + BINF)
lon_band <- po |> mutate(ymin = lon_base, ymax = aty(per_degree),
                         xmin = band, xmax = band + BINF)
lat_span <- range(lat_band$ymin, lat_band$ymax)
lon_span <- range(lon_band$xmin, lon_band$xmax)

ticks <- c(0, 5, 10, 15, 20) * 1e12
tick_lab <- c("0", "5", "10", "15", "20")

# --- the numbers the figure states --------------------------------------------
S <- list(
  lat50 = narrow_window(pl, "gdp", 0.50),
  lon50 = narrow_window(po, "gdp", 0.50),
  north_share = sum(pl$gdp[pl$mid >= TROPIC]) / sum(pl$gdp)
)
lat_km <- S$lat50$width * KM_DEG
lon_km <- S$lon50$width * KM_DEG * cos(mean(c(S$lat50$lo, S$lat50$hi)) * pi / 180)
message(sprintf("half of GDP in %.2f-%.2fN (%.2f deg, %.0f km, share %.3f)",
                S$lat50$lo, S$lat50$hi, S$lat50$width, lat_km, S$lat50$share))
message(sprintf("%.1f%% of GDP in %.2f-%.2fW (%.2f deg, %.0f km)",
                100 * S$lon50$share, -S$lon50$lo, -S$lon50$hi, S$lon50$width, lon_km))
message(sprintf("north of the Tropic: %.1f%%", 100 * S$north_share))

outline <- st_coordinates(mex_poly)
outline <- tibble(x = outline[, "X"], y = outline[, "Y"],
                  grp = paste(outline[, "L3"], outline[, "L2"], outline[, "L1"])) |>
  filter(x >= map$xmin)

cities <- CITIES |>
  filter(lon >= map$xmin) |>
  mutate(code = name,
         code = ifelse(code == "Mexico City", "Ciudad de México", code),
         px = atx(band_val(pl, lat, BINF)) + 0.3, ly = lat,
         cx = lon, cy = aty(band_val(po, lon, BINF)) + 0.3)

canvas <- list(xmin = map$xmin - 3.0, xmax = lat_base + LAT_W + 5.2,
               ymin = lon_base - 1.4, ymax = map$ymax + 2.6)

make <- function(mode) {
  p <- pal(mode)
  ggplot() +
    # the two strips, drawn first so bars and outline sit on top
    annotate("rect", xmin = canvas$xmin, xmax = lat_base + LAT_W,
             ymin = S$lat50$lo, ymax = S$lat50$hi, fill = p$accent_soft, alpha = 0.55) +
    annotate("rect", xmin = S$lon50$lo, xmax = S$lon50$hi,
             ymin = lon_base, ymax = map$ymax, fill = p$accent_soft, alpha = 0.55) +
    geom_path(data = outline, aes(x = x, y = y, group = grp),
              colour = p$faint, linewidth = 0.25, alpha = 0.7) +
    # reference grid, clipped to each histogram's own span
    annotate("segment", x = atx(ticks), xend = atx(ticks),
             y = lat_span[1], yend = lat_span[2], colour = p$grid, linewidth = 0.35) +
    annotate("segment", y = aty(ticks), yend = aty(ticks),
             x = lon_span[1], xend = lon_span[2], colour = p$grid, linewidth = 0.35) +
    # the histograms
    geom_rect(data = lat_band, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = p$primary) +
    geom_rect(data = lon_band, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = p$primary) +
    annotate("segment", x = lat_base, xend = lat_base, y = lat_span[1], yend = lat_span[2],
             colour = p$axis, linewidth = 0.45) +
    annotate("segment", x = lon_span[1], xend = lon_span[2], y = lon_base, yend = lon_base,
             colour = p$axis, linewidth = 0.45) +
    # scale labels and titles
    annotate("text", x = atx(ticks), y = lat_span[2] + 0.3, label = tick_lab,
             colour = p$sub, family = STYLE_FONT, size = 2.8, vjust = 0) +
    annotate("text", x = lat_base, y = lat_span[2] + 1.2,
             label = "Billones de pesos\npor grado de latitud",
             colour = p$ink, family = STYLE_FONT, size = 3.0, hjust = 0, vjust = 0,
             lineheight = 1.05) +
    annotate("text", x = lon_span[1] - 0.3, y = aty(ticks), label = tick_lab,
             colour = p$sub, family = STYLE_FONT, size = 2.8, hjust = 1) +
    annotate("text", x = lon_span[1], y = aty(27e12),
             label = "Billones de pesos por grado de longitud",
             colour = p$ink, family = STYLE_FONT, size = 3.0, hjust = 0, vjust = 0) +
    # Tropic of Cancer, with its share
    annotate("segment", x = canvas$xmin, xend = lat_base + LAT_W,
             y = TROPIC, yend = TROPIC, colour = p$sub, linewidth = 0.35, linetype = "22") +
    annotate("text", x = map$xmin + 0.2, y = TROPIC + 0.25,
             label = sprintf("Trópico de Cáncer. Al norte se produce el %.0f%% del PIB",
                             100 * S$north_share),
             colour = p$sub, family = STYLE_FONT, size = 2.9, hjust = 0, vjust = 0) +
    # strip labels, in the empty Pacific and Gulf
    annotate("text", x = map$xmin + 0.2, y = S$lat50$lo - 0.3,
             label = sprintf("La mitad del PIB se produce\nentre %.2f°N y %.2f°N,\nuna franja de %.0f km",
                             S$lat50$lo, S$lat50$hi, round(lat_km, -1)),
             colour = p$accent, family = STYLE_FONT, size = 3.1, hjust = 0, vjust = 1,
             lineheight = 1.05) +
    annotate("text", x = S$lon50$hi + 0.4, y = aty(21.5e12),
             label = sprintf("La mitad del PIB se produce\nentre %.2f°O y %.2f°O,\nuna franja de %.0f km",
                             -S$lon50$lo, -S$lon50$hi, round(lon_km, -1)),
             colour = p$accent, family = STYLE_FONT, size = 3.1, hjust = 0, vjust = 1,
             lineheight = 1.05) +
    # the four cities, found on each axis
    halo_text(cities, "px", "ly", ratio, STYLE_FONT, p$ink, p$bg,
              size = 2.9, hjust = 0, vjust = 0.5) +
    halo_text(filter(cities, name != "Monterrey"), "cx", "cy", ratio, STYLE_FONT, p$ink, p$bg,
              size = 2.9, hjust = 0.5, vjust = 0) +
    # Monterrey's bar stands next to the capital's; its label shifts left of it
    halo_text(filter(cities, name == "Monterrey") |> mutate(cx = cx - 0.35),
              "cx", "cy", ratio, STYLE_FONT, p$ink, p$bg, size = 2.9, hjust = 0.5, vjust = 0) +
    coord_fixed(ratio = ratio, xlim = c(canvas$xmin, canvas$xmax),
                ylim = c(canvas$ymin, canvas$ymax), expand = FALSE, clip = "off") +
    labs(title = sprintf("La mitad del PIB de México se produce en una franja de %.0f km",
                         round(lat_km, -1)),
         subtitle = sprintf("Half of Mexico's GDP is produced in a strip %.0f km wide · México, 2021",
                            round(lat_km, -1)),
         caption = caption_ls(
           notes = sprintf("Cada barra suma el PIB producido en una franja de 0.25 grados (unos 28 km). Las barras de la derecha suman por latitud y las de abajo por longitud, con la misma escala, así que una misma longitud es la misma cantidad de dinero. Un billón es un millón de millones. PIB de 2021 en pesos de agosto de 2026 (INPC). El contorno de México solo sirve de referencia. PIB total, %.1f billones de pesos (%.1f billones de dólares, a %.2f pesos por dólar, promedio de agosto de 2026).",
                           total_mxn / 1e12, total_mxn / FX / 1e12, FX),
           source = "Rossi-Hansberg y Zhang (2025), NBER WP 33458, versión 2, 2021. INEGI, PIB e INPC.",
           width = 150)) +
    theme_ls(mode) +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank())
}

dir.create("images", showWarnings = FALSE)
save_ls(make, "images/f1_gdp_marginals_2021", width = 12, height = 9.3)
message("DONE!")
