# 08_animate_v2.R -------------------------------------------------------------
# The fold as a video, in the series style v2 (../_style/style.R), light, in
# pesos of August 2026, with the byline. 03_animate.R stays as the dark v1.
#
# Same mechanic as v1: every dot carries the same money, so when the dots of a
# band queue up side by side the queue's length IS the money in that band, and
# a full queue matches the bar F1 draws. The ghost map keeps the v1 dasymetric
# surface, coloured on the light ramp.
#
# What changed against v1: light palette, dots in the slate blue F1's bars use,
# Spanish narration only, pesos, and the closing hero numbers replaced by the
# same shaded strips and labels as F1.
#
# Run from the post root: Rscript scripts/08_animate_v2.R
# Preview a few frames: PREVIEW_FRAMES=1,120,300,480 Rscript scripts/08_animate_v2.R
#
# Two videos from one script (Luis, 2026-09-25: one idea per tweet):
#   VIDEO=total       the fold of total GDP, ending on the two 50% strips (tweet 1)
#   VIDEO=per_person  total-GDP bars morphing into GDP per person (tweet 2; post 03
#                     folded in here). Its own title, so it stands alone.
# VIDEO=total Rscript scripts/08_animate_v2.R ; VIDEO=per_person Rscript scripts/08_animate_v2.R

library(dplyr)
library(terra)
library(ggplot2)
library(ragg)
library(sf)
library(parallel)

source("../_style/style.R")
source("scripts/_layout.R")
source("scripts/_stats.R")
source("scripts/_places.R")
source("scripts/_raster.R")
source("scripts/_money.R")

set.seed(20260925)

MODE <- "light"
VIDEO <- Sys.getenv("VIDEO", "total")
stopifnot(VIDEO %in% c("total", "per_person"))
P <- pal(MODE)
N_DOTS <- 90000
FPS <- 30
W_PX <- 1720
H_PX <- 1334          # F1's 12 x 9.3 aspect
SS <- 2
DPI <- 150 * SS
CORES <- 5L           # the workstation runs other work; see CLAUDE.md
STAGGER <- 0.45
SPEEDUP <- 1.2       # Luis, 2026-09-26: "todo 20% más rápido", both videos, holds included
SLOWDOWN <- 1.3 / SPEEDUP
GHOST_REST <- 1.00
GHOST_FLY <- 0.35     # slate dots over a slate map: dim hard while they fly
DOT_ALPHA <- 0.55
DOT_IGNITE <- 0.18
LAT_W <- 9.0
TROPIC <- 23.4367
KM_DEG <- 111.2

grid_map <- readRDS("data/derived/grid_map.rds")
prof_lat <- readRDS("data/derived/prof_lat_fine.rds")
prof_lon <- readRDS("data/derived/prof_lon_fine.rds")
meta <- readRDS("data/derived/meta.rds")
mex_poly <- readRDS("data/derived/mex_poly.rds")

# PPP dollars of 2021 -> pesos of August 2026 (_money.R). The ghost raster stays
# in dollars: it only picks colours, and a constant factor moves no colour once
# the limits move with it.
K <- mxn_factor(meta$total_gdp)
FX <- readRDS("data/derived/fx_2026_08.rds")$mxn_per_usd   # 10_fx.R, pesos per dollar
BINF <- meta$res
g <- filter(grid_map, x >= meta$x_min_display) |> mutate(gdp = gdp * K)
pl <- filter(prof_lat, gdp > 0) |> mutate(gdp = gdp * K, per_degree = per_degree * K)
po <- filter(prof_lon, gdp > 0, band >= meta$x_min_display) |>
  mutate(gdp = gdp * K, per_degree = per_degree * K)

peak <- max(pl$per_degree, po$per_degree)
L <- marginal_layout(meta, peak, lat_w = LAT_W)
map <- L$map; ratio <- L$ratio
canvas <- list(xmin = map$xmin - 3.0, xmax = L$lat_base + LAT_W + 5.2,
               ymin = L$lon_base - 1.6, ymax = map$ymax + 3.4)

# --- the ghost ------------------------------------------------------------------
r_gdp <- rast("data/derived/mex_gdp_1km.tif")
LIM_G <- log10(c(1e4, 1e8))     # dollars per square kilometre
map_px <- round(W_PX * SS * (map$xmax - map$xmin) / (canvas$xmax - canvas$xmin))
# Light map, slate ramp (Luis, 2026-09-26): back from the night map, but the
# brick ramp read as blood, so GDP runs pale to the slate blue of the bars and
# brick red stays on the two 50% strips alone. Dimming blends toward the page
# white inside the country outline. The night version is in 08_animate_v2.R.bak_night.
LAND <- "#F2F2F0"
LIGHTS <- c(LAND, "#DCE1E7", "#B7C2CE", "#8C9DB0", "#5F7690", "#3A5068", "#1F2E3D")
DIM <- P$bg
GULF_X <- -96.6                      # open Gulf at the Tropic (the coast is at ~97.7 W)
dot_ramp <- grDevices::colorRamp(c(P$primary, P$primary), space = "Lab")
ghost <- map_image(r_gdp, ncol_px = map_px, ramp = LIGHTS, transform = lg, limits = LIM_G)
message(sprintf("ghost %d x %d px", ghost$ncol, ghost$nrow))

# --- dots (sampled from the 25 km grid; see 03_animate.R for why) ---------------
idx <- sample.int(nrow(g), N_DOTS, replace = TRUE, prob = g$gdp)
per_dot <- sum(g$gdp) / N_DOTS
dot_w <- (L$s_x / BINF) * per_dot
dot_h <- (L$s_y / BINF) * per_dot
message(sprintf("%s dots, %.0f million pesos each", format(N_DOTS, big.mark = ","), per_dot / 1e6))

dots <- tibble(
  x0 = g$x[idx] + runif(N_DOTS, -BINF / 2, BINF / 2),
  y0 = g$y[idx] + runif(N_DOTS, -BINF / 2, BINF / 2)
) |>
  mutate(row = floor(y0 / BINF) * BINF, col = floor(x0 / BINF) * BINF) |>
  group_by(row) |>
  mutate(x1 = L$atx(0) + (rank(x0, ties.method = "first") - 0.5) * dot_w, y1 = y0) |>
  ungroup() |>
  group_by(col) |>
  mutate(y2 = L$aty(0) + (rank(y0, ties.method = "first") - 0.5) * dot_h, x2 = x0) |>
  ungroup() |>
  mutate(u_lat = (x0 - min(x0)) / diff(range(x0)),
         u_lon = (max(y0) - y0) / diff(range(y0)))

# A queue must be no longer than its bar; additive tolerance (negative longitude).
stopifnot(max(dots$x1) <= L$atx(peak) + 1e-6,
          max(dots$y2) <= L$aty(peak) + 1e-6)

# --- timeline ---------------------------------------------------------------
smooth <- function(p) p * p * (3 - 2 * p)
clamp01 <- function(p) pmin(1, pmax(0, p))
staggered <- function(p, u) smooth(clamp01((p - u * STAGGER) / (1 - STAGGER)))

timeline <- if (VIDEO == "total") {
  tribble(
    ~phase,      ~frames,
    # Motion at its own pace, short pauses (Luis, 2026-09-26: "deben verse
    # dinámicos"). One narration line stays up the whole time, so no phase has
    # to wait for a new line to be read. The paced version is in .bak_paced.
    "map",        42,      # 1.5 s still
    "fold_lat",   85,
    "hold_lat",   40,
    "back_lat",   35,
    "fold_lon",   85,
    "hold_lon",   40,
    "to_final",   40,      # settle into F1: both histograms, both strips, the Tropic
    "final",     140)      # hold on F1 (Luis, 2026-09-25: the mp4 ends on F1 and stays)
} else {
  tribble(
    ~phase,      ~frames,
    "bars_tot",   40,      # motion at its own pace; the line stays up throughout
    "morph_pc",   65,
    "hold_pc",   160)
}
timeline <- timeline |>
  mutate(frames = round(frames * SLOWDOWN),
         end = cumsum(frames), start = end - frames + 1)
N_FRAMES <- sum(timeline$frames)

frame_state <- function(i) {
  ph <- timeline[which(i >= timeline$start & i <= timeline$end)[1], ]
  list(phase = ph$phase, p = (i - ph$start) / max(1, ph$frames - 1))
}

# --- furniture --------------------------------------------------------------
ticks <- c(0, 5, 10, 15, 20) * 1e12
tick_lab <- c("0", "5", "10", "15", "20")
lat_span <- range(pl$band, pl$band + BINF)
lon_span <- range(po$band, po$band + BINF)

S <- list(lat50 = narrow_window(pl, "gdp", 0.50),
          lon50 = narrow_window(po, "gdp", 0.50),
          north_share = sum(pl$gdp[pl$mid >= TROPIC]) / sum(pl$gdp))
lat_km <- S$lat50$width * KM_DEG
lon_km <- S$lon50$width * KM_DEG * cos(mean(c(S$lat50$lo, S$lat50$hi)) * pi / 180)

# --- per person (post 03 folded in, Luis 2026-09-25) --------------------------------
# GDP per person is a RATIO: it cannot be folded, so there are no dots here. Each
# 0.25-degree bar morphs in place to the GDP per person of its 1-degree band
# (post 03's bin: it averages the sub-metro noise of the downscaling). Every
# 1-degree band holds more than 50,000 people, post 03's floor; asserted.
PC_MAX <- 650e3                      # pesos per person at full bar length
s_pcx <- LAT_W / PC_MAX
s_pcy <- s_pcx / ratio
NAT_PC <- sum(grid_map$gdp) * K / sum(grid_map$pop)
one_deg <- function(p) p |> mutate(deg = floor(band)) |> group_by(deg) |>
  summarise(g1 = sum(gdp), p1 = sum(pop), .groups = "drop") |> mutate(pc1 = g1 / p1)
d_lat <- one_deg(pl); d_lon <- one_deg(po)
stopifnot(all(d_lat$p1 >= 5e4), all(d_lon$p1 >= 5e4), max(d_lat$pc1, d_lon$pc1) <= PC_MAX)
bars_lat <- pl |> mutate(deg = floor(band)) |> left_join(d_lat, by = "deg") |>
  transmute(ymin = band, ymax = band + BINF, l_tot = per_degree * L$s_x, l_pc = pc1 * s_pcx)
bars_lon <- po |> mutate(deg = floor(band)) |> left_join(d_lon, by = "deg") |>
  transmute(xmin = band, xmax = band + BINF, l_tot = per_degree * L$s_y, l_pc = pc1 * s_pcy)
stopifnot(!anyNA(bars_lat), !anyNA(bars_lon))
nor <- grid_map$y >= TROPIC
NS_RATIO <- (sum(grid_map$gdp[nor]) / sum(grid_map$pop[nor])) /
  (sum(grid_map$gdp[!nor]) / sum(grid_map$pop[!nor]))
message(sprintf("national %.0f pesos per person; north/south %.2fx; max 1-deg band lat %.0f, lon %.0f",
                NAT_PC, NS_RATIO, max(d_lat$pc1), max(d_lon$pc1)))
camp <- d_lon[which.max(d_lon$pc1), ]
stopifnot(camp$deg == -91)          # 91-90 W: Campeche; re-check the label if this moves
pc_ticks <- c(0, 200, 400, 600) * 1e3
pc_lab <- c("0", "200", "400", "600")
cities_pc <- cities_pc_src <- NULL   # filled after `cities` below

# One fixed line per video (Luis, 2026-09-26): it stays up from the first frame
# until the close, so the motion never waits for text. What the old per-phase
# lines said is either visible (the folds, the axis titles) or in the notes.
LINE_TOTAL <- sprintf(es_en("Cada punto son %.0f millones de pesos (%.0f millones de dólares).", "Each dot is %.0f million pesos (%.0f million dollars)."),
                      per_dot / 1e6, per_dot / FX / 1e6)
LINE_PC <- es_en("Dividimos el PIB de cada franja entre la gente que vive en ella.", "We divide the GDP of each band by the people who live in it.")
NARR <- list(
  map = LINE_TOTAL, fold_lat = LINE_TOTAL, hold_lat = LINE_TOTAL, back_lat = LINE_TOTAL,
  fold_lon = LINE_TOTAL, hold_lon = LINE_TOTAL,
  bars_tot = LINE_PC, morph_pc = LINE_PC, hold_pc = LINE_PC,
  to_final = "", final = ""
)
stopifnot(all(timeline$phase %in% names(NARR)))

# Every line stays up long enough to read: (1 s + 4 words per second) / SPEEDUP,
# counted over the whole run of phases that show the same line.
narr_run <- rle(unlist(NARR[timeline$phase]))
run_s <- tapply(timeline$frames, rep(seq_along(narr_run$lengths), narr_run$lengths), sum) / FPS
read_s <- (1 + lengths(strsplit(narr_run$values, "[[:space:]]+")) / 4) / SPEEDUP
short <- nzchar(narr_run$values) & run_s < read_s - 0.05
if (any(short)) stop("narration too short: ", paste(narr_run$values[short], collapse = " | "))

outline <- st_coordinates(mex_poly)
outline <- tibble(x = outline[, "X"], y = outline[, "Y"],
                  grp = paste(outline[, "L3"], outline[, "L2"], outline[, "L1"])) |>
  filter(x >= map$xmin)

cities <- CITIES |>
  filter(lon >= map$xmin) |>
  mutate(code = ifelse(name == "Mexico City", es_en("Ciudad de México", "Mexico City"), name),
         px = L$atx(band_val(pl, lat, BINF)) + 0.3, ly = lat,
         cx = lon, cy = L$aty(band_val(po, lon, BINF)) + 0.3)
# per-person bar tips on the latitude axis, for the two cities the turn is about
cities_pc <- cities |> filter(name %in% c("Mexico City", "Monterrey")) |>
  mutate(px = L$lat_base + d_lat$pc1[match(floor(lat), d_lat$deg)] * s_pcx + 0.3)

TITLE <- if (VIDEO == "total") {
  sprintf(es_en("La mitad del PIB de México se produce en una franja de %.0f km", "Half of Mexico's GDP is produced in a strip %.0f km wide"), round(lat_km, -1))
} else sprintf(es_en("Por persona, el norte de México produce %.1f veces más que el sur", "Per person, northern Mexico produces %.1f times as much as the south"), NS_RATIO)
SUBTITLE <- if (FIG_LANG == "en") "Mexico, 2021" else if (VIDEO == "total") {
  sprintf("Half of Mexico's GDP is produced in a strip %.0f km wide · México, 2021", round(lat_km, -1))
} else sprintf("Per person, northern Mexico produces %.1f times as much as the south · México, 2021", NS_RATIO)

SOURCE <- es_en("Rossi-Hansberg y Zhang (2025), NBER WP 33458, versión 2, 2021. INEGI, PIB e INPC.", "Rossi-Hansberg and Zhang (2025), NBER WP 33458, version 2, 2021. INEGI, GDP and INPC.")
CAPTION <- if (VIDEO == "total") {
  caption_ls(notes = sprintf(es_en("Cada barra suma el PIB producido en una franja de 0.25 grados (unos 28 km). Las barras de la derecha suman por latitud y las de abajo por longitud, con la misma escala, así que una misma longitud es la misma cantidad de dinero. Un billón es un millón de millones. PIB de 2021 en pesos de agosto de 2026 (INPC). El contorno de México solo sirve de referencia. El mapa reparte el PIB según dónde vive la gente, a 1 km. PIB total, %.1f billones de pesos (%.1f billones de dólares, a %.2f pesos por dólar, promedio de agosto de 2026).",
                             "Each bar adds up the GDP produced in a band 0.25 degrees wide (about 28 km). The bars on the right add up by latitude and those at the bottom by longitude, on the same scale, so equal length means equal money. GDP of 2021 in pesos of August 2026 (INPC). The outline of Mexico is only a reference. The map spreads GDP by where people live, at 1 km. Total GDP, %.1f trillion pesos (%.1f trillion dollars, at %.2f pesos per dollar, August 2026 average)."),
                             sum(g$gdp) / 1e12, sum(g$gdp) / FX / 1e12, FX), source = SOURCE)
} else {
  caption_ls(notes = es_en("Cada barra divide el PIB de una franja de 1 grado (unos 110 km) entre la gente que vive en ella. PIB de 2021 en pesos de agosto de 2026 (INPC). El PIB de cada estado viene de INEGI, pero dentro de cada estado lo reparte un modelo que se apoya sobre todo en la población, así que las diferencias reales entre lugares probablemente son mayores.",
                          "Each bar divides the GDP of a band 1 degree wide (about 110 km) by the people who live in it. GDP of 2021 in pesos of August 2026 (INPC). Each state's GDP comes from INEGI, but within each state a model spreads it mostly by population, so the real differences between places are probably larger."),
             source = SOURCE)
}

# --- one frame --------------------------------------------------------------
draw_frame <- function(i) {
  st <- frame_state(i); ph <- st$phase; p <- st$p

  d <- dots
  if (ph == "map") {
    d$x <- d$x0; d$y <- d$y0; d$a <- 0; ghost_b <- GHOST_REST
  } else if (ph == "fold_lat") {
    e <- staggered(p, d$u_lat)
    d$x <- d$x0 + (d$x1 - d$x0) * e; d$y <- d$y0 + (d$y1 - d$y0) * e
    d$a <- smooth(clamp01(e / DOT_IGNITE))
    ghost_b <- GHOST_REST - (GHOST_REST - GHOST_FLY) * smooth(clamp01(p / 0.35))
  } else if (ph == "hold_lat") {
    d$x <- d$x1; d$y <- d$y1; d$a <- 1; ghost_b <- GHOST_FLY
  } else if (ph == "back_lat") {
    e <- smooth(p)
    d$x <- d$x1 + (d$x0 - d$x1) * e; d$y <- d$y1 + (d$y0 - d$y1) * e
    land <- smooth(clamp01((p - 0.68) / 0.32))
    d$a <- 1 - land
    ghost_b <- GHOST_FLY + (GHOST_REST - GHOST_FLY) * land
  } else if (ph == "fold_lon") {
    e <- staggered(p, d$u_lon)
    d$x <- d$x0 + (d$x2 - d$x0) * e; d$y <- d$y0 + (d$y2 - d$y0) * e
    d$a <- smooth(clamp01(e / DOT_IGNITE))
    ghost_b <- GHOST_REST - (GHOST_REST - GHOST_FLY) * smooth(clamp01(p / 0.35))
  } else if (ph == "hold_lon") {
    d$x <- d$x2; d$y <- d$y2; d$a <- 1; ghost_b <- GHOST_FLY
  } else if (ph == "to_final") {
    d$x <- d$x2; d$y <- d$y2; d$a <- 1 - smooth(p); ghost_b <- GHOST_FLY * (1 - smooth(p))
  } else if (ph == "final") {
    d$x <- d$x2; d$y <- d$y2; d$a <- 0; ghost_b <- 0
  } else if (ph == "to_bars") {
    d$x <- d$x2; d$y <- d$y2; d$a <- 1 - smooth(p); ghost_b <- GHOST_FLY
  } else {
    d$a <- 0; d$x <- d$x2; d$y <- d$y2; ghost_b <- GHOST_FLY
  }
  w <- switch(ph, fold_lat = e, fold_lon = e, back_lat = 1 - smooth(p), 1)
  if (length(w) == 1) w <- rep(w, nrow(d))
  d$col <- grDevices::rgb(dot_ramp(clamp01(w)), maxColorValue = 255)
  d$a <- d$a * DOT_ALPHA
  d <- d[d$a > 0.01, , drop = FALSE]

  out <- 1 - smooth(clamp01(p / 0.5))      # totals' furniture leaving in morph_pc
  a_lat <- switch(ph, fold_lat = smooth(p), hold_lat = 1, back_lat = 1 - smooth(p),
                  to_bars = smooth(p), bars_tot = 1, morph_pc = out,
                  to_final = smooth(p), final = 1, 0)
  a_lon <- switch(ph, fold_lon = smooth(p), hold_lon = 1, to_bars = 1, bars_tot = 1,
                  morph_pc = out, to_final = 1, final = 1, 0)
  # solid bars (both axes) from to_bars on; t = 0 total, 1 per person
  b_bar <- switch(ph, to_bars = smooth(p), bars_tot = 1, morph_pc = 1, hold_pc = 1,
                  to_final = smooth(p), final = 1, 0)
  t_pc <- switch(ph, morph_pc = smooth(p), hold_pc = 1, 0)
  a_pc <- switch(ph, morph_pc = smooth(clamp01((p - 0.5) / 0.5)), hold_pc = 1, 0)
  h_pc <- if (ph == "hold_pc") smooth(clamp01(p / 0.3)) else 0
  # strips and their labels fade in once each histogram stands
  s_lat <- switch(ph, hold_lat = smooth(clamp01((p - 0.2) / 0.3)), back_lat = 1 - smooth(clamp01(p / 0.3)),
                  to_final = smooth(p), final = 1, 0)
  s_lon <- switch(ph, hold_lon = smooth(clamp01((p - 0.2) / 0.3)),
                  to_bars = 1 - smooth(clamp01(p / 0.5)), to_final = 1, final = 1, 0)

  # Dark land, the lights, then the land again at (1 - ghost_b) to dim them.
  # At the close of video 1 a white fill fades the country out, leaving F1's
  # bare outline. Strips go above all of this, or it washes them out.
  w_out <- switch(ph, to_final = smooth(p), final = 1, 0)
  gg <- ggplot() +
    geom_polygon(data = outline, aes(x = x, y = y, group = grp), fill = LAND, colour = NA) +
    map_layer(ghost, interpolate = TRUE) +
    geom_polygon(data = outline, aes(x = x, y = y, group = grp), fill = DIM, colour = NA,
                 alpha = 1 - ghost_b)
  if (w_out > 0.01) {
    gg <- gg + geom_polygon(data = outline, aes(x = x, y = y, group = grp),
                            fill = P$bg, colour = NA, alpha = w_out)
  }
  if (s_lat > 0.01) {
    gg <- gg + annotate("rect", xmin = canvas$xmin, xmax = L$lat_base + LAT_W,
                        ymin = S$lat50$lo, ymax = S$lat50$hi,
                        fill = P$accent_soft, alpha = 0.5 * s_lat)
  }
  if (s_lon > 0.01) {
    gg <- gg + annotate("rect", xmin = S$lon50$lo, xmax = S$lon50$hi,
                        ymin = L$lon_base, ymax = map$ymax,
                        fill = P$accent_soft, alpha = 0.5 * s_lon)
  }
  gg <- gg +
    geom_path(data = outline, aes(x = x, y = y, group = grp),
              colour = P$faint, linewidth = 0.22, alpha = 0.6)

  if (a_lat > 0.02) {
    gg <- gg +
      annotate("segment", x = L$atx(ticks), xend = L$atx(ticks),
               y = lat_span[1], yend = lat_span[2],
               colour = P$grid, linewidth = 0.35, alpha = a_lat) +
      annotate("text", x = L$atx(ticks), y = lat_span[2] + 0.3, label = tick_lab,
               colour = P$sub, family = STYLE_FONT, size = 2.8, vjust = 0, alpha = a_lat) +
      annotate("text", x = L$lat_base, y = lat_span[2] + 1.2,
               label = es_en("Billones de pesos\npor grado de latitud", "Trillion pesos\nper degree of latitude"), colour = P$ink,
               family = STYLE_FONT, size = 3.0, hjust = 0, vjust = 0, lineheight = 1.05,
               alpha = a_lat) +
      halo_text(cities, "px", "ly", ratio, STYLE_FONT, P$ink, P$bg,
                size = 2.9, hjust = 0, vjust = 0.5, alpha = a_lat)
  }
  if (a_lon > 0.02) {
    gg <- gg +
      annotate("segment", y = L$aty(ticks), yend = L$aty(ticks),
               x = lon_span[1], xend = lon_span[2],
               colour = P$grid, linewidth = 0.35, alpha = a_lon) +
      annotate("text", x = lon_span[1] - 0.3, y = L$aty(ticks), label = tick_lab,
               colour = P$sub, family = STYLE_FONT, size = 2.8, hjust = 1, alpha = a_lon) +
      annotate("text", x = lon_span[1], y = L$aty(27e12),
               label = es_en("Billones de pesos por grado de longitud", "Trillion pesos per degree of longitude"), colour = P$ink,
               family = STYLE_FONT, size = 3.0, hjust = 0, vjust = 0, alpha = a_lon) +
      halo_text(filter(cities, name != "Monterrey"), "cx", "cy", ratio, STYLE_FONT, P$ink, P$bg,
                size = 2.9, hjust = 0.5, vjust = 0, alpha = a_lon) +
      halo_text(filter(cities, name == "Monterrey") |> mutate(cx = cx - 0.35),
                "cx", "cy", ratio, STYLE_FONT, P$ink, P$bg,
                size = 2.9, hjust = 0.5, vjust = 0, alpha = a_lon)
  }

  if (a_pc > 0.02) {
    gg <- gg +
      annotate("segment", x = L$lat_base + pc_ticks * s_pcx, xend = L$lat_base + pc_ticks * s_pcx,
               y = lat_span[1], yend = lat_span[2], colour = P$grid, linewidth = 0.35, alpha = a_pc) +
      annotate("text", x = L$lat_base + pc_ticks * s_pcx, y = lat_span[2] + 0.3, label = pc_lab,
               colour = P$sub, family = STYLE_FONT, size = 2.8, vjust = 0, alpha = a_pc) +
      annotate("text", x = L$lat_base, y = lat_span[2] + 1.2,
               label = es_en("Miles de pesos por persona al año,\npor grado de latitud", "Thousand pesos per person per year,\nper degree of latitude"), colour = P$ink,
               family = STYLE_FONT, size = 3.0, hjust = 0, vjust = 0, lineheight = 1.05, alpha = a_pc) +
      annotate("segment", y = L$lon_base + pc_ticks * s_pcy, yend = L$lon_base + pc_ticks * s_pcy,
               x = lon_span[1], xend = lon_span[2], colour = P$grid, linewidth = 0.35, alpha = a_pc) +
      annotate("text", x = lon_span[1] - 0.3, y = L$lon_base + pc_ticks * s_pcy, label = pc_lab,
               colour = P$sub, family = STYLE_FONT, size = 2.8, hjust = 1, alpha = a_pc) +
      annotate("text", x = lon_span[1], y = L$aty(27e12),
               label = es_en("Miles de pesos por persona al año, por grado de longitud", "Thousand pesos per person per year, per degree of longitude"), colour = P$ink,
               family = STYLE_FONT, size = 3.0, hjust = 0, vjust = 0, alpha = a_pc)
  }
  if (b_bar > 0.01) {
    bl <- mutate(bars_lat, xmin = L$lat_base, xmax = L$lat_base + l_tot + (l_pc - l_tot) * t_pc)
    bo <- mutate(bars_lon, ymin = L$lon_base, ymax = L$lon_base + l_tot + (l_pc - l_tot) * t_pc)
    gg <- gg +
      geom_rect(data = bl, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = P$primary, alpha = b_bar) +
      geom_rect(data = bo, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = P$primary, alpha = b_bar)
  }
  if (h_pc > 0.01) {
    xn <- L$lat_base + NAT_PC * s_pcx; yn <- L$lon_base + NAT_PC * s_pcy
    gg <- gg +
      annotate("segment", x = xn, xend = xn, y = lat_span[1], yend = lat_span[2],
               colour = P$accent, linewidth = 0.5, linetype = "22", alpha = h_pc) +
      annotate("segment", x = lon_span[1], xend = lon_span[2], y = yn, yend = yn,
               colour = P$accent, linewidth = 0.5, linetype = "22", alpha = h_pc) +
      annotate("text", x = lon_span[2] + 0.4, y = yn, hjust = 0, vjust = 0.5, lineheight = 1.05,
               label = sprintf(es_en("Promedio nacional,\n$%s\npor persona", "National average,\nMX$%s\nper person"),
                               format(round(NAT_PC, -3), big.mark = ",")),
               colour = P$accent, family = STYLE_FONT, size = 3.1, alpha = h_pc) +
      # the tallest per-person column; name the place so nobody has to guess
      annotate("text", x = camp$deg + 0.5, y = L$lon_base + camp$pc1 * s_pcy + 0.3,
               label = "Campeche", hjust = 0.5, vjust = 0,
               colour = P$ink, family = STYLE_FONT, size = 2.9, alpha = h_pc) +
      annotate("segment", x = canvas$xmin, xend = L$lat_base + LAT_W,
               y = TROPIC, yend = TROPIC, colour = P$sub,
               linewidth = 0.35, linetype = "22", alpha = h_pc) +
      # over the Gulf of Mexico, where the page is white: a label on dark land
      # needs a halo, and a halo on 1 km lights looks blotchy
      annotate("text", x = GULF_X, y = TROPIC + 0.25, hjust = 0, vjust = 0, lineheight = 1.05,
               label = sprintf(es_en("Trópico de Cáncer.\nAl norte se produce %.1f veces\nmás por persona que al sur", "Tropic of Cancer.\nNorth of it, output per person\nis %.1f times the south's"),
                               NS_RATIO),
               colour = P$ink, family = STYLE_FONT, size = 3.1, alpha = h_pc) +
      halo_text(cities_pc, "px", "ly", ratio, STYLE_FONT, P$ink, P$bg,
                size = 2.9, hjust = 0, vjust = 0.5, alpha = h_pc)
  }

  if (nrow(d)) {
    gg <- gg +
      geom_point(data = d, aes(x = x, y = y, alpha = a, colour = col),
                 shape = 16, size = 0.24) +
      scale_alpha_identity(guide = "none") +
      scale_colour_identity(guide = "none")
  }

  if (s_lat > 0.01) {
    gg <- gg +
      annotate("segment", x = canvas$xmin, xend = L$lat_base + LAT_W,
               y = TROPIC, yend = TROPIC, colour = P$sub,
               linewidth = 0.35, linetype = "22", alpha = s_lat) +
      # dark map: over the Gulf; once the map has faded (F1's close): F1's spot
      (if (ph %in% c("to_final", "final"))
        annotate("text", x = map$xmin + 0.2, y = TROPIC + 0.25,
                 label = sprintf(es_en("Trópico de Cáncer. Al norte se produce el %.0f%% del PIB", "Tropic of Cancer. %.0f%% of GDP is produced north of it"),
                                 100 * S$north_share),
                 colour = P$sub, family = STYLE_FONT, size = 2.9, hjust = 0, vjust = 0, alpha = s_lat)
       else
        annotate("text", x = GULF_X, y = TROPIC + 0.25, hjust = 0, vjust = 0, lineheight = 1.05,
                 label = sprintf(es_en("Trópico de Cáncer.\nAl norte se produce\nel %.0f%% del PIB", "Tropic of Cancer.\n%.0f%% of GDP is\nproduced north of it"),
                                 100 * S$north_share),
                 colour = P$ink, family = STYLE_FONT, size = 2.9, alpha = s_lat)) +
      annotate("text", x = map$xmin + 0.2, y = S$lat50$lo - 0.3,
               label = sprintf(es_en("La mitad del PIB se produce\nentre %.2f°N y %.2f°N,\nuna franja de %.0f km", "Half of GDP is produced\nbetween %.2f°N and %.2f°N,\na strip %.0f km wide"),
                               S$lat50$lo, S$lat50$hi, round(lat_km, -1)),
               colour = P$accent, family = STYLE_FONT, size = 3.1, hjust = 0, vjust = 1,
               lineheight = 1.05, alpha = s_lat)
  }
  if (s_lon > 0.01) {
    gg <- gg +
      annotate("text", x = S$lon50$hi + 0.4, y = L$aty(21.5e12),
               label = sprintf(es_en("La mitad del PIB se produce\nentre %.2f°O y %.2f°O,\nuna franja de %.0f km", "Half of GDP is produced\nbetween %.2f°W and %.2f°W,\na strip %.0f km wide"),
                               -S$lon50$lo, -S$lon50$hi, round(lon_km, -1)),
               colour = P$accent, family = STYLE_FONT, size = 3.1, hjust = 0, vjust = 1,
               lineheight = 1.05, alpha = s_lon)
  }

  gg +
    annotate("text", x = map$xmin, y = map$ymax + 1.6, label = NARR[[ph]],
             colour = P$ink, family = STYLE_FONT, size = 4.0, hjust = 0, vjust = 0) +
    coord_fixed(ratio = ratio, xlim = c(canvas$xmin, canvas$xmax),
                ylim = c(canvas$ymin, canvas$ymax), expand = FALSE, clip = "off") +
    labs(title = TITLE, subtitle = SUBTITLE, caption = CAPTION) +
    theme_ls(MODE) +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), axis.title.x = element_blank(),
          axis.title.y = element_blank(), panel.grid.major.y = element_blank(),
          plot.margin = margin(20, 20, 14, 20)) +
    byline_ls(MODE)
}

# --- render -----------------------------------------------------------------
FRAMES <- file.path("frames_v2", paste0(VIDEO, if (FIG_LANG == "en") "_en" else ""))
dir.create(FRAMES, showWarnings = FALSE, recursive = TRUE)

render_one <- function(i) {
  f <- sprintf("%s/f%04d.png", FRAMES, i)
  ggsave(f, draw_frame(i), width = W_PX / (DPI / SS), height = H_PX / (DPI / SS),
         dpi = DPI, bg = P$bg, device = agg_png)
  f
}

preview <- Sys.getenv("PREVIEW_FRAMES")
if (nzchar(preview)) {
  for (i in as.integer(strsplit(preview, ",")[[1]])) {
    message(render_one(i), "  phase=", frame_state(i)$phase)
  }
  message("PREVIEW DONE!")
  quit(save = "no")
}

unlink(list.files(FRAMES, full.names = TRUE))

# PSOCK, not fork: a forked child that touches the macOS font stack crashes.
t0 <- Sys.time()
message(sprintf("rendering %d frames on %d workers...", N_FRAMES, CORES))
wd <- getwd()
cl <- makeCluster(CORES)
on.exit(stopCluster(cl), add = TRUE)
clusterExport(cl, ls(globalenv()), envir = globalenv())
clusterExport(cl, "wd", envir = environment())
invisible(clusterEvalQ(cl, {
  setwd(wd)
  suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(ragg)})
}))
res <- parLapply(cl, seq_len(N_FRAMES), function(i) try(render_one(i), silent = TRUE))
bad <- vapply(res, function(r) inherits(r, "try-error") || is.null(r), logical(1))
if (any(bad)) { message(res[[which(bad)[1]]]); stop(sum(bad), " frames failed") }
message(sprintf("frames done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

n_written <- length(list.files(FRAMES, pattern = "^f[0-9]{4}\\.png$"))
if (n_written != N_FRAMES) stop(sprintf("expected %d frames, found %d", N_FRAMES, n_written))

mp4 <- sprintf("images/video_%s_2021_light%s.mp4", VIDEO, if (FIG_LANG == "en") "_en" else "")
gif <- sprintf("images/video_%s_2021_light%s.gif", VIDEO, if (FIG_LANG == "en") "_en" else "")
system2("ffmpeg", c("-y", "-loglevel", "error", "-framerate", FPS,
                    "-i", file.path(FRAMES, "f%04d.png"),
                    "-vf", shQuote(sprintf("scale=%d:%d:flags=lanczos", W_PX, H_PX)),
                    "-c:v", "libx264", "-preset", "slow", "-pix_fmt", "yuv420p",
                    "-crf", "16", "-movflags", "+faststart", mp4))
system2("ffmpeg", c("-y", "-loglevel", "error", "-i", mp4,
                    "-vf", shQuote("fps=20,scale=860:-2:flags=lanczos,split[a][b];[a]palettegen=stats_mode=diff[p];[b][p]paletteuse=dither=bayer:bayer_scale=3"),
                    gif))
if (!file.exists(mp4) || file.size(mp4) < 1e5) stop("mp4 encoding failed")
if (!file.exists(gif) || file.size(gif) < 1e4) stop("gif encoding failed")

message(sprintf("wrote %s (%.1f MB)", mp4, file.size(mp4) / 1e6))
message(sprintf("wrote %s (%.1f MB)", gif, file.size(gif) / 1e6))
message("DONE!")
