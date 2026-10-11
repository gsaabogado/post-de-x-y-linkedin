# 04_fig_both.R ---------------------------------------------------------------
# Figure F3 (tweet 3): temperature and rainfall at once. Each place takes the
# colour of its position in a square whose horizontal axis is temperature and
# whose vertical axis is rainfall, both measured as the share of the territory
# below that value (so half the country lies on each side of the middle).
#
# World cities are placed in the same square from their own 1991-2020 averages
# in the same data (in/world_cities_pool.rds, CMCC job 702777), which says where
# in Mexico the yearly averages resemble theirs. The nine cities are the ones
# approved in July; only their positions are computed here.
#
# The design began as a version of Milos Popovic's temperature and
# precipitation map of the Iberian peninsula.
#
# Run from the post root: Rscript scripts/04_fig_both.R   (FIG_LANG=en for English)

source("scripts/_map.R")

num <- readRDS("data/derived/numbers.rds")
tx <- share_below(FLD$tmean[LAND], FLD$q_t)
py <- share_below(FLD$ppt[LAND], FLD$q_p)
cities <- mutate(cities_xy(), code = name)

# --- world cities (the nine of _map.R) -------------------------------------------
world <- world_cities() |>
  mutate(sx = pmin(pmax(share_below(tmean, FLD$q_t), 0.04), 0.96),
         sy = pmin(pmax(share_below(ppt, FLD$q_p), 0.04), 0.96))

# The three cities named in the title must have their climate somewhere in Mexico.
TITLE_CITIES <- c("Phoenix", "Paris", "Hong Kong")
like <- num$world[match(TITLE_CITIES, num$world$name), ]
stopifnot(!anyNA(like$like_share), all(like$like_share > 0.01))

# --- the square, in map coordinates ------------------------------------------------
SQ <- list(side = LEG$w, x0 = LEG$x0, y0 = LEG$top - LEG$w)
sq_x <- function(s) SQ$x0 + s * SQ$side
sq_y <- function(s) SQ$y0 + s * SQ$side
world <- world |>
  mutate(X = sq_x(sx), Y = sq_y(sy),
         hj = c(r = 0, l = 1, b = 0.5, a = 0.5)[side],
         dx = c(r = 34, l = -34, b = 0, a = 0)[side],
         dy = c(r = -17, l = -17, b = -60, a = 28)[side])
TICK_S <- c(0.05, 0.5, 0.95)
tick_t <- sprintf("%.0f °C", value_at_share(TICK_S, FLD$q_t))
tick_p <- sprintf("%s mm", format(round(value_at_share(TICK_S, FLD$q_p), -1), big.mark = ","))

make <- function(mode) {
  p <- pal(mode)
  cp <- climate_pal(p)
  img <- relief_image(square_channels(tx, py, cp))
  n <- 240
  g <- expand.grid(sx = seq(0, 1, length.out = n), sy = seq(1, 0, length.out = n))
  ch <- square_channels(g$sx, g$sy, cp)
  sq <- grDevices::as.raster(matrix(grDevices::rgb(ch[[1]], ch[[2]], ch[[3]]), n, n, byrow = TRUE))
  lab <- leg_text
  base_map(img, p) +
    city_layers(cities, p) +
    # legend square
    annotation_raster(sq, xmin = SQ$x0, xmax = SQ$x0 + SQ$side, ymin = SQ$y0, ymax = SQ$y0 + SQ$side) +
    leg_title(p, es_en("Temperatura y lluvia a la vez", "Temperature and rainfall at once"),
              es_en("Cada color es una combinación.\nLos puntos son ciudades del mundo.",
                    "Each colour is a combination.\nThe dots are cities of the world.")) +
    lab(sq_x(TICK_S), SQ$y0 - 30 * KM, tick_t, colour = p$sub, size = 2.6, vjust = 1) +
    lab(SQ$x0 - 30 * KM, sq_y(TICK_S), tick_p, colour = p$sub, size = 2.6, hjust = 1) +
    lab(SQ$x0, SQ$y0 - 120 * KM, es_en("Más calor →", "Warmer →"),
        colour = p$ink, size = 2.9, hjust = 0, vjust = 1) +
    # rotated, in the column of the rainfall ticks, between the upper two
    lab(SQ$x0 - 60 * KM, sq_y(0.725), es_en("Más lluvia →", "More rain →"),
        colour = p$ink, size = 2.9, hjust = 0.5, vjust = 0.5, angle = 90) +
    geom_point(data = world, aes(X, Y), shape = 16, size = 2.6, colour = p$bg) +
    geom_point(data = world, aes(X, Y), shape = 16, size = 1.4, colour = p$ink) +
    city_text(world, p) +
    labs(title = es_en("En México hay climas como el de Phoenix,\nel de París y el de Hong Kong",
                       "Mexico holds climates like those of Phoenix,\nParis and Hong Kong"),
         subtitle = es_en("Mexico holds climates like those of Phoenix, Paris and Hong Kong · Temperatura y lluvia promedio, 1991-2020",
                          "Average temperature and rainfall, 1991-2020"),
         caption = caption_ls(
           notes = c(
             es_en("El color de cada lugar depende de dos cosas a la vez: su temperatura promedio y la lluvia que recibe en un año. El recuadro muestra qué color corresponde a cada combinación.",
                   "The colour of each place depends on two things at once: its average temperature and the rain it receives in a year. The square shows the colour of each combination."),
             es_en("Los ejes del recuadro ordenan el territorio de México. A la mitad de cada eje, la mitad del país queda de cada lado.",
                   "The axes of the square rank Mexico's territory. At the middle of each axis, half of the country lies on each side."),
             es_en("Las ciudades del mundo se colocan según su temperatura y su lluvia promedio utilizando los mismos datos. Coinciden en el promedio del año, aunque la temperatura y la lluvia cambien distinto durante el año.",
                   "World cities are placed by their average temperature and rainfall, using the same data. They match on the yearly average, even if temperature and rain move differently through the year."),
             paste(NOTE_GRID, NOTE_MODEL)),
           source = SOURCE, width = 210)) +
    map_frame(mode)
}

save_ls(make, "images/f3_temperature_rainfall", width = FIG_W, height = FIG_H)
message("DONE!")
