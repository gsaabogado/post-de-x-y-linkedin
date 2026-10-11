# 02_fig_temperature.R ----------------------------------------------------------
# Figure F1 (tweet 1): average temperature of the year, 1991-2020, in the
# series style. Colour is the series' diverging ramp around the national
# average (slate blue below, brick red above), linear in degrees, as the rate
# map of post 06. A ramp from pale to brick red alone was tried first and left
# the whole country red (the "reads as blood" problem of post 04).
#
# City labels carry no number: the value of an 11 km cell differs from the
# station figure people know for their city (see 05_numbers.R for the values).
#
# The title's claim is checked here: altitude must explain several times more
# of the differences in temperature across the territory than latitude does
# (scripts/05_numbers.R, each one alone, cells weighted by area).
#
# Run from the post root: Rscript scripts/02_fig_temperature.R   (FIG_LANG=en for English)

source("scripts/_map.R")

num <- readRDS("data/derived/numbers.rds")
stopifnot(num$r2_el > 5 * num$r2_lat)

T_SPAN <- 8                              # degrees each side of the national average
T_LIM <- num$t_mean + c(-1, 1) * T_SPAN  # under 2% of the territory lies outside
stopifnot(share_below(T_LIM[1], FLD$q_t) < 0.02, share_below(T_LIM[2], FLD$q_t) > 0.98)
pos <- function(v) (pmin(pmax(v, T_LIM[1]), T_LIM[2]) - T_LIM[1]) / diff(T_LIM)
cities <- mutate(cities_xy(), code = name)
TICKS <- seq(14, 26, 4)
# World cities under the legend, with the colour each would have on the map
# (Luis, 2026-10-11). A city off the scale takes the colour of its end.
world <- world_cities() |>
  mutate(value = tmean, value_lab = sprintf("%.0f °C", tmean))

make <- function(mode) {
  p <- pal(mode)
  colours <- p$div
  img <- relief_image(ramp_channels(pos(FLD$tmean[LAND]), colours))
  base_map(img, p) +
    city_layers(cities, p) +
    bar_layers(p, colours, pos, ticks = TICKS, tick_lab = sprintf("%d °C", TICKS),
               title = es_en("Temperatura promedio diaria del año", "Average daily temperature of the year"),
               sub = es_en("1991 a 2020", "1991 to 2020"),
               mark = num$t_mean,
               mark_lab = sprintf(es_en("Promedio del país, %.1f °C", "National average, %.1f °C"), num$t_mean),
               world = world,
               world_head = es_en("Ciudades del mundo, en los mismos datos", "Cities of the world, in the same data"),
               facts = c(
                 # the two figures that answer the thread's question (Luis, 2026-10-11:
                 # they left tweet 1 for lack of room and moved here)
                 sprintf(es_en("%.0f%% del territorio promedia menos de 18 °C", "%.0f%% of the territory averages below 18 °C"),
                         100 * num$share_t_under18),
                 sprintf(es_en("%.0f%% promedia 24 °C o más", "%.0f%% averages 24 °C or more"),
                         100 * num$share_t_over24),
                 sprintf(es_en("Tierras a menos de 200 metros: %.1f °C", "Land below 200 metres: %.1f °C"), num$t_low),
                 sprintf(es_en("Tierras a más de 2,000 metros: %.1f °C", "Land above 2,000 metres: %.1f °C"), num$t_high),
                 sprintf(es_en("%.0f%% del territorio está a más de 1,000 metros", "%.0f%% of the territory lies above 1,000 metres"),
                         100 * num$share_1000))) +
    labs(title = es_en("En México la temperatura depende de la altura\nmucho más que de la latitud",
                       "In Mexico temperature depends on altitude\nfar more than on latitude"),
         subtitle = es_en("In Mexico temperature depends on altitude far more than on latitude · Temperatura promedio, 1991-2020",
                          "Average temperature, 1991-2020"),
         caption = caption_ls(
           notes = c(
             sprintf(es_en("La altura sobre el nivel del mar explica %.0f%% de las diferencias de temperatura en el país.",
                           "Height above sea level explains %.0f%% of the differences in temperature across the country."),
                     100 * num$r2_el),
             es_en("Azul, más frío que el promedio del país. Rojo, más caliente.",
                   "Blue, colder than the national average. Red, warmer."),
             es_en("Cada ciudad del mundo aparece con su temperatura promedio utilizando los mismos datos.",
                   "Each city of the world appears with its average temperature, using the same data."),
             NOTE_GRID, NOTE_MODEL),
           source = SOURCE, width = 210)) +
    map_frame(mode)
}

save_ls(make, "images/f1_temperature", width = FIG_W, height = FIG_H)
message("DONE!")
