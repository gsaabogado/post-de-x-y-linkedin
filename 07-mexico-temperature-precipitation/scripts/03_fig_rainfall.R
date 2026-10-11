# 03_fig_rainfall.R -------------------------------------------------------------
# Figure F2 (tweet 2): rain of an average year, 1991-2020, in the series style.
# Colour is the series' diverging ramp around the rainfall that splits the
# territory in half (the median): brick red for drier land, slate blue for
# wetter land, on a doubling scale (each tick is twice the one before), as the
# rate map of post 06.
#
# A single ramp from pale to slate on a square-root scale was the first version
# and Luis rejected it (2026-10-11: "no tiene suficiente contraste, todo se ve
# igual"): most of the country fell in a narrow band of mid tones.
#
# City labels carry no number: the value of an 11 km cell differs from the
# station figure people know for their city (see 05_numbers.R for the values).
#
# Run from the post root: Rscript scripts/03_fig_rainfall.R   (FIG_LANG=en for English)

source("scripts/_map.R")

num <- readRDS("data/derived/numbers.rds")
P_MID <- 700                             # mm; the centre of the scale, next to the median
SPAN <- 2                                # doublings each side: 175 to 2,800 mm
stopifnot(abs(num$p_q[["p50"]] / P_MID - 1) < 0.02,
          share_below(P_MID * 2^SPAN, FLD$q_p) > 0.99)
pos <- function(v) (pmin(pmax(log2(pmax(v, 1) / P_MID), -SPAN), SPAN) + SPAN) / (2 * SPAN)
num_fmt <- function(v) format(v, big.mark = ",", trim = TRUE)
# The figure quotes the centre of the scale: the median is 694 mm, so "half of
# the country gets less than 700" holds and the title agrees with the legend.
median_mm <- P_MID
stopifnot(share_below(P_MID, FLD$q_p) >= 0.5, share_below(P_MID, FLD$q_p) < 0.51)
cities <- mutate(cities_xy(), code = name)
TICKS <- P_MID * 2^(-SPAN:SPAN)
# World cities under the legend, with the colour each would have on the map,
# as in the temperature map (Luis, 2026-10-11). Rounded to 10 mm.
world <- world_cities() |>
  mutate(value = ppt, value_lab = sprintf("%s mm", num_fmt(round(ppt, -1))))

make <- function(mode) {
  p <- pal(mode)
  colours <- rev(p$div)                  # dry in brick red, wet in slate blue
  img <- relief_image(ramp_channels(pos(FLD$ppt[LAND]), colours))
  base_map(img, p) +
    city_layers(cities, p) +
    bar_layers(p, colours, pos, ticks = TICKS, tick_lab = num_fmt(TICKS),
               title = es_en("Lluvia de un año promedio", "Rain in an average year"),
               sub = es_en("Milímetros al año, de 1991 a 2020", "Millimetres a year, 1991 to 2020"),
               mark = P_MID,
               mark_lab = sprintf(es_en("La mitad del país recibe menos de %s mm", "Half of the country gets less than %s mm"),
                                  num_fmt(median_mm)),
               world = world,
               world_head = es_en("Ciudades del mundo, en los mismos datos", "Cities of the world, in the same data"),
               facts = c(
                 sprintf(es_en("%.0f%% del territorio recibe menos de 500 mm", "%.0f%% of the territory gets less than 500 mm"),
                         100 * num$share_p_under500),
                 sprintf(es_en("La cuarta parte más lluviosa del territorio\nrecibe %.0f%% de la lluvia", "The wettest quarter of the territory\nreceives %.0f%% of the rain"),
                         100 * num$vol_wettest25),
                 sprintf(es_en("La mitad más seca recibe %.0f%%", "The driest half receives %.0f%%"),
                         100 * num$vol_driest50))) +
    labs(title = sprintf(es_en("La mitad de México recibe menos de\n%s milímetros de lluvia al año",
                               "Half of Mexico gets less than\n%s millimetres of rain a year"), num_fmt(median_mm)),
         subtitle = es_en(
           sprintf("Half of Mexico gets less than %s millimetres of rain a year · Lluvia promedio, 1991-2020",
                   num_fmt(median_mm)),
           sprintf("Average rainfall, 1991-2020 (%s mm is about %.0f inches)", num_fmt(median_mm), median_mm / 25.4)),
         caption = caption_ls(
           notes = c(
             es_en("El mapa muestra el agua que cae en un año promedio, casi toda como lluvia. Un milímetro equivale a un litro de agua por metro cuadrado.",
                   "The map shows the water that falls in an average year, nearly all of it as rain. One millimetre equals one litre of water per square metre."),
             es_en("Rojo, lugares que reciben menos lluvia que la mitad del país. Azul, lugares que reciben más. Cada marca de la escala es el doble de la anterior.",
                   "Red, places that get less rain than half of the country. Blue, places that get more. Each tick of the scale is twice the one before."),
             es_en("Cada ciudad del mundo aparece con su lluvia promedio utilizando los mismos datos.",
                   "Each city of the world appears with its average rainfall, using the same data."),
             NOTE_GRID, NOTE_MODEL),
           source = SOURCE, width = 210)) +
    map_frame(mode)
}

save_ls(make, "images/f2_rainfall", width = FIG_W, height = FIG_H)
message("DONE!")
