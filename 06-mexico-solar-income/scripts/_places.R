# _places.R -------------------------------------------------------------------
# Cities called out on the map. Coordinates are city-centre points, used only
# to place a marker and a label. The rate printed next to each name is read
# from the survey (`loc` = NOM_LOC of a 50k+ locality, or `ent` = a state row
# for Mexico City, which the survey splits into alcaldias), never typed here.

CITIES <- dplyr::tibble(
  name = c("Monterrey", "Durango", "Zapopan", "Mérida", "Chihuahua",
           "Hermosillo", "CDMX"),
  loc  = c("Monterrey", "Victoria de Durango", "Zapopan", "Mérida", "Chihuahua",
           "Hermosillo", NA),
  ent  = c(NA, NA, NA, NA, NA, NA, "Ciudad de México"),
  lat  = c(25.6866, 24.0277, 20.7214, 20.9674, 28.6353, 29.0729, 19.4326),
  lon  = c(-100.3161, -104.6532, -103.3918, -89.5926, -106.0889, -110.9559, -99.1332)
)

# Label the cities with the survey's own rate.
city_rates <- function(cities, loc, states) {
  r_loc <- loc$pct[match(cities$loc, loc$nom)]
  r_ent <- states$pct[match(cities$ent, states$nom)]
  out <- dplyr::mutate(cities, pct = dplyr::coalesce(r_loc, r_ent),
                       code = sprintf("%s %s%%", name, format(round(pct, 1), nsmall = 1)))
  stopifnot(!anyNA(out$pct))
  out
}

# City markers with a background-coloured halo, so a label stays readable when
# it lands on top of a bright metro area. `ratio` is coord_fixed's y stretch,
# needed to keep the halo circular rather than oval.
city_layers <- function(cities, ratio, font, col, bg,
                        size = 2.6, dy = 0.42, halo = 0.075, alpha = 1) {
  off <- expand.grid(ox = -1:1, oy = -1:1)
  off <- off[!(off$ox == 0 & off$oy == 0), ]
  ring <- do.call(rbind, lapply(seq_len(nrow(off)), function(k) {
    transform(cities,
              hx = lon + off$ox[k] * halo,
              hy = lat + dy + off$oy[k] * halo / ratio)
  }))
  list(
    ggplot2::geom_point(data = cities, ggplot2::aes(x = lon, y = lat),
                        shape = 16, size = 2.6, colour = bg, alpha = alpha),
    ggplot2::geom_point(data = cities, ggplot2::aes(x = lon, y = lat),
                        shape = 21, size = 1.5, stroke = 0.5,
                        colour = col, fill = NA, alpha = alpha),
    ggplot2::geom_text(data = ring, ggplot2::aes(x = hx, y = hy, label = code),
                       colour = bg, family = font, size = size,
                       hjust = 0.5, vjust = 0, alpha = alpha),
    ggplot2::geom_text(data = cities,
                       ggplot2::aes(x = lon, y = lat + dy, label = code),
                       colour = col, family = font, size = size,
                       hjust = 0.5, vjust = 0, alpha = alpha)
  )
}

# The same halo treatment for a label parked at the end of a profile bar.
halo_text <- function(df, xcol, ycol, ratio, font, col, bg,
                      size = 2.9, hjust = 0, vjust = 0.5, halo = 0.075, alpha = 1) {
  off <- expand.grid(ox = -1:1, oy = -1:1)
  off <- off[!(off$ox == 0 & off$oy == 0), ]
  ring <- do.call(rbind, lapply(seq_len(nrow(off)), function(k) {
    transform(df,
              hx = df[[xcol]] + off$ox[k] * halo,
              hy = df[[ycol]] + off$oy[k] * halo / ratio)
  }))
  list(
    ggplot2::geom_text(data = ring, ggplot2::aes(x = hx, y = hy, label = code),
                       colour = bg, family = font, size = size,
                       hjust = hjust, vjust = vjust, alpha = alpha),
    ggplot2::geom_text(data = df,
                       ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]], label = code),
                       colour = col, family = font, size = size,
                       hjust = hjust, vjust = vjust, alpha = alpha)
  )
}
