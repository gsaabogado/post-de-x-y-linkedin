# _places.R -------------------------------------------------------------------
# The four cities the animation calls out. Coordinates are city-centre points,
# used only to place a marker and a label, never to compute any statistic.

CITIES <- dplyr::tibble(
  code = c("CDMX", "GDL", "MTY", "TIJ"),
  name = c("Mexico City", "Guadalajara", "Monterrey", "Tijuana"),
  lat  = c(19.4326, 20.6597, 25.6866, 32.5149),
  lon  = c(-99.1332, -103.3496, -100.3161, -117.0382)
)

# Where each city's own band sits on a profile, so a label can be parked just
# past the end of that band's bar.
city_on <- function(prof, values, base, scale, bin) {
  band <- floor(values / bin) * bin
  prof$per_degree[match(band, prof$band)] * scale + base
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
