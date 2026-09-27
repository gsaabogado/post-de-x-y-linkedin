# _raster.R -------------------------------------------------------------------
# Turn a raster into a ready-coloured image.
#
# geom_raster hands the device one rectangle per cell, so when the panel gives
# the map fewer pixels than the raster has columns the device drops cells and
# the country looks chewed up. Here terra resamples onto the pixel grid the
# panel will actually have, averaging rather than discarding, and the result is
# drawn as a single image with annotation_raster.
#
# `transform` maps raw cell values onto the scale the colour ramp runs over, and
# `limits` are given ON THAT SCALE. This post uses log2 of income relative to
# the national average, so the middle of the ramp is exactly the country mean.

library(terra)

# `mask_sf` crops the grid to a country outline. The raster is first split into
# `crop_fact` sub-pixels by NEAREST NEIGHBOUR, so no cell value changes and the
# interior keeps its honest 25 km blocks; only the edge gets cut finely enough
# to trace a coastline.
map_image <- function(raster_path, ncol_px = NULL, ramp, limits,
                      transform = log10, alpha = 1,
                      mask_sf = NULL, crop_fact = 20) {
  r <- if (inherits(raster_path, "SpatRaster")) raster_path else rast(raster_path)
  e <- ext(r)

  if (!is.null(mask_sf)) {
    # terra::vect() has no method for a bare sfc, so promote it to sf first.
    poly <- if (inherits(mask_sf, "SpatVector")) mask_sf else vect(sf::st_as_sf(mask_sf))
    r <- mask(disagg(r, crop_fact, method = "near"), poly)
  }

  if (!is.null(ncol_px) && ncol_px != ncol(r)) {
    aspect <- (e$ymax - e$ymin) / (e$xmax - e$xmin)
    target <- rast(e, ncols = ncol_px, nrows = round(ncol_px * aspect),
                   crs = crs(r))
    r <- resample(r, target, method = "average")
  }

  v <- matrix(values(r, mat = FALSE), nrow = nrow(r), ncol = ncol(r), byrow = TRUE)

  z <- transform(v)
  z <- pmin(pmax(z, limits[1]), limits[2])
  t <- (z - limits[1]) / (limits[2] - limits[1])
  pal <- grDevices::colorRampPalette(ramp, space = "Lab")(512)
  img <- matrix(pal[1 + round(t * 511)], nrow(v), ncol(v))

  if (alpha < 1) {
    img[] <- paste0(img, sprintf("%02X", as.integer(round(alpha * 255))))
  }
  img[is.na(v) | v <= 0] <- "#00000000"

  list(img = img, xmin = e$xmin, xmax = e$xmax, ymin = e$ymin, ymax = e$ymax,
       ncol = ncol(r), nrow = nrow(r))
}

# annotation_raster layer straight from map_image(). interpolate = FALSE keeps
# a coarse grid honest, showing its own cells rather than a smooth invention.
map_layer <- function(m, interpolate = TRUE) {
  ggplot2::annotation_raster(m$img, xmin = m$xmin, xmax = m$xmax,
                             ymin = m$ymin, ymax = m$ymax,
                             interpolate = interpolate)
}
