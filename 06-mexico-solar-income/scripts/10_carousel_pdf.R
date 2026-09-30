# LinkedIn carousel: the four English slides as one PDF, one page per slide,
# in thread order. Each page keeps its PNG's own size (no crop, no padding),
# embedded lossless (Zip), so the small text stays sharp.
# Run from the post folder after any English re-render (FIG_LANG=en).
library(magick)

SLIDES <- c("f1_map_2025_light_en", "f2_deciles_2020_light_en",
            "f3_ac_2020_light_en", "f4_concentration_2020_light_en")
OUT <- file.path("images", "carousel_en.pdf")

im <- image_read(file.path("images", paste0(SLIDES, ".png")), density = "320x320")
image_write(im, OUT, format = "pdf", compression = "Zip", density = "320x320")

pages <- nrow(image_info(image_read_pdf(OUT, density = 10)))
stopifnot(pages == length(SLIDES))
cat(OUT, ":", pages, "pages\n")
