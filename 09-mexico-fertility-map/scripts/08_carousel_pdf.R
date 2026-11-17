# 08_carousel_pdf.R ------------------------------------------------------------
# LinkedIn carousel: the thread's figures in stills, in thread order, in English
# (FIG_LANG=en renders of scripts 05 and 07). Embedded lossless (Zip) so the small
# text of the notes stays sharp.
#
# Run from the post root after any English re-render: Rscript scripts/08_carousel_pdf.R
library(magick)

SLIDES <- c("f1_map_light_en", "f2_correlates_light_en")
OUT <- file.path("images", "carousel_en.pdf")

files <- file.path("images", paste0(SLIDES, ".png"))
stopifnot(all(file.exists(files)))
im <- image_read(files, density = "320x320")
image_write(im, OUT, format = "pdf", compression = "Zip", density = "320x320")

pages <- nrow(image_info(image_read_pdf(OUT, density = 10)))
stopifnot(pages == length(SLIDES))
cat(OUT, ":", pages, "pages\n")
