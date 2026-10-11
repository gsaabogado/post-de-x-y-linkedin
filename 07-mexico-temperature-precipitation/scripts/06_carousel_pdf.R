# 06_carousel_pdf.R -------------------------------------------------------------
# LinkedIn carousel: the thread in stills, one page per tweet, in English
# (FIG_LANG=en renders of 02, 03 and 04). All three are 3840 x 2560, so every
# page of the PDF has the same size. Embedded lossless (Zip), so the small
# text stays sharp.
#
# Run from the post root after any English re-render: Rscript scripts/06_carousel_pdf.R
library(magick)

SLIDES <- c("f1_temperature_light_en", "f2_rainfall_light_en", "f3_temperature_rainfall_light_en")
OUT <- file.path("images", "carousel_en.pdf")

files <- file.path("images", paste0(SLIDES, ".png"))
stopifnot(all(file.exists(files)))
im <- image_read(files, density = "320x320")
info <- image_info(im)
stopifnot(length(unique(info$width)) == 1, length(unique(info$height)) == 1,
          all(abs(info$width / info$height - 1.5) < 0.001))
image_write(im, OUT, format = "pdf", compression = "Zip", density = "320x320")

out <- image_info(image_read_pdf(OUT, density = 10))
stopifnot(nrow(out) == length(SLIDES), length(unique(out$width)) == 1, length(unique(out$height)) == 1)
cat(OUT, ":", nrow(out), "pages of the same size\n")
