# 09_carousel_pdf.R ------------------------------------------------------------
# LinkedIn carousel: the thread in stills, one page per tweet, in English
# (FIG_LANG=en renders). F1, F2 and the last frame of the video. The frame is
# 2880 x 1920 and the stills 3840 x 2560, the same 3:2, so it is scaled up to
# their size and every page of the PDF has the same size. Embedded lossless
# (Zip), so the small text stays sharp.
#
# Run from the post root after any English re-render: Rscript scripts/09_carousel_pdf.R
library(magick)

SLIDES <- c("f1_fiscal_profiles_2024_light_en", "f2_fiscal_totals_2024_light_en",
            "f3_projection_2075_light_en")
OUT <- file.path("images", "carousel_en.pdf")

files <- file.path("images", paste0(SLIDES, ".png"))
stopifnot(all(file.exists(files)))
im <- image_read(files, density = "320x320")
info <- image_info(im)
stopifnot(all(abs(info$width / info$height - 1.5) < 0.001))
im <- image_resize(im, sprintf("%dx%d!", max(info$width), max(info$height)), filter = "Lanczos")
image_write(im, OUT, format = "pdf", compression = "Zip", density = "320x320")

out <- image_info(image_read_pdf(OUT, density = 10))
stopifnot(nrow(out) == length(SLIDES), length(unique(out$width)) == 1, length(unique(out$height)) == 1)
cat(OUT, ":", nrow(out), "pages of the same size\n")
