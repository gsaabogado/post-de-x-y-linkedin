# 30_carousel_pdf.R ------------------------------------------------------------
# LinkedIn carousel: the thread in stills, one page per tweet, in English
# (FIG_LANG=en renders of scripts 26 to 29). Embedded lossless (Zip) so the small
# text of the notes stays sharp.
#
# Run from the post root after any English re-render: Rscript scripts/30_carousel_pdf.R
library(magick)

SLIDES <- c("f1_adopcion_light_en", "f0_precio_tarifas_light_en", "f3_mismos_paneles_light_en",
            "f4_anios_recuperar_light_en", "f5_reparto_ingreso_light_en", "f6_peso_ingreso_light_en")
OUT <- file.path("images", "carousel_en.pdf")

files <- file.path("images", paste0(SLIDES, ".png"))
stopifnot(all(file.exists(files)))
im <- image_read(files, density = "320x320")
image_write(im, OUT, format = "pdf", compression = "Zip", density = "320x320")

pages <- nrow(image_info(image_read_pdf(OUT, density = 10)))
stopifnot(pages == length(SLIDES))
cat(OUT, ":", pages, "pages\n")
