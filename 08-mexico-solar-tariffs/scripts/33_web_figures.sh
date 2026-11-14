#!/bin/sh
# 33_web_figures.sh -------------------------------------------------------------
# Web copies of the six thread figures for the post's page on luissarmiento.com,
# Spanish and English, webp, 1600 px wide (the size of the other posts' figures).
#
# Run from the post root after any re-render, with the site's figure folder:
#   SITE_IMAGES=<site>/public/images/data sh scripts/33_web_figures.sh
set -eu
: "${SITE_IMAGES:?set SITE_IMAGES to the public/images/data folder of the site}"
[ -d "$SITE_IMAGES" ] || { echo "no such folder: $SITE_IMAGES" >&2; exit 1; }

n=0
for stem in f1_adopcion f0_precio_tarifas f3_mismos_paneles f4_anios_recuperar f5_reparto_ingreso f6_peso_ingreso; do
  n=$((n + 1))
  cwebp -quiet -q 82 -resize 1600 0 "images/${stem}_light.png" -o "$SITE_IMAGES/tariffs-f${n}-es.webp"
  cwebp -quiet -q 82 -resize 1600 0 "images/${stem}_light_en.png" -o "$SITE_IMAGES/tariffs-f${n}-en.webp"
done
ls -l "$SITE_IMAGES"/tariffs-*.webp
