#!/bin/sh
# 07_web_figures.sh -------------------------------------------------------------
# Web copies of the figures for the post's page on luissarmiento.com: the three
# maps in Spanish and English as webp, 1600 px wide (the size of the other
# posts' figures there).
#
# Run from the post root after any re-render, with the site's figure folder:
#   SITE_IMAGES=<site>/public/images/data sh scripts/07_web_figures.sh
set -eu
: "${SITE_IMAGES:?set SITE_IMAGES to the public/images/data folder of the site}"
[ -d "$SITE_IMAGES" ] || { echo "no such folder: $SITE_IMAGES" >&2; exit 1; }

for pair in f1_temperature:f1 f2_rainfall:f2 f3_temperature_rainfall:f3; do
  stem=${pair%%:*}; short=${pair##*:}
  cwebp -quiet -q 82 -resize 1600 0 "images/${stem}_light.png" -o "$SITE_IMAGES/climate-${short}-es.webp"
  cwebp -quiet -q 82 -resize 1600 0 "images/${stem}_light_en.png" -o "$SITE_IMAGES/climate-${short}-en.webp"
done
ls -l "$SITE_IMAGES"/climate-*.webp
