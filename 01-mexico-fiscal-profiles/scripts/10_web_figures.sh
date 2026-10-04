#!/bin/sh
# 10_web_figures.sh -------------------------------------------------------------
# Web copies of the figures for the post's page on luissarmiento.com: the four
# stills in Spanish and English as webp, 1600 px wide (the size of the other
# posts' figures there). F3 is the last frame of the video. F4 is not in the
# thread and appears on the page only.
#
# Run from the post root after any re-render, with the site's figure folder:
#   SITE_IMAGES=<site>/public/images/data sh scripts/10_web_figures.sh
set -eu
: "${SITE_IMAGES:?set SITE_IMAGES to the public/images/data folder of the site}"
[ -d "$SITE_IMAGES" ] || { echo "no such folder: $SITE_IMAGES" >&2; exit 1; }

for pair in f1_fiscal_profiles_2024:f1 f2_fiscal_totals_2024:f2 f3_projection_2075:f3 f4_crossing_2075:f4; do
  stem=${pair%%:*}; short=${pair##*:}
  cwebp -quiet -q 82 -resize 1600 0 "images/${stem}_light.png" -o "$SITE_IMAGES/fiscal-${short}-es.webp"
  cwebp -quiet -q 82 -resize 1600 0 "images/${stem}_light_en.png" -o "$SITE_IMAGES/fiscal-${short}-en.webp"
done
ls -l "$SITE_IMAGES"/fiscal-*.webp
