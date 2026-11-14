#!/usr/bin/env bash
# 19_fetch_precios_instalacion.sh ---------------------------------------------------
# Public reports that print installed prices of PV systems in Mexico, for the yearly
# payback. Saves them to data/raw/costos/ and writes MANIFEST.csv (url, bytes, md5).
# Every value taken from them is transcribed, with page, in
# data/raw/costos/precios_instalados_transcritos.csv.
# Run from the post root: bash scripts/19_fetch_precios_instalacion.sh
set -euo pipefail
OUT=data/raw/costos
mkdir -p "$OUT"
IEA=https://iea-pvps.org/wp-content/uploads/2020/01
while read -r name url; do
  [ -z "$name" ] && continue
  want=$(curl -sIL -m 60 "$url" | tr -d '\r' | awk 'tolower($1)=="content-length:"{v=$2} END{print v}')
  curl -sL -C - -m 180 -o "$OUT/$name" "$url" || curl -sL -m 180 -o "$OUT/$name" "$url"
  got=$(stat -f %z "$OUT/$name")
  if [ -n "$want" ] && [ "$want" != "$got" ]; then echo "SIZE MISMATCH $name: $want vs $got" >&2; exit 1; fi
  head -c 4 "$OUT/$name" | grep -q %PDF || { echo "NOT A PDF: $name" >&2; exit 1; }
  echo "$name,$url,$got,$(md5 -q "$OUT/$name")"
done > "$OUT/MANIFEST.body" <<LIST
iea_trends_2007.pdf $IEA/tr_2007.pdf
iea_trends_2008.pdf $IEA/tr_2008.pdf
iea_trends_2009.pdf $IEA/tr_2009_neu.pdf
iea_trends_2010.pdf $IEA/tr_2010_low.pdf
iea_trends_2011.pdf $IEA/tr_2011.pdf
iea_nsr_mexico_2010.pdf $IEA/nsr_2010_MEX.pdf
abm_mercado_fv_baja_escala_2017.pdf https://www.abm.org.mx/descargas/Paneles_Solares_2017.pdf
asolmex_monitor_precios_2020.pdf https://asolmex.org/Micrositio_GSD/Monitor-Indice-de-Precios/MonitorIndicePreciosGSD_Mex_13032020.pdf
asolmex_monitor_precios_2021.pdf https://asolmex.org/intranet/Micrositio_GSD/Monitor-Indice-de-Precios/Monitor_precios_GSD_reporte2aedicion_oct2021.pdf
imco_paneles_solares_2019.pdf https://imco.org.mx/wp-content/uploads/2019/11/130319_EnergiaSolar_Documento.pdf
creara_grid_parity_monitor_mexico_2015.pdf https://www.solareb2b.it/newsletter/pv_grid_parity_monitor_mexico.pdf
LIST
{ echo "archivo,url,bytes,md5"; cat "$OUT/MANIFEST.body"; } > "$OUT/MANIFEST.csv"
rm "$OUT/MANIFEST.body"
cat "$OUT/MANIFEST.csv"
