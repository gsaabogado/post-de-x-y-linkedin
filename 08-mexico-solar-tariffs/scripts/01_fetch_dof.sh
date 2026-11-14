#!/bin/zsh
# 01_fetch_dof.sh --------------------------------------------------------------
# Downloads the official texts behind household electricity tariffs and rooftop
# solar compensation from the Diario Oficial de la Federación (sidof.segob.gob.mx)
# into data/raw/dof/, checks each file holds the expected title, and writes
# data/raw/dof/MANIFEST.csv (file, note id, url, bytes, md5, date).
#
# The server sends no Content-Length (compressed, chunked), so the check is on
# content: the file must contain its title and its closing signature line.
#
# CFE's own tariff pages (app.cfe.mx) answer "Access denied" to scripts and to a
# browser outside Mexico (2026-10-12); current monthly prices are NOT fetched here.
#
# Run from the post root: zsh scripts/01_fetch_dof.sh
set -euo pipefail
out=data/raw/dof
mkdir -p $out
ua="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/126.0 Safari/537.36"
# note id | short name | text the file must contain | text near its end
notes=(
  "5506179|acuerdo_123_2017_tarifas_domesticas|Acuerdo 123/2017|Rúbrica"
  "5547404|acuerdo_134_2018_factor_ajuste|Acuerdo 134|Rúbrica"
  "5777718|acuerdo_presidencial_2025-12-31|mecanismo de fijación de las tarifas finales|Rúbrica"
  "5474790|res_142_2017_generacion_distribuida|RES/142/2017|Precio Marginal Local"
  "5797487|tarifas_cne_cfe_2026-08|1 de agosto de 2026|DB2"
)
echo "file,note_id,url,bytes,md5,downloaded" > $out/MANIFEST.csv
for n in $notes; do
  id=${n%%|*}; rest=${n#*|}; name=${rest%%|*}; rest=${rest#*|}; must=${rest%%|*}; tail=${rest#*|}
  url="https://sidof.segob.gob.mx/notas/docFuente/$id"
  f=$out/${name}.html
  curl -sS --compressed -L -m 120 -A "$ua" -o $f "$url"
  grep -q "$must" $f || { echo "STOP: $f lacks '$must'"; exit 1; }
  grep -q "$tail" $f || { echo "STOP: $f looks truncated (no '$tail')"; exit 1; }
  echo "${name}.html,$id,$url,$(stat -f %z $f),$(md5 -q $f),$(date +%F)" >> $out/MANIFEST.csv
  echo "ok  ${name}.html  $(stat -f %z $f) bytes"
done
