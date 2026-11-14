#!/usr/bin/env bash
# 22_fetch_eia861_fx.sh -------------------------------------------------------------
# US utility data for the border pairs, and the peso-dollar rate.
#   EIA-861 2020 and 2024 (net metering, sales to ultimate customers) -> data/raw/eia861/
#   FRED EXMXUS, monthly                                              -> data/raw/fx/
# Each zip is checked byte for byte against Content-Length. MANIFEST.csv records
# url, bytes and md5.
# Run from the post root: bash scripts/22_fetch_eia861_fx.sh
set -euo pipefail
OUT=data/raw/eia861
mkdir -p "$OUT" data/raw/fx
UA="Mozilla/5.0"
echo "file,url,bytes,md5" > "$OUT/MANIFEST.csv"
while read -r year url; do
  f="$OUT/f861$year.zip"
  want=$(curl -sIL -A "$UA" "$url" | tr -d '\r' | awk 'tolower($1)=="content-length:"{v=$2} END{print v}')
  [ -n "$want" ] || { echo "no Content-Length for $url" >&2; exit 1; }
  curl -sL -A "$UA" -o "$f" "$url"
  got=$(stat -f %z "$f")
  if [ "$got" != "$want" ]; then curl -sL -C - -A "$UA" -o "$f" "$url"; got=$(stat -f %z "$f"); fi
  [ "$got" = "$want" ] || { echo "$f: $got bytes, expected $want" >&2; exit 1; }
  mkdir -p "$OUT/$year"
  unzip -o -q -j "$f" "Net_Metering_$year.xlsx" "Sales_Ult_Cust_$year.xlsx" -d "$OUT/$year"
  echo "f861$year.zip,$url,$got,$(md5 -q "$f")" >> "$OUT/MANIFEST.csv"
done <<EOF
2020 https://www.eia.gov/electricity/data/eia861/archive/zip/f8612020.zip
2024 https://www.eia.gov/electricity/data/eia861/zip/f8612024.zip
EOF
curl -sL "https://fred.stlouisfed.org/graph/fredgraph.csv?id=EXMXUS&cosd=2020-01-01" -o data/raw/fx/EXMXUS.csv
head -1 data/raw/fx/EXMXUS.csv | grep -q EXMXUS || { echo "bad FX file" >&2; exit 1; }
cat "$OUT/MANIFEST.csv"; tail -1 data/raw/fx/EXMXUS.csv
