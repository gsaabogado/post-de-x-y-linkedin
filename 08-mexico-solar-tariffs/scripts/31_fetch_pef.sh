#!/bin/sh
# 31_fetch_pef.sh --------------------------------------------------------------------
# Federal budget decrees and Cuenta Pública 2025, with the line "Subsidios a las Tarifas Eléctricas" (Anexo 20,
# Ramo 23) -> data/raw/pef/. Needs the Mexican VPN: Diputados and the DOF refuse other
# connections. Checks each file against Content-Length.
# Run from the post root: sh scripts/31_fetch_pef.sh
set -eu
OUT=data/raw/pef; mkdir -p "$OUT"
get() {
  want=$(curl -sIL -m 40 "$1" | tr -d '\r' | awk 'tolower($1)=="content-length:"{v=$2} END{print v}')
  curl -sL -m 280 -C - -o "$OUT/$2" "$1"
  got=$(stat -f %z "$OUT/$2")
  [ "$want" = "$got" ] || { echo "size mismatch $2: $got of $want"; exit 1; }
  echo "ok $2 $got"
}
get https://www.diputados.gob.mx/LeyesBiblio/pdf/PEF_2026.pdf PEF_2026.pdf
get https://www.diputados.gob.mx/LeyesBiblio/abro/pef_2025/PEF_2025_abro.pdf PEF_2025.pdf
# Cuenta Pública 2025: Ramo 23 analysis (no programme line) and CFE's notes (subsidy received)
CP=https://www.cuentapublica.hacienda.gob.mx/work/models/CP/2025/tomo
get $CP/III/MAT_Print.23R23.03.AEPE.pdf CP2025_R23_AEPE.pdf
get $CP/VIII/53TVV.02.NEF.doc CP2025_53TVV_NEF.doc
get $CP/VIII/53UIR.02.NEF.doc CP2025_53UIR_NEF.doc
