# 04 · Where Mexico's GDP is produced

Mexico's GDP folded onto latitude and longitude, first in total and then per person, plus a Lorenz curve of GDP between places. Money is in pesos of August 2026, with dollars at the August 2026 average exchange rate.

## Inputs

- `RH_GDP_FEATHER` in `04-mexico-gdp-marginals/.Renviron`, the path to a feather file with the Rossi-Hansberg and Zhang (2025) gridded GDP, version 2, 0.25 degrees (NBER Working Paper 33458).
- The WorldPop 2025 1 km grid for Mexico, downloaded by `../02-mexico-population-latitude/scripts/00_download_worldpop.R`.
- `10_fx.R` downloads the peso-dollar rate from FRED (series EXMXUS).

## Run order

`01_build_grids.R`, `04_dasymetric.R` (spreads 0.25 degree GDP over the 1 km population grid), `10_fx.R`, then `05_fig_v2.R` (F1), `06_fig_lorenz.R` (F2) and `08_animate_v2.R` (the two videos, `VIDEO=total` and `VIDEO=per_person`). The videos need ffmpeg.
