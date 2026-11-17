# 09 · Children per woman in Mexico, by municipality

A map of children per woman (total fertility rate) in each of Mexico's 2,478 municipalities, from INEGI's Encuesta Intercensal 2025 (births of 2024), and a figure that sets it against four features of each municipality. 97.7% of the population lives in municipalities below 2.1 children per woman, and only 235 municipalities, with 2.3% of the population, are at 2.1 or more.

- **Figure 1**, children per woman by municipality in nine classes centred on 2.1, red below and blue above (`scripts/05_fig_map.R`). The 7 municipalities without an estimate take the mean of the municipalities within 500 m, on the map only.
- **Figure 2**, children per woman against women's schooling, poverty, the share living in places under 2,500 people and the share who consider themselves indigenous, one dot per municipality sized by women aged 15 to 49, with a weighted local regression (`scripts/07_fig_correlates.R`).

## Inputs

All inputs are public. Their locations are read from environment variables in a `.Renviron` inside the post folder:

- `EIC_ZIP`: INEGI, Encuesta Intercensal 2025, "Principales resultados por localidad de 50 000 y más habitantes" (file 105, which also holds the municipal rows with `TGF`, `GRAPROES_F` and `PCN_POB_IND`).
- `MG_ZIP`: INEGI, Marco Geoestadístico, Encuesta Intercensal 2025 (municipal and state boundaries).
- `CONEVAL_DIR`: CONEVAL, municipal poverty 2020.
- `CENSO20_CSV`: INEGI, Censo de Población y Vivienda 2020, principales resultados por localidad (for the rural share).
- `SINAC_DIR`, `SINAC25_CSV`: Secretaría de Salud (DGIS, datos abiertos), birth certificates 2024 and 2025. Used only for the comparison in script 03, not for the figures.
- `SHP_DIR`: metropolitan zones of 2010, also only for script 03.

## Run order

From the post folder: 01, 04, 05, 06, 07 for the figures (02 and 03 for the comparison with birth certificates). `FIG_LANG=en` in front of 05 and 07 gives the English figures, 08 builds the LinkedIn carousel from them and 09 the web copies.

Packages: tidyverse, sf and magick.

## Decisions

- **Source of the rate.** The Intercensal's own `TGF`, not births from certificates over women from the survey. Certificates give 1.34 for the country against the survey's 1.23, read higher in the big cities and much lower in the Chiapas highlands (possibly births at home without a certificate).
- **Classes.** Breaks at 0.8, 1.0, 1.2, 1.4, 1.7, 2.1, 2.5 and 3.0, centred on the replacement level 2.1. No smoothing.
- **Correlations.** Weighted by women aged 15 to 49. Poverty and the rural share are from 2020, so the 12 municipalities created since then are missing from those panels.

## Limits

The survey visited every home in 750 municipalities and a sample in the rest, so small municipalities carry sampling error. The correlations are associations between places, not effects.
