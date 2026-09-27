# 06 · Solar panels and income in Mexican homes

Who has solar panels in Mexico. The share of homes with a panel nearly doubled between 2020 and 2025, and the 2020 census microdata show that panels concentrate at the top of the income distribution and in homes with air conditioning.

## Inputs

Set `MLPV_IN` in `06-mexico-solar-income/.Renviron` to a folder that holds these files, with this layout:

- `Census/intercensal2025/raw/conjunto_de_datos_eic2025_105_csv.zip`, INEGI Encuesta Intercensal 2025, municipal table 105 (dwellings with a solar panel).
- `Census/censo2020_ca/raw/Censo2020_CA_eum_csv.zip`, INEGI Censo de Población y Vivienda 2020, extended questionnaire microdata.
- `Census/derived/panel_shares_state_national_2015_2020.csv`, national shares for 2015 and 2020 (the script falls back to published values if it is missing).
- `shp/agem/00mun.shp`, INEGI Marco Geoestadístico, municipalities.
- `Electricity/cfe_users_consumption_mun/cfe_users_consumption_mun_2010_2017.csv`, CFE customers by municipality and tariff (script 09 only).

The map also needs the WorldPop 2025 1 km grid, downloaded by `../02-mexico-population-latitude/scripts/00_download_worldpop.R`.

## Run order

`01_build_data.R`, `02_map.R` (F1), `03_build_income.R`, `04_fig_deciles.R` (F2), `05_build_ac.R`, `06_fig_ac.R` (F3), `07_build_concentration.R`, `08_fig_concentration.R` (F4). `09_dac_vs_panel.R` computes the numbers on the full-price tariff (DAC) quoted in the thread.
