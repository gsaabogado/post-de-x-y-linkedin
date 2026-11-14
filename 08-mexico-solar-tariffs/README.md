# 08 · Electricity subsidies and solar panels in Mexico

Six figures on whether Mexico's subsidised household electricity tariffs hold back rooftop solar. Only 1.5% of Mexican homes have panels. In December 2025, 99.9% of CFE's residential customers were on one of the seven subsidised tariffs, and the average home on the most common one would need 48 years to recover the cost of the smallest system installers sell (16 at the estimated unsubsidised tariff). The subsidy depends on climate and consumption, not on income.

- **Figure 1**, homes with panels by income decile (urban homes, Censo 2020), with reference lines for the United States, Germany, Brazil and Switzerland (`scripts/28_fig_adopcion.R`).
- **Figure 2**, average price per kWh by tariff, summer and the rest of the year, against the estimated unsubsidised tariff and US border states (`scripts/27_precio_por_tarifa.R`).
- **Figure 3**, years to recover a four-panel system (2.4 kW) by tariff, at today's tariff and with no subsidy (`scripts/29_fig_mismos_paneles.R`).
- **Figures 4 to 6**, homes by years to pay back, the subsidy by income decile, and the subsidy as a share of income (`scripts/26_fig_tarifa_costo.R`).

## Inputs

Most inputs are public and downloaded by the scripts: tariff agreements in the Diario Oficial (01, 08), the regulator's cost-based tariffs (03), CFE's old tariff pages in the Wayback Machine (10), reports with installed prices (19), EIA-861 and the peso-dollar rate (22), and the budget decrees (31). Three cases need more:

- **CFE's tariff pages** (app.cfe.mx) refuse scripted clients and connections from outside Mexico. `04_cfe_extract.js` runs in a browser console on those pages, from Mexico or through a VPN with a Mexican exit, and `09_verify_cfe_raw.js` re-checks every reading against its hash. The Diario Oficial and Hacienda also refuse some foreign connections.
- **Local files**, read from environment variables in a `.Renviron` inside the post folder: `MLPV_IN` (INEGI Censo 2020 and Encuesta Intercensal 2025, CFE customers and consumption by municipality 2010-2017, Ember's panel exports from China), `CFE_MUN_DIR` (CFE's quarterly statistic Usuarios y Consumos por Municipio, 2020-2025, from the national transparency platform), `INPC_CSV` (consumer prices) and `ENIGH_HH` (ENIGH 2024 households). Some scripts also reuse outputs of posts 01, 04 and 06.
- **Three small tables made by hand**, included in `data/raw/`: installed prices transcribed from the reports (`costos/precios_instalados_transcritos.csv`, with `costos/MANIFEST.csv` listing each report and its checksum), the packages seven Mexican installers publish (`costos/paquetes_instaladores_2026.csv`) and the share of homes with panels in the four reference countries, each with its source (`paises/hogares_con_paneles.csv`).

## Run order

From the post folder: 01, 02, 03, 05, 06, 07, 08, 10, 11, 12, 13, 14, 17, then 18, 19, 19b, 19c, 19d, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31 and 32. `FIG_LANG=en` in front of 26 to 29 gives the English figures, and 30 builds the LinkedIn carousel from them. `_fuentes.R` holds the source lines the figures share.

Packages: dplyr, readr, tidyr, purrr, stringr, tibble, ggplot2, readxl, sf, terra, rmapshaper, httr2, jsonlite and magick.

## Decisions

- **Estimated unsubsidised tariff.** The regulator's (CNE) cost-based household tariffs DB1 and DB2, September 2026, median of CFE's 17 divisions, with tax. DB1 applies up to 150 kWh a month and DB2 above.
- **Consumption.** Electricity spending in ENIGH 2024, scaled to CFE's mean consumption by tariff, since reported spending runs 20% to 35% above CFE's metered kWh. Each home gets the main tariff of its municipality. Each home keeps its consumption at any price.
- **Systems.** Whole panels from four (2.4 kW), the smallest package installers usually sell, priced from published packages and a 2021 price index updated with the price of panels China exports to Mexico. The 2026 installed price is an estimate.
- **Payback.** System cost over yearly savings, with net metering (credits for 12 months), no interest, battery, maintenance or panel wear, and the generation of each tariff's municipalities from the Global Solar Atlas.
- **Estimated subsidy.** Yearly bill at the estimated unsubsidised tariff minus the bill at today's tariff. It is our calculation, not the fiscal subsidy: the federal budget line "Subsidios a las Tarifas Eléctricas" was 84,805.5 million pesos in 2025, which CFE reports having received.

## Limits

The calculations are illustrative. Homes would use less power at higher prices, which the estimates ignore. Survey consumption is scaled to CFE's averages by tariff, not observed per home. Without a battery, panels give no protection against blackouts.
