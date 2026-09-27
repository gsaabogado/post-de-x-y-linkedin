# Post de X y LinkedIn

Code behind the data threads by Luis Sarmiento (ETH Zürich, CMCC) on X ([@LSarmientoEcon](https://x.com/LSarmientoEcon)) and LinkedIn. Each folder is one post and runs on its own. The figures are built in R, and every number in them is computed by these scripts.

Código detrás de los hilos con datos de Luis Sarmiento en X y LinkedIn. Cada carpeta es un post y corre por sí sola.

| Folder | Post | Data |
| --- | --- | --- |
| `04-mexico-gdp-marginals` | Where Mexico's GDP is produced, folded onto latitude and longitude, then per person | Rossi-Hansberg and Zhang (2025) gridded GDP, WorldPop 2025, INEGI, FRED |
| `06-mexico-solar-income` | Who has solar panels in Mexico, by place and by income | INEGI Encuesta Intercensal 2025, Censo 2020 (extended questionnaire), CFE |
| `02-mexico-population-latitude` | Only the WorldPop download that posts 04 and 06 reuse | WorldPop 2025, 1 km |

## How to run

Run every script from its post folder, in the order given in that folder's README, for example `cd 06-mexico-solar-income && Rscript scripts/02_map.R`. All figure scripts load the shared style from `_style/style.R`. They write Spanish figures by default, and `FIG_LANG=en` writes the English versions used on LinkedIn.

The repository holds code only. Raw and derived data are not included, because they are large and most of them are free to download from the source. Each post's README lists the inputs and where to get them. Input locations are read from environment variables, which you can set in a `.Renviron` file inside the post folder.

## License

Code under the MIT License. The data belong to their providers and follow their terms.
