# 07 · Temperature and rainfall in Mexico

Three maps of Mexico's climate, 1991 to 2020, built to answer a question referees often ask: why study temperature in Mexico if it is such a hot country? It is not one climate. Some 28% of the territory averages below 18 °C and 21% averages 24 °C or more, 14% gets less than 250 mm of rain a year and another 14% gets more than 1,500.

- **Figure 1**, average daily temperature of the year against the national average (20.4 °C). Height above sea level explains 81% of the differences across the country.
- **Figure 2**, rain of an average year around 700 mm, the value that splits the territory in half, on a scale where each tick doubles the one before.
- **Figure 3**, both at once. Each place takes the colour of its position in a square of temperature against rainfall, and nine cities of the world are placed in the same square from their own averages in the same data.

Figures 1 and 2 also list those nine cities under the legend, each with the colour it would have on the map.

## Inputs

The climate is ERA5-Land (Copernicus Climate Change Service), 0.1 degrees, about 11 km. The raw daily fields are hundreds of gigabytes, so the averaging ran on a cluster and the repository carries its two small results:

- `in/mx_climatology_1991_2020.rds`, average temperature and yearly rainfall for a box around Mexico (`scripts/00a_climatology_cluster.R`).
- `in/world_cities_pool.rds`, the same two averages for 53 large cities of the world, each over the 5 x 5 cells around its centre (`scripts/00b_world_cities_cluster.R`).

The terrain (Terrain Tiles on AWS, Mapzen) and the national boundary (GADM 4.1) are not included. `scripts/00c_terrain_boundary.R` downloads both into `in/`.

## Run order

From the post folder:

```
Rscript scripts/00c_terrain_boundary.R   # terrain and boundary, once
Rscript scripts/01_fields.R              # relief and climate on one grid, about 80 s, 5 GB of memory
Rscript scripts/05_numbers.R             # every number quoted in the figures and texts
Rscript scripts/02_fig_temperature.R     # figure 1
Rscript scripts/03_fig_rainfall.R        # figure 2
Rscript scripts/04_fig_both.R            # figure 3
Rscript scripts/06_carousel_pdf.R        # LinkedIn carousel from the English figures
```

`FIG_LANG=en` in front of 02, 03 and 04 gives the English figures. `scripts/_map.R` holds what the three figures share: colours, the relief blend, the cities, the frame and the legend column. `07_web_figures.sh` writes the web copies of the figures.

Packages: terra, sf, rayshader, ggplot2, dplyr, tibble, ragg, magick, and elevatr and geodata for the download.

## Decisions

- **Shares of the territory.** Every share uses the true area of each cell, inside the national boundary only.
- **Altitude and latitude.** The 81% is the R² of a linear regression of temperature on height above sea level alone, cells weighted by area. Latitude alone gives 6%, and the two together 85%. It describes an association in these data.
- **Colour scales.** Temperature is drawn against the national average and rainfall against the median, both on the series' two-colour ramp. A single ramp left the temperature map all red and the rainfall map without contrast.
- **The square of figure 3.** Its axes rank Mexico's territory, so half of the country lies on each side of the middle of each axis.
- **A climate like a city's.** The areas quoted in the thread (50,000 km² like Phoenix, 26,000 like Paris, 65,000 like Hong Kong) count land within 1.5 °C of the city's average temperature and within 20% of its yearly rainfall, in the same data.
- **Relief.** Flat ground keeps the exact colour of the legend. Slopes turned away from the sun darken and slopes facing it lighten.

## Limits

ERA5-Land is a reanalysis: a climate model fed with measurements, not a set of station readings. At a given place it can differ from the station normal, mostly in rainfall. That is why the Mexican cities on the maps carry no number, and why Lima is not among the world cities: these data give it 243 mm of rain a year against about 6 mm at its airport. Madrid, which replaced it, is 14.3 °C and 466 mm here against 15.4 °C and 416 mm at Retiro (AEMET, 1991-2020). The other eight cities have not been compared with station records.

The colour changes every 11 km, the detail of the climate data. The relief is finer and only helps to place mountains, valleys and coasts. The shading depends on the version of rayshader (0.41.2 here).

A match with a city holds for the yearly average only. Temperature and rain can move very differently through the year.

## Credit

The two-variable relief map began as a version of Milos Popovic's temperature and precipitation map of the Iberian peninsula. His code for maps of this kind is at [github.com/milos-agathon/3d-bivariate-climate-maps](https://github.com/milos-agathon/3d-bivariate-climate-maps).
