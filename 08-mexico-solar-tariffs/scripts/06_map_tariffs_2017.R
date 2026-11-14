# 06_map_tariffs_2017.R ---------------------------------------------------------
# Household customers by municipality and tariff in 2017 (CFE open data, the last
# year published, kept in the MLPV project): for each municipality the tariff that
# holds most household customers, the share of each tariff and the share on DAC.
# Joined to INEGI's municipal polygons (Marco Geoestadístico, December 2021),
# simplified for the web.
#
# Writes data/derived/mun_tarifas_2017.csv (one row per municipality) and
#        data/derived/mun_tarifas_2017.geojson (simplified polygons, WGS84).
#
# Needs MLPV_IN in .Renviron. Run from the post root: Rscript scripts/06_map_tariffs_2017.R

library(dplyr)
library(readr)
library(tidyr)
library(sf)
library(rmapshaper)

mlpv <- path.expand(Sys.getenv("MLPV_IN"))
if (!nzchar(mlpv)) stop("Set MLPV_IN in .Renviron")

HOME_TARIFFS <- c("01", "1A", "1B", "1C", "1D", "1E", "1F", "DAC")

cfe <- read_csv(file.path(mlpv, "Electricity/cfe_users_consumption_mun/cfe_users_consumption_mun_2010_2017.csv"),
                col_types = cols(cvegeo = "c", tarifa = "c", cve_ent = "c", cve_mun = "c",
                                 entidad = "c", municipio = "c", .default = "d")) |>
  filter(year == 2017, tarifa %in% HOME_TARIFFS)

mun <- cfe |>
  group_by(cvegeo, entidad, municipio) |>
  summarise(clientes = sum(users),
            kwh = sum(kwh),
            clientes_dac = sum(users[tarifa == "DAC"]),
            kwh_dac = sum(kwh[tarifa == "DAC"]),
            tarifa_principal = tarifa[tarifa != "DAC"][which.max(users[tarifa != "DAC"])],
            pct_tarifa_principal = 100 * max(users[tarifa != "DAC"]) / sum(users[tarifa != "DAC"]),
            .groups = "drop") |>
  filter(clientes > 0) |>
  mutate(tarifa_principal = sub("^01$", "1", tarifa_principal),
         pct_dac = 100 * clientes_dac / clientes,
         kwh_mes_por_cliente = kwh / clientes / 12)
stopifnot(!anyNA(mun$tarifa_principal), !any(duplicated(mun$cvegeo)))

nat <- cfe |> group_by(tarifa) |> summarise(clientes = sum(users), kwh = sum(kwh), .groups = "drop") |>
  mutate(pct_clientes = 100 * clientes / sum(clientes), pct_kwh = 100 * kwh / sum(kwh),
         kwh_mes_por_cliente = kwh / clientes / 12)
print(nat)
write_csv(nat, "data/derived/nacional_tarifas_2017.csv")
write_csv(mun, "data/derived/mun_tarifas_2017.csv")

shp <- st_read(file.path(mlpv, "shp/agem/00mun.shp"), quiet = TRUE) |>
  select(cvegeo = CVEGEO, nombre = NOMGEO)
g <- shp |> left_join(mun |> select(cvegeo, tarifa_principal, pct_tarifa_principal, pct_dac, clientes, kwh_mes_por_cliente),
                      by = "cvegeo")
message(sprintf("polygons %d, with CFE data %d, CFE municipalities without polygon %d",
                nrow(g), sum(!is.na(g$tarifa_principal)), sum(!mun$cvegeo %in% shp$cvegeo)))

g <- ms_simplify(g, keep = 0.012, keep_shapes = TRUE) |> st_transform(4326)
unlink("data/derived/mun_tarifas_2017.geojson")
st_write(g, "data/derived/mun_tarifas_2017.geojson", quiet = TRUE, layer_options = "COORDINATE_PRECISION=3")
message(sprintf("geojson %.0f KB", file.size("data/derived/mun_tarifas_2017.geojson") / 1024))
print(table(g$tarifa_principal, useNA = "ifany"))
