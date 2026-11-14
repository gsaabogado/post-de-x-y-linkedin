# 19b_precio_instalado_pesos.R -----------------------------------------------------
# One row per year: installed price of a small home system in pesos per W, from the
# values transcribed in data/raw/costos/precios_instalados_transcritos.csv. Dollars
# go to pesos at the annual average rate (FRED AEXMXUS, Federal Reserve), then to
# pesos of the latest INPC month. Tables keep the size range of each source.
# Run from the post root: Rscript scripts/19b_precio_instalado_pesos.R

library(dplyr)
library(readr)
library(stringr)

x   <- read_csv("data/raw/costos/precios_instalados_transcritos.csv", show_col_types = FALSE)
fx  <- read_csv("data/raw/costos/fred_aexmxus_anual.csv", show_col_types = FALSE) |>
  transmute(anio = as.integer(str_sub(observation_date, 1, 4)), mxn_usd = as.numeric(AEXMXUS))
inp <- read_csv("data/derived/primer/inpc.csv", show_col_types = FALSE) |>
  group_by(anio) |> summarise(a_pesos_ult = mean(a_pesos_ult), .groups = "drop")

# the series a home faced: the smallest size each source prints for that year
keep <- tribble(
  ~anio, ~tamano_kw,                   ~tramo,
  2007,  "<10",                        "A",
  2008,  "1-2",                        "A",
  2009,  "1-2",                        "A",
  2010,  "1-2",                        "A",
  2013,  "1-10",                       "B",
  2014,  "1-10",                       "B",
  2015,  "1-10",                       "B",
  2016,  "3.46 promedio residencial",  "B",
  2017,  "1-10",                       "B",
  2018,  "0-10 en su mayoría",         "B",
  2019,  "0-2.5",                      "B",
  2021,  "0-5",                        "B")

out <- x |>
  inner_join(keep, by = c("anio", "tamano_kw")) |>
  left_join(fx, by = "anio") |>
  left_join(inp, by = "anio") |>
  mutate(pesos_w = if_else(unidad == "USD/W", valor * mxn_usd, valor),
         pesos_w_reales = pesos_w * a_pesos_ult) |>
  select(anio, tramo, tamano_kw, valor, unidad, mxn_usd, pesos_w, pesos_w_reales, iva, fuente)
stopifnot(nrow(out) == nrow(keep), !anyNA(out$pesos_w), !anyNA(out$pesos_w_reales))
write_csv(out, "data/derived/precio_instalado_pesos.csv")
print(out |> mutate(across(c(pesos_w, pesos_w_reales), ~ round(.x, 1))) |>
        select(anio, tramo, tamano_kw, valor, unidad, pesos_w, pesos_w_reales, iva), n = 20, width = 120)
