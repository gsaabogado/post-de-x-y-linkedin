# 19c_serie_mejor_estimacion.R ------------------------------------------------------
# Best-guess installed price of a small home system (about 0 to 5 kW, without IVA),
# one value per year 2008 to 2026, in pesos per W. Built in dollars, because the
# equipment is imported and installers quote in dollars, then converted at each
# year's rate. Every year carries how it was obtained:
#   observado  printed by a Mexican source for a small system
#   relleno    between two observed years
#   modelado   2021 survey price moved by the world panel price (Ember, China customs)
# Method, choices and checks: notes.md, section 2026-10-31.
#
# Run from the post root: Rscript scripts/19c_serie_mejor_estimacion.R
# Output: data/derived/serie_precio_instalado.csv

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

readRenviron(".Renviron")
YEARS <- 2008:2026

fx <- read_csv("data/raw/costos/fred_aexmxus_anual.csv", show_col_types = FALSE) |>
  transmute(anio = as.integer(str_sub(observation_date, 1, 4)), mxn_usd = as.numeric(AEXMXUS)) |>
  filter(anio %in% YEARS)
# 2026 has no annual average: the August rate read for the GDP post (FRED EXMXUS)
fx26 <- readRDS("../04-mexico-gdp-marginals/data/derived/fx_2026_08.rds")$mxn_per_usd
stopifnot(!2026 %in% fx$anio, is.numeric(fx26), fx26 > 10)
fx <- bind_rows(fx, tibble(anio = 2026L, mxn_usd = fx26))
stopifnot(nrow(fx) == length(YEARS), !anyNA(fx$mxn_usd))

inpc <- read_csv("data/derived/primer/inpc.csv", show_col_types = FALSE) |>
  group_by(anio) |> summarise(a_pesos_ult = mean(a_pesos_ult), .groups = "drop")

# world panel price, USD per W, from value and capacity of China's exports to Mexico
ember <- read_csv(file.path(path.expand(Sys.getenv("MLPV_IN")),
                            "Trade/ember_china/ember_china_solar_exports_mx.csv"),
                  show_col_types = FALSE) |>
  filter(`Commodity type` == "Panels") |>
  mutate(anio = as.integer(str_sub(as.character(Date), 1, 4))) |>
  group_by(anio) |>
  summarise(panel_usd_w = sum(`Amount (USD)`) / sum(`Capacity (MW)`) / 1e6, .groups = "drop")
stopifnot(all(2021:2026 %in% ember$anio), !anyNA(ember$panel_usd_w))

# observed prices, by source ---------------------------------------------------------
# 2008-2010 printed in pesos (1 to 2 kW); 2013-2017 ABM 1 to 10 kW; 2018 Grupo AVE;
# 2019 and 2021 the 0 to 5 kW row of the Monitor's own comparison table (Tabla 5).
pesos_obs <- c(`2008` = 138.6, `2009` = 108, `2010` = 80.5)
usd_obs   <- c(`2013` = 2.50, `2014` = 2.30, `2015` = 2.00, `2016` = 1.75, `2017` = 1.55,
               `2018` = 26.92 / 19.23, `2019` = 1.35, `2021` = 1.24)

s <- tibble(anio = YEARS) |>
  left_join(fx, by = "anio") |>
  mutate(usd = coalesce(unname(usd_obs[as.character(anio)]),
                        unname(pesos_obs[as.character(anio)]) / mxn_usd))

# gaps inside the observed span: a constant percentage change in dollars
obs_years <- s$anio[!is.na(s$usd)]
fill <- exp(approx(obs_years, log(s$usd[!is.na(s$usd)]), xout = s$anio)$y)

# after 2021: the survey price moves one dollar for each dollar of panel price
p21 <- ember$panel_usd_w[ember$anio == 2021]
retail_panel_21 <- 0.50                                   # Monitor 2021, Tabla 8, 0 to 5 kW
s <- s |>
  left_join(ember, by = "anio") |>
  mutate(metodo = case_when(!is.na(usd) ~ "observado",
                            anio <= 2021 ~ "relleno",
                            TRUE ~ "modelado"),
         usd_central = case_when(metodo == "observado" ~ usd,
                                 metodo == "relleno" ~ fill,
                                 TRUE ~ 1.24 - (p21 - panel_usd_w)),
         # the low case passes the panel fall through in proportion to the retail panel
         usd_bajo = case_when(metodo == "modelado" ~ 1.24 - retail_panel_21 * (1 - panel_usd_w / p21),
                              metodo == "relleno" ~ usd_central * 0.8,
                              anio <= 2010 ~ usd_central * 0.8,
                              TRUE ~ usd_central * 0.9),
         # the high case passes half of it through
         usd_alto = case_when(metodo == "modelado" ~ 1.24 - 0.5 * (p21 - panel_usd_w),
                              metodo == "relleno" ~ usd_central * 1.2,
                              anio <= 2010 ~ usd_central * 1.2,
                              TRUE ~ usd_central * 1.1)) |>
  # panels rose a little in 2022, which swaps the two cases that year
  mutate(bajo = pmin(usd_bajo, usd_alto, usd_central), alto = pmax(usd_bajo, usd_alto, usd_central)) |>
  left_join(inpc, by = "anio") |>
  mutate(pesos = usd_central * mxn_usd, pesos_bajo = bajo * mxn_usd, pesos_alto = alto * mxn_usd,
         pesos_hoy = pesos * a_pesos_ult, pesos_con_iva = pesos * 1.16) |>
  select(anio, metodo, mxn_usd, usd = usd_central, pesos, pesos_bajo, pesos_alto, pesos_hoy, pesos_con_iva)
stopifnot(nrow(s) == length(YEARS), !anyNA(s$pesos), !anyNA(s$pesos_hoy), all(s$pesos_bajo <= s$pesos),
          all(s$pesos_alto >= s$pesos))
write_csv(s, "data/derived/serie_precio_instalado.csv")

print(s |> mutate(across(c(usd), ~ round(.x, 2)), across(c(mxn_usd, pesos, pesos_bajo, pesos_alto, pesos_hoy, pesos_con_iva), ~ round(.x, 1))),
      n = 25, width = 120)

# does it behave ---------------------------------------------------------------------
chk <- s |> mutate(sube_nominal = pesos > lag(pesos), sube_real = pesos_hoy > lag(pesos_hoy), sube_usd = usd > lag(usd))
cat("\nYears the price rose. In pesos:", chk$anio[which(chk$sube_nominal)],
    " In real pesos:", chk$anio[which(chk$sube_real)], " In dollars:", chk$anio[which(chk$sube_usd)], "\n")

# checks against numbers the series was NOT built from -------------------------------
at <- function(y) s$pesos[s$anio == y]
cat(sprintf("2016 ABM residential 32.37 pesos per W; series %.1f\n", at(2016)))
cat(sprintf("2018 IMCO quote 2 panels 0.65 kW, without IVA %.1f; series %.1f\n", 21600 / 1.16 / 650, at(2018)))
cat(sprintf("2018 IMCO 2.25 kW %.1f (IVA not stated); series %.1f\n", 69000 / 2250, at(2018)))
cat(sprintf("2023 Grupo AVE 0 to 250 kW 15.22; times small-size premium 1.24/0.90 = %.1f; series %.1f\n",
            15.22 * 1.24 / 0.90, at(2023)))
cat(sprintf("2026 with IVA %.1f; market guides 18 to 28 pesos per W (unsourced)\n", at(2026) * 1.16))
