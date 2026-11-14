# 23_frontera.R ---------------------------------------------------------------------
# Homes with solar panels on both sides of the Mexico-US border.
#   Mexico: Intercensal 2025 tabulado 105 (PCN_VPH_PANEL), and Censo 2020 extended
#   questionnaire by national income tenth (post 06, viv2020_slim.rds). 2020 income is
#   household labour income over the square root of household size, pesos of Aug 2026.
#   US: EIA-861, net-metered residential PV customers over residential customers, and
#   average residential revenue per kWh. A utility's area is not a city.
#
# Run from the post root, after 22 and 21: Rscript scripts/23_frontera.R
# Output: data/derived/frontera/{pares.csv, mexico_2020.csv, precio_mx.csv}

library(dplyr)
library(readr)
library(readxl)
library(stringr)

readRenviron(".Renviron")
OUT <- "data/derived/frontera"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
source("../06-mexico-solar-income/scripts/_survey.R")

# West to east along the border
PARES <- tribble(
  ~orden, ~cve,    ~mx,               ~us,               ~utility,                             ~st,
  1L,     "02004", "Tijuana",         "San Diego",       "San Diego Gas & Electric Co",        "CA",
  2L,     "02002", "Mexicali",        "Valle Imperial",  "Imperial Irrigation District",       "CA",
  3L,     "26043", "Nogales",         "UNS Electric",    "UNS Electric, Inc",                  "AZ",
  4L,     "08037", "Ciudad Juárez",   "El Paso",         "El Paso Electric Co",                "TX",
  5L,     "28032", "Reynosa",         "Magic Valley",    "Magic Valley Electric Coop Inc",     "TX",
  6L,     "28022", "Matamoros",       "Brownsville",     "Brownsville Public Utilities Board", "TX"
)
num <- \(x) suppressWarnings(as.numeric(x))
hoja <- function(f, sh, skip) suppressWarnings(suppressMessages(
  read_excel(f, sheet = sh, col_names = FALSE, .name_repair = "minimal", skip = skip)))

# US ---------------------------------------------------------------------------------
eia <- function(y) {
  d <- file.path("data/raw/eia861", y)
  f_nm <- file.path(d, sprintf("Net_Metering_%d.xlsx", y))
  # read the residential PV customer column by its header, never by position
  h <- hoja(f_nm, "States", 0)[1:3, ]
  grp <- zoo_fill <- unlist(h[2, ]); for (i in seq_along(grp)) if (is.na(grp[i]) && i > 1) grp[i] <- grp[i - 1]
  col_pv <- which(unlist(h[3, ]) == "Residential" & str_detect(grp, "Installations|Customers"))[1]
  stopifnot(!is.na(col_pv), str_detect(paste(unlist(h[1:2, 6:col_pv]), collapse = " "), "Photovoltaic"))
  nm <- hoja(f_nm, "States", 3)
  n <- tibble(st = nm[[2]], utility = nm[[4]], clientes_pv = num(nm[[col_pv]]))
  sa <- hoja(file.path(d, sprintf("Sales_Ult_Cust_%d.xlsx", y)), 1, 3)
  s <- tibble(utility = sa[[3]], st = sa[[7]], rev = num(sa[[10]]), mwh = num(sa[[11]]), clientes = num(sa[[12]])) |>
    group_by(utility, st) |>
    summarise(cents = 100 * sum(rev, na.rm = TRUE) / sum(mwh, na.rm = TRUE),
              cents_max = max(100 * rev / mwh, na.rm = TRUE),
              clientes = sum(clientes, na.rm = TRUE), .groups = "drop")
  r <- PARES |> select(us, utility, st) |> left_join(n, by = c("utility", "st")) |> left_join(s, by = c("utility", "st")) |>
    mutate(anio = y, pct_us = 100 * clientes_pv / clientes)
  stopifnot(nrow(r) == 6, !anyNA(r$pct_us))
  r
}
us <- bind_rows(eia(2020), eia(2024))
print(as.data.frame(us |> select(anio, us, clientes_pv, clientes, pct_us, cents, cents_max) |> mutate(across(where(is.double), ~ round(.x, 2)))))

# Mexico 2025 ------------------------------------------------------------------------
EIC <- file.path(path.expand(Sys.getenv("MLPV_IN")), "Census/intercensal2025/raw/conjunto_de_datos_eic2025_105_csv.zip")
eic <- read_csv(unz(EIC, "conjunto_de_datos/conjunto_datos_eic2025_105.csv"),
                col_types = cols(.default = "c"), locale = locale(encoding = "latin1"))
mx25 <- eic |> filter(CVE_MUN != "000", CVE_LOC == "0000", substr(CVEGEO, 1, 5) %in% PARES$cve) |>
  transmute(cve = substr(CVEGEO, 1, 5), estimador = ESTIMADOR, v = num(PCN_VPH_PANEL)) |>
  filter(estimador %in% c("Valor", "Error estándar")) |>
  tidyr::pivot_wider(names_from = estimador, values_from = v) |>
  transmute(cve, pct_mx = Valor, se_mx = `Error estándar`)
stopifnot(nrow(mx25) == 6, !anyNA(mx25$pct_mx))

pares <- PARES |> left_join(mx25, by = "cve") |>
  left_join(us |> filter(anio == 2024) |> select(us, pct_us, cents, cents_max, clientes_pv, clientes), by = "us") |>
  left_join(us |> filter(anio == 2020) |> transmute(us, pct_us_2020 = pct_us), by = "us") |>
  mutate(veces = pct_us / pct_mx)
write_csv(pares, file.path(OUT, "pares.csv"))
print(as.data.frame(pares |> select(mx, pct_mx, se_mx, us, pct_us, pct_us_2020, cents, veces) |> mutate(across(where(is.double), ~ round(.x, 2)))))

# Mexico 2020 by national income tenth -------------------------------------------------
v <- readRDS("../06-mexico-solar-income/data/derived/viv2020_slim.rds") |>
  mutate(cve = paste0(substr(ESTRATO, 1, 2), substr(ESTRATO, 4, 6)))
stopifnot(nrow(v) > 4e6, all(PARES$cve %in% v$cve))
nh <- survey_setup(v)
en_pares <- v$cve %in% PARES$cve
dom <- case_when(en_pares & v$grp == "D10" ~ "10% de mayor ingreso", TRUE ~ NA_character_)
mx20 <- bind_rows(ratio_se(v, nh, v$panel, dom),
                  ratio_se(v, nh, v$panel, if_else(en_pares, "todos los hogares", NA_character_))) |>
  mutate(ingreso_d10_piso = min(v$inc[v$grp == "D10"], na.rm = TRUE),
         ingreso_d10_mediana = median(v$inc[v$grp == "D10"], na.rm = TRUE))
print(as.data.frame(mx20))
write_csv(mx20, file.path(OUT, "mexico_2020.csv"))

# Mexico as a whole: all homes 2025, top income tenth 2020 (the 2025 tabulados carry no income)
nac25 <- eic |> filter(CVEGEO == "000000000", ESTIMADOR == "Valor") |> pull(PCN_VPH_PANEL) |> num()
nac20 <- ratio_se(v, nh, v$panel, if_else(v$grp == "D10", "D10", NA_character_))
stopifnot(length(nac25) == 1, !is.na(nac25), nrow(nac20) == 1)
nac <- tibble(pct_2025 = nac25, pct_d10_2020 = nac20$pct, se_d10_2020 = nac20$se)
print(as.data.frame(nac))
write_csv(nac, file.path(OUT, "mexico_nacional.csv"))

# Mexico's price ---------------------------------------------------------------------
cur <- read_csv("data/derived/payback/payback_por_consumo.csv", show_col_types = FALSE) |>
  filter(tarifa == "1", kwh_mes == 90)
fx <- read_csv("data/raw/fx/EXMXUS.csv", show_col_types = FALSE) |> filter(!is.na(EXMXUS)) |> slice_tail(n = 1)
stopifnot(nrow(cur) == 1, nrow(fx) == 1)
dac <- read_csv("data/derived/payback/payback_por_consumo.csv", show_col_types = FALSE) |>
  filter(tarifa == "1", kwh_mes == 255)            # first step into DAC from tariff 1, Central region
cne <- read_csv("data/derived/cne_db1_db2_2026_septiembre.csv", show_col_types = FALSE) |>
  filter(cat == "DB1") |> group_by(unidades) |> summarise(total = median(total), .groups = "drop")
stopifnot(nrow(dac) == 1, dac$en_dac, setequal(cne$unidades, c("$/kWh", "$/mes")))
db1 <- setNames(cne$total, cne$unidades)
# Every price is the whole bill over kWh, as the US average revenue is: the same 90 kWh
# home at the regulator's DB1 tariff, fixed charge and IVA included
precio <- tibble(pesos_kwh = cur$recibo / (90 * 12), tc = fx$EXMXUS, fecha_tc = fx$observation_date,
                 centavos_usd = 100 * pesos_kwh / tc,
                 pesos_costo = (db1[["$/mes"]] + 90 * db1[["$/kWh"]]) * 1.16 / 90,
                 centavos_costo = 100 * pesos_costo / tc,
                 pesos_dac = dac$recibo / (255 * 12), centavos_dac = 100 * pesos_dac / tc)
write_csv(precio, file.path(OUT, "precio_mx.csv"))
print(as.data.frame(precio))
