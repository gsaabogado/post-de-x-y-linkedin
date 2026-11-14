# 17_mun_tarifas_anual.R ----------------------------------------------------------
# Main household tariff of each municipality, by year, from the two CFE sources we
# hold, for the primer's tariff map over time:
#   2010-2017  CFE open data "Usuarios y consumo de electricidad por municipio"
#              (MLPV project, one value of users per municipality, tariff and year)
#   2020-2025  CFE "Usuarios y Consumos por Municipio", PNT obligation Estadísticas
#              (Dropbox Data/cfe_usuarios_municipio/raw_2026-10/, CFE_MUN_DIR in .Renviron), the
#              December column of sheet "Usuarios" (the 2022 Q4 file holds October
#              to December only, so December is the one month every PNT year has)
# 2018 and 2019 are missing: their PNT links are dead (requested, folio 340007700439826).
#
# Same rules as script 06: household tariffs 1 to 1F and DAC; the main tariff is the
# one with most household customers outside DAC; "01" printed as "1". The PNT sheets
# carry a national row (Cve Inegi 99, Cve Mun 999) and state "Totales" rows: dropped.
#
# Writes data/derived/mun_tarifas_anual.csv (one row per municipality and year).
# Run from the post root: Rscript scripts/17_mun_tarifas_anual.R

library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

readRenviron(".Renviron")
HOME_TARIFFS <- c("01", "1A", "1B", "1C", "1D", "1E", "1F", "DAC")

# 2010-2017, CFE open data ------------------------------------------------------------
mlpv <- path.expand(Sys.getenv("MLPV_IN"))
od <- read_csv(file.path(mlpv, "Electricity/cfe_users_consumption_mun/cfe_users_consumption_mun_2010_2017.csv"),
               col_types = cols(cvegeo = "c", tarifa = "c", cve_ent = "c", cve_mun = "c",
                                entidad = "c", municipio = "c", .default = "d")) |>
  filter(tarifa %in% HOME_TARIFFS) |>
  transmute(year = as.integer(year), cvegeo, entidad, municipio, tarifa, users, fuente = "datos abiertos CFE")

# 2020-2025, PNT statistic, December ------------------------------------------------------
raw_dir <- path.expand(Sys.getenv("CFE_MUN_DIR"))   # the snapshot folder raw_2026-10
if (!dir.exists(raw_dir)) stop("Set CFE_MUN_DIR in .Renviron (Dropbox Data/cfe_usuarios_municipio/raw_2026-10)")
tmp <- file.path(tempdir(), "cfe_mun"); dir.create(tmp, showWarnings = FALSE)
files <- list.files(raw_dir, "^cfe_usuarios_municipio_.*\\.(zip|xlsx)$", full.names = TRUE)
xlsx <- map_chr(files, \(f) {
  if (str_ends(f, "xlsx")) return(f)
  d <- file.path(tmp, tools::file_path_sans_ext(basename(f))); unzip(f, exdir = d)
  x <- list.files(d, "\\.xlsx$", full.names = TRUE, recursive = TRUE); stopifnot(length(x) == 1); x
})

read_pnt <- function(f) {
  raw <- read_excel(f, "Usuarios", col_names = FALSE, .name_repair = "minimal")
  hr <- which(apply(raw, 1, \(r) any(r %in% "Tarifa")))[1]
  names(raw) <- make.unique(as.character(unlist(raw[hr, ])))
  yr <- as.integer(str_match(paste(unlist(raw[seq_len(hr), 1]), collapse = " "), "Totales (\\d{4})")[, 2])
  stopifnot(!is.na(yr), "Diciembre" %in% names(raw))
  raw[-seq_len(hr), ] |>
    filter(!is.na(`Cve Mun`), `Cve Mun` != "999", !is.na(`Cve Inegi`),
           !str_detect(coalesce(`Entidad Federativa`, ""), "Nacional|Totales"),
           Tarifa %in% HOME_TARIFFS) |>
    transmute(year = yr, cvegeo = paste0(str_pad(`Cve Inegi`, 2, pad = "0"), str_pad(`Cve Mun`, 3, pad = "0")),
              entidad = `Entidad Federativa`, municipio = Municipio, tarifa = Tarifa,
              users = suppressWarnings(as.numeric(Diciembre)), fuente = "PNT Estadísticas, diciembre")
}
pnt <- map_dfr(xlsx, read_pnt)
stopifnot(!anyNA(pnt$users), sort(unique(pnt$year)) == 2020:2025)

# main tariff by municipality and year ------------------------------------------------------
all <- bind_rows(od, pnt)
stopifnot(!any(duplicated(all[c("year", "cvegeo", "tarifa")])))
mun <- all |>
  group_by(year, cvegeo, fuente) |>
  summarise(entidad = first(entidad), municipio = first(municipio),
            clientes = sum(users),
            clientes_dac = sum(users[tarifa == "DAC"]),
            tarifa_principal = tarifa[tarifa != "DAC"][which.max(users[tarifa != "DAC"])],
            pct_tarifa_principal = 100 * max(users[tarifa != "DAC"]) / sum(users[tarifa != "DAC"]),
            .groups = "drop") |>
  filter(clientes > 0) |>
  mutate(tarifa_principal = sub("^01$", "1", tarifa_principal),
         pct_dac = 100 * clientes_dac / clientes)
stopifnot(!anyNA(mun$tarifa_principal), !any(duplicated(mun[c("year", "cvegeo")])))
write_csv(mun, "data/derived/mun_tarifas_anual.csv")

# checks ------------------------------------------------------------------------------------
cat("\nHousehold customers (1-1F and DAC), millions, and municipalities, by year:\n")
print(mun |> group_by(year) |> summarise(millones = round(sum(clientes) / 1e6, 2), municipios = n(),
                                         dac = sum(clientes_dac)), n = 20)
cat("\nMunicipalities by main tariff and year:\n")
print(mun |> count(year, tarifa_principal) |> pivot_wider(names_from = tarifa_principal, values_from = n, values_fill = 0), n = 20)
old <- read_csv("data/derived/mun_tarifas_2017.csv", col_types = cols(cvegeo = "c", .default = "c"))
cmp <- mun |> filter(year == 2017) |> inner_join(old |> select(cvegeo, t06 = tarifa_principal), by = "cvegeo")
stopifnot(nrow(cmp) == nrow(old), all(cmp$tarifa_principal == cmp$t06))
cat("\n2017 equals script 06 in all", nrow(cmp), "municipalities\n")
stopifnot(abs(sum(mun$clientes[mun$year == 2024]) / 1e6 - 43.72) < 0.005)

# national customers and consumption by tariff, PNT years --------------------------------
# Customers: December (clientes_dic) and the mean of the months the file holds
# (clientes_prom). kWh: sum of those months from sheet "Ventas". kWh per customer a
# month = kWh / mean customers / months, so a year with all 12 months matches script
# 14's 2010-2017 definition (yearly kWh / customers / 12). 2022's file holds 3 months.
MESES <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto",
           "Septiembre", "Octubre", "Noviembre", "Diciembre")
read_sheet_long <- function(f, sheet) {
  raw <- read_excel(f, sheet, col_names = FALSE, .name_repair = "minimal")
  hr <- which(apply(raw, 1, \(r) any(r %in% "Tarifa")))[1]
  h <- as.character(unlist(raw[hr, ]))
  names(raw) <- make.unique(ifelse(is.na(h) | h == "", paste0("x", seq_along(h)), h))   # Ventas has blank header cells
  yr <- as.integer(str_match(paste(unlist(raw[seq_len(hr), 1]), collapse = " "), "Totales (\\d{4})")[, 2])
  raw[-seq_len(hr), ] |>
    filter(!is.na(`Cve Mun`), `Cve Mun` != "999", !is.na(`Cve Inegi`),
           !str_detect(coalesce(`Entidad Federativa`, ""), "Nacional|Totales"),
           Tarifa %in% HOME_TARIFFS) |>
    select(Tarifa, any_of(MESES)) |>
    pivot_longer(-Tarifa, names_to = "mes", values_to = "v") |>
    mutate(year = yr, v = as.numeric(v), tarifa = sub("^01$", "1", Tarifa)) |>
    group_by(year, tarifa, mes) |> summarise(v = sum(v), .groups = "drop")
}
nac <- map_dfr(xlsx, \(f) {
  u <- read_sheet_long(f, "Usuarios"); k <- read_sheet_long(f, "Ventas")
  stopifnot(setequal(u$mes, k$mes), !anyNA(u$v), !anyNA(k$v))
  u |> group_by(year, tarifa) |>
    summarise(meses = n_distinct(mes), clientes_dic = v[mes == "Diciembre"], clientes_prom = mean(v), .groups = "drop") |>
    left_join(k |> group_by(year, tarifa) |> summarise(kwh = sum(v), .groups = "drop"), by = c("year", "tarifa"))
}) |>
  group_by(year) |>
  mutate(pct_clientes_dic = 100 * clientes_dic / sum(clientes_dic),
         kwh_mes_cliente = kwh / clientes_prom / meses) |> ungroup()
stopifnot(nrow(nac) == 6 * 8, !anyNA(nac$kwh_mes_cliente),
          all(abs(nac |> group_by(year) |> summarise(t = sum(clientes_dic)) |> pull(t) -
                  mun |> filter(year >= 2020) |> group_by(year) |> summarise(t = sum(clientes)) |> pull(t)) < 1))
write_csv(nac, "data/derived/nacional_tarifas_anual.csv")
cat("\nNational, 2025:\n"); print(nac |> filter(year == 2025) |> mutate(across(where(is.double), \(x) round(x, 1))))
