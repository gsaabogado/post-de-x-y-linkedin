# 07_parse_cfe_history.R --------------------------------------------------------
# Monthly household prices 2016 to 2026 in long form, from the four readings of
# CFE's tariff pages in data/raw/cfe/:
#   cfe_tarifas_2016_2017_acervo.txt (archive pages: 2016 and January to November 2017),
#   cfe_tarifas_2017_esquema_vigente.txt (current pages: used for December 2017 only),
#   cfe_tarifas_2018_2025.txt (yearly blocks compacted in the browser, format in their
#   headers), cfe_verano_feb_mar_oct.txt (summer prices of February, March and October,
#   2016 to 2025) and the tables that 05_parse_cfe_2026.R wrote for 2026.
# Writes data/derived/cfe_precios_mensual.csv: one row per month and price series.
#   serie            what the price is (see SERIES below)
#   precio           pesos per kWh, or pesos per month for the DAC fixed charge
# Summer series run from February to October: a locality's summer is six months in a row
# that start between February and May, so no locality pays summer prices in all nine.
#
# Run from the post root, after 05: Rscript scripts/07_parse_cfe_history.R

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

T_NAMES <- c("t1_basico", "t1_intermedio", "excedente")
S_NAMES <- c("verano_1A1D_basico", "verano_1A1D_intermedio", "verano_1C1D_intermedio_alto",
             "verano_1E1F_basico", "verano_1E1F_intermedio_bajo", "verano_1E_intermedio_alto",
             "verano_1F_intermedio_alto")
D_NAMES <- c("dac_cargo_fijo", "dac_bc_verano", "dac_bc_fuera", "dac_bcs_verano", "dac_bcs_fuera",
             "dac_central", "dac_noroeste", "dac_norte_noreste", "dac_sur_peninsular")

read_blocks <- function(path) {
  x <- read_lines(path)
  x <- x[!str_detect(x, "^# [A-Za-z ]")]                # drop the prose header, keep "# 2021 hash"
  year <- NA_integer_
  out <- list()
  for (l in x) {
    if (str_detect(l, "^# \\d{4} ")) { year <- as.integer(str_sub(l, 3, 6)); next }
    p <- str_split(l, " ")[[1]]
    if (!p[1] %in% c("T", "S", "D", "E")) next
    nm <- switch(p[1], T = T_NAMES, S = S_NAMES, D = D_NAMES, E = "excedente_verano_leido")
    v <- as.numeric(p[-(1:2)])
    stopifnot(length(v) == length(nm), !anyNA(v))
    out[[length(out) + 1]] <- tibble(anio = year, mes = as.integer(p[2]), serie = nm, precio = v)
  }
  bind_rows(out)
}

# 2017 has two readings. The archive holds what was charged from January to November;
# the current pages show the December scheme for the whole year. Where both give a
# month they must agree, then the archive stands and December comes from the current pages.
acervo  <- read_blocks("data/raw/cfe/cfe_tarifas_2016_2017_acervo.txt")
vigente <- read_blocks("data/raw/cfe/cfe_tarifas_2017_esquema_vigente.txt")
both <- inner_join(acervo, vigente, by = c("anio", "mes", "serie"), suffix = c("_acervo", "_vigente"))
stopifnot(nrow(both) > 0, all(both$anio == 2017), all(both$precio_acervo == both$precio_vigente))
cat("2017, months in both readings:", nrow(both), "prices, all equal\n")
hist <- bind_rows(acervo,
                  anti_join(vigente, acervo, by = c("anio", "mes", "serie")),
                  read_blocks("data/raw/cfe/cfe_tarifas_2018_2025.txt"))
stopifnot(setequal(unique(hist$anio), 2016:2025),
          all(count(filter(hist, serie == "t1_basico"), anio)$n == 12),
          all(count(filter(hist, serie == "dac_central"), anio)$n == 12))

# summer prices of February, March and October; the excess price read with them must
# be tariff 1's of the same month
verano <- read_blocks("data/raw/cfe/cfe_verano_feb_mar_oct.txt")
chk <- verano |> filter(serie == "excedente_verano_leido") |>
  left_join(filter(hist, serie == "excedente"), by = c("anio", "mes"), suffix = c("_verano", "_t1"))
stopifnot(nrow(chk) == 30, !anyNA(chk$precio_t1), all(chk$precio_verano == chk$precio_t1))
verano <- filter(verano, serie != "excedente_verano_leido")
stopifnot(nrow(inner_join(verano, hist, by = c("anio", "mes", "serie"))) == 0)
hist <- bind_rows(hist, verano)
stopifnot(all(count(filter(hist, serie == "verano_1A1D_basico"), anio)$n == 9))

# 2026 from the tables of script 05 ---------------------------------------------
b <- read_csv("data/derived/cfe_bloques_2026.csv", show_col_types = FALSE)
pick <- function(tar, temp, n, name) b |> filter(tarifa == tar, temporada == temp, bloque_n == n) |>
  transmute(anio = 2026L, mes, serie = name, precio = precio_kwh)
y26 <- bind_rows(
  pick("1", "todo el año", 1, "t1_basico"), pick("1", "todo el año", 2, "t1_intermedio"),
  pick("1", "todo el año", 3, "excedente"),
  pick("1A", "verano", 1, "verano_1A1D_basico"), pick("1A", "verano", 2, "verano_1A1D_intermedio"),
  pick("1C", "verano", 3, "verano_1C1D_intermedio_alto"),
  pick("1E", "verano", 1, "verano_1E1F_basico"), pick("1E", "verano", 2, "verano_1E1F_intermedio_bajo"),
  pick("1E", "verano", 3, "verano_1E_intermedio_alto"), pick("1F", "verano", 3, "verano_1F_intermedio_alto"))
d <- read_csv("data/derived/cfe_dac_2026.csv", show_col_types = FALSE)
reg <- c(CEN = "dac_central", NO = "dac_noroeste", NNE = "dac_norte_noreste", SP = "dac_sur_peninsular")
d26 <- bind_rows(
  d |> filter(region == "CEN") |> transmute(anio = 2026L, mes, serie = "dac_cargo_fijo", precio = cargo_fijo_mes),
  d |> filter(region %in% names(reg)) |> transmute(anio = 2026L, mes, serie = reg[region], precio = precio_kwh),
  d |> filter(region == "BC") |> transmute(anio = 2026L, mes, serie = "dac_bc_verano", precio = precio_kwh_verano),
  d |> filter(region == "BC") |> transmute(anio = 2026L, mes, serie = "dac_bc_fuera", precio = precio_kwh),
  d |> filter(region == "BCS") |> transmute(anio = 2026L, mes, serie = "dac_bcs_verano", precio = precio_kwh_verano),
  d |> filter(region == "BCS") |> transmute(anio = 2026L, mes, serie = "dac_bcs_fuera", precio = precio_kwh))

out <- bind_rows(hist, y26, d26) |> arrange(serie, anio, mes)
stopifnot(!anyNA(out$precio), nrow(out) == nrow(distinct(out, anio, mes, serie)))
write_csv(out, "data/derived/cfe_precios_mensual.csv")

# tariff 1 in December of each year, and what the 2019 rule did
print(out |> filter(mes == 12, serie %in% c(T_NAMES, "dac_central")) |>
        pivot_wider(names_from = serie, values_from = precio) |>
        mutate(alza_basico_pct = round(100 * (t1_basico / lag(t1_basico) - 1), 2)), n = 12)

# DAC, Central region, range within each year
print(out |> filter(serie == "dac_central") |> group_by(anio) |>
        summarise(meses = n(), minimo = min(precio), maximo = max(precio)), n = 12)
