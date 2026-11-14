# 20_payback_anual.R ----------------------------------------------------------------
# Yearly payback of a home solar system by tariff, 2008 to 2026: a home with its
# tariff's average consumption that year, buying at that year's installed price and
# saving at that year's electricity prices. Simple payback, as in script 18. Systems come
# in whole panels of that year's typical wattage, in even numbers from four: the smallest
# package that covers the home's consumption, and the four-panel package for comparison.
#
# Inputs: precios_hogar_detalle.csv (every tariff, month and block), primer/clientes_tarifa.csv
# and nacional_tarifas_anual.csv (kWh per home), payback/yield_tarifa_mes.csv and
# payback/perfil_mensual_tarifa.csv (script 18), serie_precio_instalado.csv (script 19c),
# panel_w_anual.csv (script 19d).
#
# Rules of each period are the ones printed: until June 2012 the blocks depend on how
# much the home consumes in the month, so the bill is read from the table that applies
# to the kWh actually billed after the panel.
# Assumptions in notes.md, section 2026-10-31.
#
# Run from the post root: Rscript scripts/20_payback_anual.R
# Output: data/derived/payback/payback_anual.csv

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

OUT       <- "data/derived/payback"
YEARS     <- 2008:2026
TARIFFS   <- c("1", "1A", "1B", "1C", "1D", "1E", "1F")
IVA       <- 1.16        # on the bill and on the system; 15% in 2008-2009, ignored
MIN_KWH   <- 25          # monthly minimum billed
SUMMER    <- 4:9         # six months, April start; the true start varies by locality
MIN_PANELES <- 4         # installers' smallest package with installation and CFE paperwork
PASO_PANELES <- 2        # packages move in even numbers (data/raw/costos/paquetes_instaladores_2026.csv)
REGION    <- "Central"   # DAC prices

# prices ---------------------------------------------------------------------------
det <- read_csv("data/derived/precios_hogar_detalle.csv", show_col_types = FALSE,
                col_types = cols(parte = "c", captura = "c", region = "c", otros_valores = "c")) |>
  filter(anio %in% YEARS) |>
  mutate(dia = as.integer(coalesce(str_match(parte, "^(\\d+)")[, 2], "1"))) |>
  group_by(tarifa, region, temporada, esquema, bloque, anio, mes) |>
  slice_max(dia, n = 1, with_ties = FALSE) |> ungroup() |>
  mutate(lim = as.numeric(str_extract(esquema, "\\d+")),
         tipo = case_when(esquema == "única" ~ "unica", str_starts(esquema, "hasta") ~ "hasta", TRUE ~ "mas"),
         hasta = coalesce(kwh_hasta, Inf), desde0 = kwh_desde - 1)

hog <- det |> filter(tarifa != "DAC") |> split(~ tarifa + anio + mes + temporada, drop = TRUE, sep = "|")
season_of <- function(t, m) if (t == "1") "todo el año" else if (m %in% SUMMER) "verano" else "fuera de verano"

bill_tariff <- function(q, t, y, m) {
  b <- hog[[paste(t, y, m, season_of(t, m), sep = "|")]]
  if (is.null(b)) stop("no prices ", t, " ", y, " ", m)
  q <- max(q, MIN_KWH)
  b <- b[b$tipo == "unica" | (b$tipo == "hasta" & q <= b$lim) | (b$tipo == "mas" & q > b$lim), ]
  en <- pmax(0, pmin(q, b$hasta) - b$desde0)
  stopifnot(abs(sum(en) - q) < 1e-6)                # the blocks cover the kWh, no hole or overlap
  sum(en * b$precio) * IVA
}

dac_raw <- det |> filter(tarifa == "DAC")
dac_month <- function(y, m) {
  mm <- max(dac_raw$mes[dac_raw$anio == y & dac_raw$mes <= m & dac_raw$bloque != "cargo_fijo"])  # 2026: carries October
  d <- dac_raw[dac_raw$anio == y & dac_raw$mes == mm, ]
  fijo <- d$precio[d$bloque == "cargo_fijo"]
  p <- d$precio[d$bloque == "energia" & d$region == REGION]
  stopifnot(length(fijo) == 1, length(p) == 1)
  c(fijo = fijo, p = p)
}
DAC <- map(YEARS, \(y) map(1:12, \(m) dac_month(y, m))) |> setNames(YEARS)
bill_dac <- function(q, y, m) { d <- DAC[[as.character(y)]][[m]]; unname(d["fijo"] + q * d["p"]) * IVA }

# consumption ----------------------------------------------------------------------
a <- read_csv("data/derived/primer/clientes_tarifa.csv", show_col_types = FALSE) |>
  transmute(anio, tarifa = sub("^01$", "1", tarifa), kwh = kwh_mes_cliente)
b <- read_csv("data/derived/nacional_tarifas_anual.csv", show_col_types = FALSE) |>
  filter(meses == 12) |> transmute(anio = year, tarifa, kwh = kwh_mes_cliente)     # 2022 holds 3 months: out
cons <- bind_rows(a, b |> anti_join(a, by = c("anio", "tarifa"))) |>
  # 1D in 2017 reads 176 against 226 and 239 around it, unexplained: treated as missing
  filter(!(tarifa == "1D" & anio == 2017))
cons <- expand_grid(tarifa = c(TARIFFS, "DAC"), anio = YEARS) |>
  left_join(cons, by = c("tarifa", "anio")) |>
  group_by(tarifa) |>
  mutate(origen_kwh = if_else(is.na(kwh), "estimado", "cfe"),
         kwh = approx(anio[!is.na(kwh)], kwh[!is.na(kwh)], xout = anio, rule = 2)$y) |>
  ungroup()
stopifnot(nrow(cons) == 8 * length(YEARS), !anyNA(cons$kwh))

yield <- read_csv(file.path(OUT, "yield_tarifa_mes.csv"), show_col_types = FALSE)
shape <- read_csv(file.path(OUT, "perfil_mensual_tarifa.csv"), show_col_types = FALSE)
shape_of <- function(t) { s <- shape[shape$tarifa == t, ]; s$indice[match(c(2:12, 1), s$mes)] }
yield_of <- function(t) { d <- yield[yield$tarifa == t, ]; d$kwh_kwp[order(d$mes)] }

precio <- read_csv("data/derived/serie_precio_instalado.csv", show_col_types = FALSE)
panel_w <- read_csv("data/derived/panel_w_anual.csv", show_col_types = FALSE)
stopifnot(all(YEARS %in% panel_w$anio))
stopifnot(identical(as.integer(precio$anio), YEARS))

# one home, one year ---------------------------------------------------------------
billed_year <- function(c12, gen) {
  credit <- 0; q <- numeric(12)
  for (y in 1:2) for (m in 1:12) { net <- c12[m] - gen[m] - credit; q[m] <- max(net, 0); credit <- max(-net, 0) }
  q
}

run_year <- function(t, y) {
  en_dac <- t == "DAC"
  kwh <- cons$kwh[cons$tarifa == t & cons$anio == y]
  c12 <- kwh * shape_of(t)
  gy  <- yield_of(if (en_dac) "1" else t)        # DAC home: tariff 1's sun, DAC prices Central
  billf <- if (en_dac) \(q) sum(map_dbl(1:12, \(m) bill_dac(q[m], y, m))) else
    \(q) sum(map_dbl(1:12, \(m) bill_tariff(q[m], t, y, m)))
  b0 <- billf(c12)
  p <- precio[precio$anio == y, ]
  w <- panel_w$w[panel_w$anio == y]
  # headline: the smallest package that covers the home's consumption over the year
  # comparison: the smallest package on sale, whatever it covers
  n_cubre <- max(MIN_PANELES, PASO_PANELES * ceiling(sum(c12) / sum(gy) * 1000 / w / PASO_PANELES))
  fila <- function(n, regla) {
    s <- n * w / 1000
    ah <- b0 - billf(billed_year(c12, s * gy))
    tibble(anio = y, tarifa = t, regla = regla, kwh_mes = kwh,
           origen_kwh = cons$origen_kwh[cons$tarifa == t & cons$anio == y],
           metodo_precio = p$metodo, w_panel = w, paneles = n, kwp = s, recibo_ano = b0, ahorro_ano = ah,
           costo = s * 1000 * p$pesos * IVA,
           pb = s * 1000 * p$pesos * IVA / ah,
           pb_bajo = s * 1000 * p$pesos_bajo * IVA / ah,
           pb_alto = s * 1000 * p$pesos_alto * IVA / ah)
  }
  bind_rows(fila(n_cubre, "cubre_consumo"), fila(MIN_PANELES, "paquete_minimo"))
}

res <- expand_grid(t = c(TARIFFS, "DAC"), y = YEARS) |> pmap(\(t, y) run_year(t, y)) |> list_rbind()
stopifnot(nrow(res) == 2 * 8 * length(YEARS), !anyNA(res$pb), all(res$ahorro_ano > 0))
write_csv(res, file.path(OUT, "payback_anual.csv"))

# check: 2026 must reproduce script 18 (bill 1,483, saving 1,090 at 91 kWh, tariff 1)
t1 <- res |> filter(tarifa == "1", anio == 2026, regla == "cubre_consumo")
cat(sprintf("Tariff 1, 2026: %.1f kWh, bill %.0f, saving %.0f (script 18: 91 kWh, 1,483, 1,090)\n",
            t1$kwh_mes, t1$recibo_ano, t1$ahorro_ano))
stopifnot(abs(t1$recibo_ano - 1483) < 25, abs(t1$ahorro_ano - 1090) < 25)

tabla <- function(reg, col, dec = 1) {
  w <- res |> filter(regla == reg) |> mutate(p = round(.data[[col]], dec)) |>
    select(anio, tarifa, p) |> pivot_wider(names_from = tarifa, values_from = p)
  print(as.data.frame(w), row.names = FALSE)
}
cat("\nPayback, smallest package covering consumption\n"); tabla("cubre_consumo", "pb")
cat("\nPanels\n"); tabla("cubre_consumo", "paneles", 0)
cat("\nkW\n"); tabla("cubre_consumo", "kwp", 2)
cat("\nCost, pesos with IVA\n"); tabla("cubre_consumo", "costo", 0)
cat("\nPayback, 4-panel package\n"); tabla("paquete_minimo", "pb")
cat("\nLow and high payback, 2026\n")
print(as.data.frame(res |> filter(regla == "cubre_consumo", anio == 2026) |>
  mutate(across(c(pb, pb_bajo, pb_alto), ~ round(.x, 1))) |> select(tarifa, pb_bajo, pb, pb_alto)), row.names = FALSE)
