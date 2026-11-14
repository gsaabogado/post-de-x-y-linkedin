# 21_hogares_por_consumo.R ----------------------------------------------------------
# How many homes sit on each side of the payback lines, 2026.
# 1. Payback by monthly consumption, every tariff, at 2026 prices, whole panels (rules of
#    script 20): a curve per tariff, stored for the figure.
# 2. ENIGH 2024 electricity spending (clave 045100, gastoshogar) turned into kWh through
#    the 2024 bill of the home's municipal main tariff, including the DAC step. Checked
#    against CFE's mean kWh by tariff. Survey spending is what the home reports paying.
# 3. Each home gets the payback of its consumption.
#
# Run from the post root: Rscript scripts/21_hogares_por_consumo.R
# Needs ENIGH_HH (folder of concentradohogar.csv and gastoshogar.csv) in .Renviron.
# Output: data/derived/payback/payback_por_consumo.csv, hogares_por_payback.csv

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

readRenviron(".Renviron")
OUT <- "data/derived/payback"
TARIFFS <- c("1", "1A", "1B", "1C", "1D", "1E", "1F")
IVA <- 1.16; MIN_KWH <- 25; SUMMER <- 4:9; REGION <- "Central"
MIN_PANELES <- 4; PASO_PANELES <- 2
VIDA <- 25                                  # years, the panel warranty installers quote

det <- read_csv("data/derived/precios_hogar_detalle.csv", show_col_types = FALSE,
                col_types = cols(parte = "c", captura = "c", region = "c", otros_valores = "c")) |>
  filter(anio %in% c(2024, 2026)) |>
  mutate(dia = as.integer(coalesce(str_match(parte, "^(\\d+)")[, 2], "1"))) |>
  group_by(tarifa, region, temporada, esquema, bloque, anio, mes) |>
  slice_max(dia, n = 1, with_ties = FALSE) |> ungroup() |>
  mutate(hasta = coalesce(kwh_hasta, Inf), desde0 = kwh_desde - 1)
hog <- det |> filter(tarifa != "DAC") |> split(~ tarifa + anio + mes + temporada, drop = TRUE, sep = "|")
season_of <- function(t, m) if (t == "1") "todo el año" else if (m %in% SUMMER) "verano" else "fuera de verano"
bill_t <- function(q, t, y, m) {
  b <- hog[[paste(t, y, m, season_of(t, m), sep = "|")]]
  q <- max(q, MIN_KWH)
  sum(pmax(0, pmin(q, b$hasta) - b$desde0) * b$precio) * IVA
}
dac_raw <- det |> filter(tarifa == "DAC")
dac_m <- function(y, m) {
  mm <- max(dac_raw$mes[dac_raw$anio == y & dac_raw$mes <= m & dac_raw$bloque != "cargo_fijo"])
  d <- dac_raw[dac_raw$anio == y & dac_raw$mes == mm, ]
  c(fijo = d$precio[d$bloque == "cargo_fijo"], p = d$precio[d$bloque == "energia" & d$region == REGION])
}
DAC <- map(c(`2024` = 2024, `2026` = 2026), \(y) map(1:12, \(m) dac_m(y, m)))
bill_d <- function(q, y, m) { d <- DAC[[as.character(y)]][[m]]; unname(d["fijo"] + q * d["p"]) * IVA }

shape <- read_csv(file.path(OUT, "perfil_mensual_tarifa.csv"), show_col_types = FALSE)
shape_of <- function(t) { s <- shape[shape$tarifa == t, ]; s$indice[match(c(2:12, 1), s$mes)] }
yield <- read_csv(file.path(OUT, "yield_tarifa_mes.csv"), show_col_types = FALSE)
yield_of <- function(t) { d <- yield[yield$tarifa == t, ]; d$kwh_kwp[order(d$mes)] }
lim <- read_csv("data/derived/dac_limites.csv", col_types = "cd")
limite <- setNames(lim$limite_dac_kwh_mes, lim$tarifa)
p26 <- read_csv("data/derived/serie_precio_instalado.csv", show_col_types = FALSE) |> filter(anio == 2026)
w26 <- read_csv("data/derived/panel_w_anual.csv", show_col_types = FALSE) |> filter(anio == 2026) |> pull(w)
stopifnot(nrow(p26) == 1, length(w26) == 1, setequal(names(limite), TARIFFS))

billed_year <- function(c12, gen) {
  credit <- 0; q <- numeric(12)
  for (y in 1:2) for (m in 1:12) { net <- c12[m] - gen[m] - credit; q[m] <- max(net, 0); credit <- max(-net, 0) }
  q
}

# 1. payback by consumption ----------------------------------------------------------
CG <- c(seq(25, 500, 5), seq(510, 1500, 10), seq(1525, 6000, 25))
curva <- expand_grid(tarifa = TARIFFS, kwh_mes = CG) |>
  pmap(\(tarifa, kwh_mes) {
    t <- tarifa; c12 <- kwh_mes * shape_of(t); gy <- yield_of(t)
    en_dac <- kwh_mes > limite[[t]]
    billf <- \(q, dac) sum(map_dbl(1:12, \(m) if (dac) bill_d(q[m], 2026, m) else bill_t(q[m], t, 2026, m)))
    b0 <- billf(c12, en_dac)
    n <- max(MIN_PANELES, PASO_PANELES * ceiling(sum(c12) / sum(gy) * 1000 / w26 / PASO_PANELES))
    s <- n * w26 / 1000
    q <- billed_year(c12, s * gy)
    # with DAC: stays on DAC (cautious) or leaves it, if the 12-month average on net consumption is the rule
    ah_q <- b0 - billf(q, en_dac)
    ah_s <- b0 - billf(q, en_dac && mean(q) > limite[[t]])
    tibble(tarifa = t, kwh_mes, en_dac, paneles = n, kwp = s, recibo = b0, costo = s * 1000 * p26$pesos * IVA,
           ahorro = ah_q, pb = costo / ahorro, pb_sale_dac = costo / ah_s)
  }) |> list_rbind()
write_csv(curva, file.path(OUT, "payback_por_consumo.csv"))

# 2. spending to kWh -----------------------------------------------------------------
enigh <- path.expand(Sys.getenv("ENIGH_HH"))
if (enigh == "") enigh <- path.expand(sub("^ENIGH_HH=", "", grep("^ENIGH_HH=", readLines("../01-mexico-fiscal-profiles/.Renviron"), value = TRUE)))
tmp <- tempfile(fileext = ".csv")
system2("sh", c("-c", shQuote(sprintf("LC_ALL=C awk -F, 'NR==1 || $3==\"045100\"' %s > %s",
                                      shQuote(file.path(enigh, "gastoshogar.csv")), tmp))))
gas <- read_csv(tmp, col_types = cols(.default = "c")) |>
  transmute(folioviv, foliohog, gasto_tri = as.numeric(gasto_tri)) |>
  group_by(folioviv, foliohog) |> summarise(gasto_tri = sum(gasto_tri, na.rm = TRUE), .groups = "drop")
con <- read_csv(file.path(enigh, "concentradohogar.csv"), col_types = cols(.default = "c"),
                col_select = c(folioviv, foliohog, ubica_geo, factor, tot_integ, ing_cor)) |>
  mutate(factor = as.numeric(factor), cvegeo = str_pad(ubica_geo, 5, pad = "0"))
stopifnot(nrow(con) > 80000, !anyNA(con$factor))
mun <- read_csv("data/derived/mun_tarifas_anual.csv", col_types = cols(cvegeo = "c")) |>
  filter(year == 2024) |> select(cvegeo, tarifa_principal)
h <- con |> left_join(gas, by = c("folioviv", "foliohog")) |> left_join(mun, by = "cvegeo")
cat(sprintf("Homes %d; with an electricity line %.1f%% (weighted); municipality matched %.1f%%\n", nrow(h),
            100 * sum(h$factor[!is.na(h$gasto_tri) & h$gasto_tri > 0]) / sum(h$factor),
            100 * sum(h$factor[!is.na(h$tarifa_principal)]) / sum(h$factor)))
h <- h |> filter(!is.na(gasto_tri), gasto_tri > 0, tarifa_principal %in% TARIFFS) |>
  mutate(pesos_mes = gasto_tri / 3)

# the 2024 monthly bill of each tariff on a grid, tariff and DAC sides of the limit
bg <- expand_grid(tarifa = TARIFFS, kwh_mes = c(seq(5, 500, 5), seq(510, 1500, 10), seq(1525, 12000, 25))) |>
  mutate(bill = map2_dbl(tarifa, kwh_mes, \(t, k) {
    c12 <- k * shape_of(t)
    mean(map_dbl(1:12, \(m) if (k > limite[[t]]) bill_d(c12[m], 2024, m) else bill_t(c12[m], t, 2024, m)))
  }))
to_kwh <- function(pesos, t) {
  g <- bg[bg$tarifa == t, ]
  g <- g[order(g$kwh_mes), ]
  sub <- g[g$kwh_mes <= limite[[t]], ]; top <- g[g$kwh_mes > limite[[t]], ]
  out <- approx(sub$bill, sub$kwh_mes, xout = pesos, rule = 2, ties = "ordered")$y
  # between the tariff's top bill and DAC's first bill the home sits at the limit
  hi <- pesos >= min(top$bill)
  out[hi] <- approx(top$bill, top$kwh_mes, xout = pesos[hi], rule = 2)$y
  out
}
h <- h |> group_by(tarifa_principal) |> mutate(kwh_mes = to_kwh(pesos_mes, first(tarifa_principal))) |> ungroup()

chk <- h |> group_by(tarifa = tarifa_principal) |>
  summarise(enigh_kwh = round(weighted.mean(kwh_mes, factor)), mediana = round(median(kwh_mes)),
            homes_m = round(sum(factor) / 1e6, 1), .groups = "drop") |>
  left_join(read_csv("data/derived/nacional_tarifas_anual.csv", show_col_types = FALSE) |>
              filter(year == 2024) |> transmute(tarifa, cfe_kwh = round(kwh_mes_cliente), cfe_m = round(clientes_dic / 1e6, 1)), by = "tarifa")
print(as.data.frame(chk), row.names = FALSE)

# 3. payback of each home ------------------------------------------------------------
# Survey kWh run above CFE's metered mean (homes with no electricity line drop out, and
# the reported payment can carry street lighting and arrears), so the headline scales each
# tariff's homes to CFE's mean; the unscaled result is kept beside it.
pb_de <- function(k, t) {
  g <- curva[curva$tarifa == t, ]
  g$pb[findInterval(k, g$kwh_mes, all.inside = TRUE)]
}
escala <- setNames(chk$cfe_kwh / chk$enigh_kwh, chk$tarifa)
h <- h |>
  mutate(kwh_cal = kwh_mes * escala[tarifa_principal]) |>
  group_by(tarifa_principal) |>
  mutate(pb_sin = pb_de(kwh_mes, first(tarifa_principal)), pb_cal = pb_de(kwh_cal, first(tarifa_principal))) |>
  ungroup()
CORTES <- c(0, 5, 10, VIDA, Inf); ETIQ <- c("menos de 5", "5 a 10", "10 a 25", "mas de 25")
reparte <- function(x, ver) {
  h |> mutate(grupo = cut(.data[[x]], CORTES, labels = ETIQ)) |> group_by(grupo) |>
    summarise(hogares = sum(factor), .groups = "drop") |> mutate(pct = 100 * hogares / sum(hogares), version = ver)
}
stopifnot(!anyNA(h$pb_sin), !anyNA(h$pb_cal))
saveRDS(h |> transmute(tarifa = tarifa_principal, factor, kwh_cal, pb_cal,
                       tot_integ = as.numeric(tot_integ), ing_cor = as.numeric(ing_cor)),
        file.path(OUT, "hogares_kwh.rds"))                   # for script 24
tot <- bind_rows(reparte("pb_cal", "ajustada a CFE"), reparte("pb_sin", "sin ajustar"))
write_csv(tot, file.path(OUT, "hogares_por_payback.csv"))
print(as.data.frame(tot |> mutate(hogares_m = round(hogares / 1e6, 1), pct = round(pct, 1)) |> select(version, grupo, hogares_m, pct)), row.names = FALSE)
cat("By tariff, percent of homes, scaled to CFE\n")
por_t <- h |> mutate(grupo = cut(pb_cal, CORTES, labels = ETIQ)) |> group_by(tarifa_principal, grupo) |>
  summarise(w = sum(factor), .groups = "drop") |> group_by(tarifa_principal) |> mutate(p = round(100 * w / sum(w)))
write_csv(por_t, file.path(OUT, "hogares_por_payback_tarifa.csv"))
print(as.data.frame(por_t |> select(-w) |> pivot_wider(names_from = grupo, values_from = p, values_fill = 0)), row.names = FALSE)
cat(sprintf("Homes above 140 kWh a month, tariff 1, scaled: %.1f%%\n",
            100 * sum(h$factor[h$tarifa_principal == "1" & h$kwh_cal > 140]) / sum(h$factor[h$tarifa_principal == "1"])))
cat("\nBreak-even consumption, kWh a month, 2026\n")
print(as.data.frame(curva |> group_by(tarifa) |>
  summarise(pb25 = min(kwh_mes[pb <= 25]), pb10 = min(kwh_mes[pb <= 10]), pb5 = min(kwh_mes[pb <= 5]), .groups = "drop")), row.names = FALSE)
cat("\nHomes above the DAC limit: survey", round(100 * sum(h$factor[h$kwh_mes > limite[h$tarifa_principal]]) / sum(h$factor), 2),
    "%, scaled", round(100 * sum(h$factor[h$kwh_cal > limite[h$tarifa_principal]]) / sum(h$factor), 2), "%\n")
print(as.data.frame(curva |> filter(tarifa == "1", kwh_mes %in% c(90, 140, 175, 200, 250, 255, 300, 400, 600)) |>
  mutate(across(c(recibo, costo, ahorro), round), across(c(pb, pb_sale_dac), ~ round(.x, 1)))), row.names = FALSE)
