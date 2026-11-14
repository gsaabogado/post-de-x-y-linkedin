# 24_tarifa_costo.R -----------------------------------------------------------------
# What changes if homes on tariffs 1 to 1F paid the regulator's cost-based tariff.
# Each home keeps its consumption (no price response) and pays DB1 up to 150 kWh a month
# or DB2 above, at the September 2026 median of CFE's 17 divisions, with IVA. The fixed
# charge stays with a panel, so a panel saves the energy charge only. Same systems and
# same net metering as script 21. The gap to the cost-based bill is NOT the fiscal
# subsidy, which is a different number.
#
# Run from the post root, after 21: Rscript scripts/24_tarifa_costo.R
# Output: data/derived/tarifa_costo/{hogares_por_payback.csv, deciles.csv, resumen.csv}

library(dplyr)
library(purrr)
library(readr)
library(tidyr)

OUT <- "data/derived/tarifa_costo"; PB <- "data/derived/payback"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
TARIFFS <- c("1", "1A", "1B", "1C", "1D", "1E", "1F")
IVA <- 1.16; MIN_PANELES <- 4; PASO_PANELES <- 2; VIDA <- 25

cne <- read_csv("data/derived/cne_db1_db2_2026_septiembre.csv", show_col_types = FALSE)
stopifnot(n_distinct(cne$division) == 17)
med <- cne |> group_by(cat, unidades) |> summarise(total = median(total), .groups = "drop")
tar <- \(ca, u) med$total[med$cat == ca & med$unidades == u]
p1 <- tar("DB1", "$/kWh"); p2 <- tar("DB2", "$/kWh"); f1 <- tar("DB1", "$/mes"); f2 <- tar("DB2", "$/mes")
stopifnot(length(c(p1, p2, f1, f2)) == 4)

curva <- read_csv(file.path(PB, "payback_por_consumo.csv"), show_col_types = FALSE)
h <- readRDS(file.path(PB, "hogares_kwh.rds"))
stopifnot(!anyNA(h$kwh_cal), !anyNA(h$ing_cor), !anyNA(h$tot_integ))

# Same system, same generation and same credit rule as script 21
shape <- read_csv(file.path(PB, "perfil_mensual_tarifa.csv"), show_col_types = FALSE)
yield <- read_csv(file.path(PB, "yield_tarifa_mes.csv"), show_col_types = FALSE)
shape_of <- function(t) { s <- shape[shape$tarifa == t, ]; s$indice[match(c(2:12, 1), s$mes)] }
yield_of <- function(t) { d <- yield[yield$tarifa == t, ]; d$kwh_kwp[order(d$mes)] }
w26 <- read_csv("data/derived/panel_w_anual.csv", show_col_types = FALSE) |> filter(anio == 2026) |> pull(w)
stopifnot(length(w26) == 1)
billed_year <- function(c12, gen) {
  credit <- 0; q <- numeric(12)
  for (y in 1:2) for (m in 1:12) { net <- c12[m] - gen[m] - credit; q[m] <- max(net, 0); credit <- max(-net, 0) }
  q
}

cf <- curva |> filter(kwh_mes <= 1500) |> select(tarifa, kwh_mes, paneles, kwp, costo, pb, recibo) |>
  mutate(ahorro_costo = pmap_dbl(list(tarifa, kwh_mes, kwp), \(t, k, s) {
    c12 <- k * shape_of(t); q <- billed_year(c12, s * yield_of(t))
    (sum(c12) - sum(q)) * (if (k <= 150) p1 else p2) * IVA
  }), pb_costo = costo / ahorro_costo)

en_rejilla <- function(k, t, col) { g <- cf[cf$tarifa == t, ]; g[[col]][findInterval(k, g$kwh_mes, all.inside = TRUE)] }
recibo_de <- function(k, t) { g <- curva[curva$tarifa == t, ]; approx(g$kwh_mes, g$recibo, xout = k, rule = 2)$y }
bill_costo <- \(k) ifelse(k <= 150, f1 + k * p1, f2 + k * p2) * 12 * IVA

h <- h |> group_by(tarifa) |>
  mutate(pb_costo = en_rejilla(kwh_cal, first(tarifa), "pb_costo"), recibo = recibo_de(kwh_cal, first(tarifa))) |>
  ungroup() |>
  mutate(brecha = pmax(bill_costo(kwh_cal) - recibo, 0), y_aj = ing_cor / sqrt(tot_integ))
stopifnot(!anyNA(h$pb_costo), !anyNA(h$brecha))

# Homes by years to recover ----------------------------------------------------------
CORTES <- c(0, 5, 10, VIDA, Inf); ETIQ <- c("menos de 5", "5 a 10", "10 a 25", "mas de 25")
reparte <- function(x, ver) h |> mutate(grupo = cut(.data[[x]], CORTES, labels = ETIQ)) |>
  group_by(grupo) |> summarise(hogares = sum(factor), .groups = "drop") |>
  mutate(pct = 100 * hogares / sum(hogares), version = ver)
tab <- bind_rows(reparte("pb_cal", "tarifas actuales"), reparte("pb_costo", "tarifa de costo"))
write_csv(tab, file.path(OUT, "hogares_por_payback.csv"))
print(as.data.frame(tab |> mutate(hogares_m = round(hogares / 1e6, 1), pct = round(pct, 1)) |> select(-hogares)))

# Gap to the cost-based bill by tenth of income adjusted by size ---------------------
o <- order(h$y_aj); acum <- cumsum(h$factor[o]) / sum(h$factor)
h$decil <- NA_integer_; h$decil[o] <- pmin(10L, ceiling(acum * 10))
dec <- h |> group_by(decil) |>
  summarise(hogares = sum(factor), kwh = weighted.mean(kwh_cal, factor), brecha_hogar = weighted.mean(brecha, factor),
            brecha_total = sum(brecha * factor), ingreso_anual = weighted.mean(ing_cor, factor) * 4, .groups = "drop") |>
  mutate(pct = 100 * brecha_total / sum(brecha_total), pct_ingreso = 100 * brecha_hogar / ingreso_anual)
stopifnot(nrow(dec) == 10, abs(sum(dec$pct) - 100) < 1e-9)
write_csv(dec, file.path(OUT, "deciles.csv"))
print(as.data.frame(dec |> mutate(across(where(is.double), ~ round(.x, 1)))))

# Average price paid, whole bill over kWh, all homes and the top income tenth
precio_de <- \(d) sum(d$recibo * d$factor) / sum(d$kwh_cal * 12 * d$factor)
precios <- tibble(pesos_todos = precio_de(h), pesos_d10 = precio_de(h[h$decil == 10, ]),
                  kwh_todos = weighted.mean(h$kwh_cal, h$factor),
                  kwh_d10 = weighted.mean(h$kwh_cal[h$decil == 10], h$factor[h$decil == 10]))
write_csv(precios, file.path(OUT, "precios_grupo.csv"))
print(as.data.frame(precios))

t1 <- cf |> filter(tarifa == "1", kwh_mes == 90)
costo_4p <- t1$costo
res <- tibble(brecha_total_mm = sum(h$brecha * h$factor) / 1e6, hogares_m = sum(h$factor) / 1e6,
              brecha_hogar = weighted.mean(h$brecha, h$factor),
              pb_t1_90_actual = t1$pb, pb_t1_90_costo = t1$pb_costo, costo_paquete = costo_4p,
              sistemas_m = sum(h$brecha * h$factor) / costo_4p / 1e6,
              db1_kwh = p1, db2_kwh = p2, db1_mes = f1)
write_csv(res, file.path(OUT, "resumen.csv"))

# Targeting: the poorest k tenths keep today's tariffs, everyone else pays the cost-based tariff
foco <- map(c(0, 3, 4, 5, 6), \(k) {
  pb <- if_else(h$decil <= k, h$pb_cal, h$pb_costo)
  tibble(decil_protegido = k,
         pct_mas_25 = 100 * sum(h$factor[pb > VIDA]) / sum(h$factor),
         pct_menos_10 = 100 * sum(h$factor[pb <= 10]) / sum(h$factor),
         pct_brecha_que_queda = 100 * sum((h$brecha * h$factor)[h$decil <= k]) / sum(h$brecha * h$factor),
         ahorro_mm = sum((h$brecha * h$factor)[h$decil > k]) / 1e6)
}) |> list_rbind()
write_csv(foco, file.path(OUT, "focalizacion.csv"))
print(as.data.frame(foco |> mutate(across(everything(), ~ round(.x, 1)))))
print(as.data.frame(res |> mutate(across(everything(), ~ round(.x, 2)))))
