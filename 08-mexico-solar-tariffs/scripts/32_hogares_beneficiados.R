# 32_hogares_beneficiados.R ----------------------------------------------------------
# Share of homes on tariffs 1 to 1F that pay LESS than the estimated unsubsidised bill,
# i.e. that actually receive a subsidy, before the clip at zero used in script 24.
# Checks the CFE director's "cerca del 89 % de los usuarios de tipo residencial"
# (Chamber of Deputies, 27 October 2025) against our 99.9% on subsidised tariffs.
# Same inputs and bill rules as script 24.
#
# Run from the post root, after 21: Rscript scripts/32_hogares_beneficiados.R
# Output: data/derived/tarifa_costo/beneficiados.csv

library(dplyr)
library(readr)

OUT <- "data/derived/tarifa_costo"; PB <- "data/derived/payback"
IVA <- 1.16

cne <- read_csv("data/derived/cne_db1_db2_2026_septiembre.csv", show_col_types = FALSE)
stopifnot(n_distinct(cne$division) == 17)
med <- cne |> group_by(cat, unidades) |> summarise(total = median(total), .groups = "drop")
tar <- \(ca, u) med$total[med$cat == ca & med$unidades == u]
p1 <- tar("DB1", "$/kWh"); p2 <- tar("DB2", "$/kWh"); f1 <- tar("DB1", "$/mes"); f2 <- tar("DB2", "$/mes")
stopifnot(length(c(p1, p2, f1, f2)) == 4)

curva <- read_csv(file.path(PB, "payback_por_consumo.csv"), show_col_types = FALSE)
h <- readRDS(file.path(PB, "hogares_kwh.rds"))
stopifnot(!anyNA(h$kwh_cal), !anyNA(h$factor))

recibo_de <- function(k, t) { g <- curva[curva$tarifa == t, ]; approx(g$kwh_mes, g$recibo, xout = k, rule = 2)$y }
bill_costo <- \(k) ifelse(k <= 150, f1 + k * p1, f2 + k * p2) * 12 * IVA

h <- h |> group_by(tarifa) |> mutate(recibo = recibo_de(kwh_cal, first(tarifa))) |> ungroup() |>
  mutate(gap = bill_costo(kwh_cal) - recibo)
stopifnot(!anyNA(h$gap))

res <- bind_rows(
  h |> group_by(tarifa) |> summarise(hogares_m = sum(factor) / 1e6, pct_beneficiados = 100 * sum(factor[gap > 0]) / sum(factor),
                                     kwh_umbral = suppressWarnings(min(kwh_cal[gap <= 0])), .groups = "drop"),
  h |> summarise(tarifa = "Todas", hogares_m = sum(factor) / 1e6, pct_beneficiados = 100 * sum(factor[gap > 0]) / sum(factor),
                 kwh_umbral = NA_real_))
print(res, n = Inf)
write_csv(res, file.path(OUT, "beneficiados.csv"))
