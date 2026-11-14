# 03_cne_cost_tariffs.R ---------------------------------------------------------
# The regulator's cost-based household tariffs (categories DB1, up to 150 kWh a
# month, and DB2, above) by CFE tariff division, for one month, from CNE's
# "Tarifas finales del Suministro Básico" file kept in the MLPV project.
# No household pays these: tariffs 1 to 1F and DAC are set by Hacienda instead.
# Writes data/derived/cne_db1_db2_<year>_<month>.csv and prints the range, and
# data/derived/cne_db1_db2_mensual.csv with every month from December 2017.
#
# Needs MLPV_IN in .Renviron (the MLPV project's in/ folder).
# Run from the post root: Rscript scripts/03_cne_cost_tariffs.R

library(dplyr)
library(readr)
library(tidyr)

YEAR <- "2026"
MONTH <- "Septiembre"

mlpv <- path.expand(Sys.getenv("MLPV_IN"))
if (!nzchar(mlpv)) stop("Set MLPV_IN in .Renviron")
f <- file.path(mlpv, "Electricity/cne_tarifas/raw/TarifasFinalesdeSuministroBasico.csv")

raw <- read_csv(f, col_names = FALSE, col_types = cols(.default = "c"),
                locale = locale(encoding = "latin1"))
div <- unlist(raw[1, ]); hdr <- unlist(raw[2, ])
div[div == ""] <- NA
div <- fill(data.frame(d = div), d)$d
tot <- which(hdr == "TOTAL")
stopifnot(length(tot) == 17)

d <- raw[-(1:2), ]
key <- d[, 1:7]
names(key) <- c("anio", "mes", "cat", "desc", "periodo", "cargo", "unidades")
key <- key |> mutate(across(everything(), ~ na_if(.x, ""))) |> fill(anio, mes, cat, desc)
vals <- d[, tot]
names(vals) <- div[tot]

MESES <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto",
           "Septiembre", "Octubre", "Noviembre", "Diciembre")
all_months <- bind_cols(key, vals) |>
  filter(cat %in% c("DB1", "DB2")) |>
  pivot_longer(-(anio:unidades), names_to = "division", values_to = "total") |>
  mutate(total = as.numeric(total), anio = as.integer(anio),
         mes_n = match(tolower(trimws(mes)), tolower(MESES))) |>
  select(anio, mes, mes_n, cat, cargo, unidades, division, total)
if (anyNA(all_months$mes_n)) stop("Unknown month label: ", paste(unique(all_months$mes[is.na(all_months$mes_n)]), collapse = ", "))
stopifnot(!anyNA(all_months$total))
write_csv(all_months, "data/derived/cne_db1_db2_mensual.csv")
print(all_months |> filter(unidades == "$/kWh") |> group_by(anio, cat) |>
        summarise(meses = n_distinct(mes_n), mediana = median(total), .groups = "drop") |>
        pivot_wider(names_from = cat, values_from = mediana))

out <- all_months |> filter(anio == as.integer(YEAR), mes == MONTH) |> select(-mes_n)
stopifnot(nrow(out) == 2 * 2 * 17, !anyNA(out$total))

write_csv(out, sprintf("data/derived/cne_db1_db2_%s_%s.csv", YEAR, tolower(MONTH)))
print(out |> group_by(cat, unidades) |>
        summarise(min = min(total), median = median(total), max = max(total), .groups = "drop"))
