# 29_fig_mismos_paneles.R -----------------------------------------------------------
# Figure 3 of the thread: years for the average home of each tariff to recover the same
# four-panel system, 2026, at today's tariffs (script 20, smallest package) and at the
# regulator's cost-based tariff. A panel saves the energy charge only, so the fixed
# charge stays; same generation and credit rule as scripts 20 and 21; DAC's home uses
# tariff 1's sun. Every home keeps its consumption. The same home at the DAC step comes
# from script 21: it holds consumption nearly fixed, which the tariff averages do not.
# Run from the post root, after 20 and 21: Rscript scripts/29_fig_mismos_paneles.R
# Output: data/derived/tarifa_costo/payback_cuatro_paneles.csv, images/f3_mismos_paneles_*
# FIG_LANG=en writes the English version.

library(dplyr)
library(ggplot2)
library(purrr)
library(readr)
library(tidyr)

source("../_style/style.R")
source("scripts/_fuentes.R")
IVA <- 1.16; PB <- "data/derived/payback"

d <- read_csv(file.path(PB, "payback_anual.csv"), show_col_types = FALSE) |>
  filter(anio == 2026, regla == "paquete_minimo") |>
  transmute(tarifa, kwh_mes, paneles, kwp, costo, ahorro_ano, pb)
paso <- read_csv(file.path(PB, "payback_por_consumo.csv"), show_col_types = FALSE) |>
  filter(tarifa == "1", kwh_mes %in% c(250, 255)) |> arrange(kwh_mes)
ORD <- c("1", "1A", "1B", "1C", "1D", "1E", "1F", "DAC")
stopifnot(setequal(d$tarifa, ORD), all(d$paneles == 4), n_distinct(round(d$costo)) == 1,
          nrow(paso) == 2, !paso$en_dac[1], paso$en_dac[2], identical(paso$paneles[1], paso$paneles[2]))

shape <- read_csv(file.path(PB, "perfil_mensual_tarifa.csv"), show_col_types = FALSE)
yield <- read_csv(file.path(PB, "yield_tarifa_mes.csv"), show_col_types = FALSE)
shape_of <- function(t) { s <- shape[shape$tarifa == t, ]; s$indice[match(c(2:12, 1), s$mes)] }
yield_of <- function(t) { x <- yield[yield$tarifa == t, ]; x$kwh_kwp[order(x$mes)] }
billed_year <- function(c12, gen) {
  credit <- 0; q <- numeric(12)
  for (y in 1:2) for (m in 1:12) { net <- c12[m] - gen[m] - credit; q[m] <- max(net, 0); credit <- max(-net, 0) }
  q
}
cne <- read_csv("data/derived/cne_db1_db2_2026_septiembre.csv", show_col_types = FALSE) |>
  group_by(cat, unidades) |> summarise(total = median(total), .groups = "drop")
tar <- \(ca) cne$total[cne$cat == ca & cne$unidades == "$/kWh"]
stopifnot(length(c(tar("DB1"), tar("DB2"))) == 2)

d <- d |> mutate(ahorro_costo = pmap_dbl(list(tarifa, kwh_mes, kwp), \(t, k, s) {
  c12 <- k * shape_of(t); q <- billed_year(c12, s * yield_of(if (t == "DAC") "1" else t))
  (sum(c12) - sum(q)) * (if (k <= 150) tar("DB1") else tar("DB2")) * IVA
}), pb_costo = costo / ahorro_costo)
stopifnot(!anyNA(d$pb_costo))
write_csv(d, "data/derived/tarifa_costo/payback_cuatro_paneles.csv")
print(as.data.frame(d |> select(tarifa, kwh_mes, kwp, costo, ahorro_ano, pb, ahorro_costo, pb_costo) |> mutate(across(where(is.double), ~ round(.x, 2)))))
print(as.data.frame(paso |> select(kwh_mes, en_dac, paneles, costo, ahorro, pb)))

g <- d |> select(tarifa, actual = pb, costo = pb_costo) |> pivot_longer(-tarifa, names_to = "precio", values_to = "anos") |>
  mutate(tarifa = factor(tarifa, ORD), precio = factor(precio, c("actual", "costo")))
fila <- \(t, col) d[[col]][d$tarifa == t]
fmt <- \(x) ifelse(x < 3, sprintf("%.1f", x), sprintf("%.0f", x))
rango <- range(d$kwh_mes[d$tarifa != "DAC"]); kd <- fila("DAC", "kwh_mes")

# Share of residential customers on each tariff, December 2025, shown under each tariff
cli <- read_csv("data/derived/nacional_tarifas_anual.csv", show_col_types = FALSE) |>
  filter(year == 2025) |> transmute(tarifa, pct = 100 * clientes_dic / sum(clientes_dic))
stopifnot(setequal(cli$tarifa, ORD), abs(sum(cli$pct) - 100) < 1e-9)
pct_de <- setNames(cli$pct, cli$tarifa)[ORD]
print(round(pct_de, 1))
nom <- setNames(ORD, ORD); nom["DAC"] <- es_en("DAC: alto consumo,\nsin subsidio", "DAC: high use,\nno subsidy")
etiq <- setNames(paste0(nom, "\n(", ifelse(pct_de < 1, sprintf("%.1f%%", pct_de), sprintf("%.0f%%", pct_de)), ")"), ORD)

make <- function(mode) {
  cc <- pal(mode)
  ggplot(g, aes(x = tarifa, y = anos, fill = precio)) +
    geom_hline(yintercept = 25, colour = cc$ink, linetype = "dotted", linewidth = 0.5) +
    geom_col(position = position_dodge(0.76), width = 0.72) +
    geom_text(aes(label = fmt(anos), colour = precio), position = position_dodge(0.76), vjust = -0.5, size = 3.8,
              family = STYLE_FONT, fontface = "bold") +
    annotate("label", x = 8.45, y = 25, hjust = 1, colour = cc$ink, fill = cc$bg, size = 3.8, family = STYLE_FONT, linewidth = 0,
             label = es_en("Garantía de los paneles: 25 años", "Panel warranty: 25 years")) +
    annotate("text", x = 5.2, y = 46, hjust = 0, size = 3.9, family = STYLE_FONT, colour = cc$accent,
             label = es_en("Con la tarifa actual", "At today's tariff")) +
    annotate("text", x = 5.2, y = 41.5, hjust = 0, size = 3.9, family = STYLE_FONT, colour = cc$primary,
             label = es_en("Sin subsidio", "With no subsidy")) +
    scale_fill_manual(values = c(actual = cc$accent, costo = cc$primary)) +
    scale_colour_manual(values = c(actual = cc$accent, costo = cc$primary)) +
    scale_y_continuous(limits = c(0, 54), breaks = seq(0, 50, 10), expand = c(0, 0)) +
    scale_x_discrete(labels = etiq) +
    labs(title = es_en(sprintf("Un sistema fotovoltaico pequeño se paga en\n%.0f años en la tarifa más común", fila("1", "pb")),
                       sprintf("A small solar system takes %.0f years\nto pay back on the most common tariff", fila("1", "pb"))),
         subtitle = es_en("Años para recuperar la inversión por tarifa, hogar promedio", "Years to recover the investment by tariff, average home"),
         x = es_en("Tarifa de la CFE (porcentaje de hogares)", "CFE tariff (share of homes)"), y = es_en("Años para recuperar la inversión", "Years to recover the investment"),
         caption = caption_ls(c(es_en(sprintf("Sistema: instalación de cuatro paneles con una capacidad de %.1f kW. Precio estimado de $%s con IVA, en todas las tarifas. Es el sistema más pequeño que suelen vender las instaladoras. Medición neta, sin batería ni mantenimiento. En 1F y DAC no cubre todo el consumo.", d$kwp[1], format(round(d$costo[1], -2), big.mark = ",", trim = TRUE)),
                                      sprintf("System: installation of four panels with a capacity of %.1f kW. Estimated price of %s pesos with tax, on every tariff. It is the smallest system installers usually sell. Net metering, no battery or maintenance. On 1F and DAC it does not cover all consumption.", d$kwp[1], format(round(d$costo[1], -2), big.mark = ",", trim = TRUE))),
                                es_en(sprintf("DAC (Doméstica de Alto Consumo): tarifa sin subsidio para quien supera un límite de consumo. Consumo: promedio de 2025 según la CFE, de %.0f a %.0f kWh al mes en las tarifas 1 a 1F y %.0f en DAC, región Central. El consumo también cambia entre tarifas, no solo el precio.", rango[1], rango[2], kd),
                                      sprintf("DAC (high-consumption household tariff): no subsidy, for homes above a consumption limit. Consumption: 2025 mean from CFE, %.0f to %.0f kWh a month on tariffs 1 to 1F and %.0f on DAC, Central region. Consumption also differs between tariffs, not only price.", rango[1], rango[2], kd)),
                                es_en(paste0("Tarifa estimada sin subsidio: costo de la luz por región tarifaria, de acuerdo con la Comisión Nacional de Energía (CNE). Se usan las tarifas DB1 y DB2 del regulador."),
                                      paste0("Estimated unsubsidised tariff: the cost of power by tariff region, according to Mexico's National Energy Commission (CNE). Based on the regulator's DB1 and DB2 tariffs.")),
                                es_en("Hogares: clientes residenciales de la CFE en cada tarifa, diciembre de 2025.", "Homes: CFE residential customers on each tariff, December 2025.")),
                              paste(F_CFE, F_CNE, F_ATLAS, F_PAQUETES, F_PRECIO), width = 205)) +
    theme_ls(mode) + theme(plot.caption = element_text(lineheight = 1.1), axis.text.x = element_text(colour = cc$ink, size = rel(0.95)))
}
save_ls(make, "images/f3_mismos_paneles")
