# 26_fig_tarifa_costo.R -------------------------------------------------------------
# Figures 4 and 5 of the thread: homes by years to recover the investment, current
# tariffs against the regulator's cost-based tariff, and the gap by income tenth.
# Run from the post root, after 24: Rscript scripts/26_fig_tarifa_costo.R
# FIG_LANG=en writes the English version.

library(dplyr)
library(ggplot2)
library(purrr)
library(readr)

source("../_style/style.R")
source("scripts/_fuentes.R")
dir.create("images", showWarnings = FALSE)

t4 <- read_csv("data/derived/tarifa_costo/hogares_por_payback.csv", show_col_types = FALSE)
dec <- read_csv("data/derived/tarifa_costo/deciles.csv", show_col_types = FALSE)
res <- read_csv("data/derived/tarifa_costo/resumen.csv", show_col_types = FALSE)
stopifnot(nrow(t4) == 8, nrow(dec) == 10, nrow(res) == 1)

NOTA_CONS <- es_en("Consumo: gasto en luz de la ENIGH 2024, ajustado al consumo promedio de la CFE por tarifa. Hogares en tarifas 1 a 1F.",
                   "Consumption: electricity spending in ENIGH 2024, scaled to CFE's mean consumption by tariff. Homes on tariffs 1 to 1F.")
NOTA_COSTO <- es_en("Tarifa estimada sin subsidio: costo de la luz por región tarifaria, de acuerdo con la Comisión Nacional de Energía (CNE).",
                    "Estimated unsubsidised tariff: the cost of power by tariff region, according to Mexico's National Energy Commission (CNE).")
CAP <- theme(plot.caption = element_text(lineheight = 1.1))

# F4 ---------------------------------------------------------------------------------
# Share of homes that use the four-panel package, from the home table of script 21
cur <- read_csv("data/derived/payback/payback_por_consumo.csv", show_col_types = FALSE)
hog <- readRDS("data/derived/payback/hogares_kwh.rds")
pan <- map2_dbl(hog$tarifa, hog$kwh_cal, \(t, k) { g <- cur[cur$tarifa == t, ]; g$paneles[findInterval(k, g$kwh_mes, all.inside = TRUE)] })
pct4 <- 100 * sum(hog$factor[pan == 4]) / sum(hog$factor)
cat(sprintf("Homes with four panels: %.1f%%\n", pct4))
stopifnot(pct4 > 90)
GR <- c("menos de 5", "5 a 10", "10 a 25", "mas de 25")
ET <- c(es_en("Menos de 5 años", "Under 5 years"), es_en("5 a 10 años", "5 to 10 years"), es_en("10 a 25 años", "10 to 25 years"),
        es_en("Más de 25 años, la garantía", "Over 25 years, the warranty"))
d4 <- t4 |> mutate(grupo = factor(grupo, GR, ET), version = factor(version, c("tarifas actuales", "tarifa de costo")))
stopifnot(!anyNA(d4$grupo), !anyNA(d4$version))
rec <- d4 |> filter(grupo != ET[4]) |> group_by(version) |> summarise(p = sum(pct), .groups = "drop")
rec_costo <- rec$p[rec$version == "tarifa de costo"]; rec_act <- rec$p[rec$version == "tarifas actuales"]
f4 <- function(mode) {
  cc <- pal(mode)
  ggplot(d4, aes(x = grupo, y = pct, fill = version)) +
    geom_col(position = position_dodge(0.76), width = 0.72) +
    geom_text(aes(label = ifelse(pct < 1, sprintf("%.1f%%", pct), sprintf("%.0f%%", pct)), colour = version),
              position = position_dodge(0.76), vjust = -0.5,
              size = 4, family = STYLE_FONT, fontface = "bold") +
    annotate("text", x = 0.62, y = 73, hjust = 0, size = 3.9, family = STYLE_FONT, colour = cc$accent,
             label = es_en("Tarifas actuales", "Current tariffs")) +
    annotate("text", x = 0.62, y = 66.5, hjust = 0, size = 3.9, family = STYLE_FONT, colour = cc$primary,
             label = es_en("Tarifa estimada sin subsidio", "Estimated unsubsidised tariff")) +
    scale_fill_manual(values = c("tarifas actuales" = cc$accent, "tarifa de costo" = cc$primary)) +
    scale_colour_manual(values = c("tarifas actuales" = cc$accent, "tarifa de costo" = cc$primary)) +
    scale_y_continuous(limits = c(0, 80), breaks = seq(0, 75, 25), labels = \(x) paste0(x, "%"), expand = c(0, 0)) +
    labs(title = es_en("Sin subsidio, la inversión se pagaría en menos de\n25 años en la mayoría de los hogares",
                       "With no subsidy, the investment would pay back\nwithin 25 years in most homes"),
         subtitle = es_en("Hogares según los años para pagar sus paneles", "Homes by years to pay for their panels"),
         x = es_en("Años para recuperar la inversión", "Years to recover the investment"), y = es_en("Porcentaje de hogares", "Share of homes"),
         caption = caption_ls(c(es_en(sprintf("Sistema: para el %.0f%% de las casas se usa una instalación de cuatro paneles con una capacidad de 2.4 kW y un precio de $%s con IVA. Esta es la instalación más chica que suelen vender las instaladoras. Para los hogares que consumen más de unos 350 kWh al mes se usa una instalación de seis paneles o más. Precios de instalación estimados para 2026.", pct4, format(round(res$costo_paquete, -2), big.mark = ",", trim = TRUE)),
                                      sprintf("System: %.0f%% of homes use an installation of four panels with a capacity of 2.4 kW and a price of %s pesos with tax. This is the smallest installation installers usually sell. Homes using more than about 350 kWh a month use an installation of six panels or more. Installed prices estimated for 2026.", pct4, format(round(res$costo_paquete, -2), big.mark = ",", trim = TRUE))),
                                es_en("Años para recuperar: costo del sistema entre el ahorro anual. Sin intereses, batería, mantenimiento ni desgaste de los paneles. Con medición neta: lo que los paneles envían a la red se resta del consumo, con créditos por 12 meses. Generación de cada tarifa según el Global Solar Atlas.",
                                      "Years to recover: system cost over yearly savings. No interest, battery, maintenance or panel wear. With net metering: what panels send to the grid is subtracted from consumption, with credits for 12 months. Output of each tariff from the Global Solar Atlas."),
                                es_en("Tarifa estimada sin subsidio: costo de la luz por región tarifaria, de acuerdo con la Comisión Nacional de Energía (CNE).",
                                      "Estimated unsubsidised tariff: the cost of power by tariff region, according to Mexico's National Energy Commission (CNE)."),
                                es_en(sprintf("Consumo: gasto en luz de la ENIGH 2024, ajustado al consumo promedio de la CFE por tarifa. Cada casa recibe la tarifa principal de su municipio. Son %.1f millones de hogares en tarifas 1 a 1F que reportan gasto en luz.", res$hogares_m),
                                      sprintf("Consumption: electricity spending in ENIGH 2024, scaled to CFE's mean consumption by tariff. Each home gets the main tariff of its municipality. These are %.1f million homes on tariffs 1 to 1F that report electricity spending.", res$hogares_m))),
                              paste(F_CFE, F_CNE, F_ENIGH, F_ATLAS, F_PAQUETES, F_PRECIO), width = 205)) +
    theme_ls(mode) + CAP + theme(axis.text.x = element_text(colour = cc$ink, size = rel(0.92)))
}
save_ls(f4, "images/f4_anios_recuperar")

# F5 ---------------------------------------------------------------------------------
pobre <- sum(dec$pct[dec$decil <= 2]); rico <- sum(dec$pct[dec$decil >= 9])
d5 <- dec |> mutate(g = if_else(decil <= 2 | decil >= 9, "punta", "medio"))
f5 <- function(mode) {
  cc <- pal(mode)
  ggplot(d5, aes(x = factor(decil), y = pct, fill = g)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 10, colour = cc$ink, linetype = "dotted", linewidth = 0.5) +
    geom_label(aes(label = sprintf("%.1f%%", pct)), fill = cc$bg, colour = cc$ink, size = 3.8, family = STYLE_FONT,
               vjust = 0, nudge_y = 0.08, linewidth = 0, label.padding = unit(1.5, "pt")) +
    annotate("text", x = 0.5, y = 10.15, hjust = 0, vjust = 0, size = 3.6, family = STYLE_FONT, colour = cc$sub,
             label = es_en("Reparto igual", "Equal split")) +
    scale_fill_manual(values = c(punta = cc$accent, medio = cc$primary)) +
    scale_y_continuous(limits = c(0, 13), breaks = seq(0, 12, 4), labels = \(x) paste0(x, "%"), expand = c(0, 0)) +
    scale_x_discrete(labels = c(es_en("1\nMenor ingreso", "1\nLowest income"), 2:9, es_en("10\nMayor ingreso", "10\nHighest income"))) +
    labs(title = es_en("El subsidio no se dirige a los más pobres", "The subsidy is not aimed at the poorest"),
         subtitle = es_en("Reparto del subsidio estimado por decil de ingreso",
                          "Share of the estimated subsidy by income decile"),
         x = es_en("Decil de hogares por ingreso ajustado por tamaño", "Decile of homes by income adjusted for size"), y = es_en("Porcentaje del subsidio estimado\nque recibe cada grupo", "Share of the estimated subsidy\neach group receives"),
         caption = caption_ls(c(es_en("El subsidio depende del clima y del consumo, no del ingreso.", "The subsidy depends on climate and consumption, not on income."),
                                es_en(sprintf("Subsidio estimado: recibo anual con la tarifa estimada sin subsidio menos recibo con la tarifa actual. Suma $%s millones al año en %.1f millones de hogares, unos $%s por hogar: $%s en el 10%% de menor ingreso y $%s en el de mayor ingreso. En 2025, Hacienda transfirió a la CFE $84,806 millones para cubrir parte de este subsidio.",
                                              format(round(res$brecha_total_mm, -2), big.mark = ",", trim = TRUE), res$hogares_m, format(round(res$brecha_hogar, -2), big.mark = ",", trim = TRUE),
                                              format(round(dec$brecha_hogar[dec$decil == 1], -2), big.mark = ",", trim = TRUE), format(round(dec$brecha_hogar[dec$decil == 10], -2), big.mark = ",", trim = TRUE)),
                                      sprintf("Estimated subsidy: yearly bill at the estimated unsubsidised tariff minus the bill at the current tariff. It adds up to %s million pesos a year across %.1f million homes, about %s pesos per home: %s in the lowest income tenth and %s in the highest. In 2025 the finance ministry transferred 84,806 million pesos to CFE to cover part of this subsidy.",
                                              format(round(res$brecha_total_mm, -2), big.mark = ",", trim = TRUE), res$hogares_m, format(round(res$brecha_hogar, -2), big.mark = ",", trim = TRUE),
                                              format(round(dec$brecha_hogar[dec$decil == 1], -2), big.mark = ",", trim = TRUE), format(round(dec$brecha_hogar[dec$decil == 10], -2), big.mark = ",", trim = TRUE))),
                                es_en(sprintf("Sistemas: ese monto equivale a %.1f millones de sistemas de cuatro paneles (2.4 kW) a unos $%s con IVA. Precio de instalación estimado para 2026.", res$sistemas_m, format(round(res$costo_paquete, -2), big.mark = ",", trim = TRUE)),
                                      sprintf("Systems: that sum equals %.1f million four-panel systems (2.4 kW) at about %s pesos with tax. Installed price estimated for 2026.", res$sistemas_m, format(round(res$costo_paquete, -2), big.mark = ",", trim = TRUE))),
                                es_en("Tarifa estimada sin subsidio: costo de la luz por región tarifaria, de acuerdo con la Comisión Nacional de Energía (CNE).",
                                      "Estimated unsubsidised tariff: the cost of power by tariff region, according to Mexico's National Energy Commission (CNE)."),
                                es_en("Consumo: gasto en luz de la ENIGH 2024, ajustado al consumo promedio de la CFE por tarifa.",
                                      "Consumption: electricity spending in ENIGH 2024, scaled to CFE's mean consumption by tariff.")),
                              paste(F_CFE, F_CP, F_CNE, F_ENIGH, F_PAQUETES, F_PRECIO), width = 205)) +
    theme_ls(mode) + CAP + theme(axis.text.x = element_text(colour = cc$ink))
}
save_ls(f5, "images/f5_reparto_ingreso")

# F6 ---------------------------------------------------------------------------------
d6 <- dec |> mutate(g = if_else(decil %in% c(1, 10), "punta", "medio"))
p1 <- d6$pct_ingreso[d6$decil == 1]; p10 <- d6$pct_ingreso[d6$decil == 10]
foco <- read_csv("data/derived/tarifa_costo/focalizacion.csv", show_col_types = FALSE) |> filter(decil_protegido == 4)
stopifnot(nrow(foco) == 1)
hoy25 <- t4$pct[t4$version == "tarifas actuales" & t4$grupo == "mas de 25"]
stopifnot(length(hoy25) == 1)
sobra <- format(round(foco$ahorro_mm, -2), big.mark = ",", trim = TRUE)
pesos4 <- format(round(res$costo_paquete, -2), big.mark = ",", trim = TRUE)
f6 <- function(mode) {
  cc <- pal(mode)
  ggplot(d6, aes(x = factor(decil), y = pct_ingreso, fill = g)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", pct_ingreso)), vjust = -0.5, colour = cc$ink, size = 3.8, family = STYLE_FONT) +
    scale_fill_manual(values = c(punta = cc$accent, medio = cc$primary)) +
    scale_y_continuous(limits = c(0, 3.2), breaks = seq(0, 3, 1), labels = \(x) paste0(x, "%"), expand = c(0, 0)) +
    scale_x_discrete(labels = c(es_en("1\nMenor ingreso", "1\nLowest income"), 2:9, es_en("10\nMayor ingreso", "10\nHighest income"))) +
    labs(title = es_en("El subsidio debe concentrarse en los que más lo necesitan",
                       "The subsidy should go to those who need it most"),
         subtitle = es_en("Subsidio estimado como porcentaje del ingreso del hogar",
                          "Estimated subsidy as a share of household income"),
         x = es_en("Decil de hogares por ingreso ajustado por tamaño", "Decile of homes by income adjusted for size"), y = es_en("Porcentaje del\ningreso del hogar", "Share of\nhousehold income"),
         caption = caption_ls(c(es_en(sprintf("Subsidio estimado: recibo anual con la tarifa estimada sin subsidio menos recibo con la tarifa actual, promedio por hogar, de $%s a $%s al año según el decil.",
                                              format(round(min(dec$brecha_hogar), -2), big.mark = ",", trim = TRUE), format(round(max(dec$brecha_hogar), -2), big.mark = ",", trim = TRUE)),
                                      sprintf("Estimated subsidy: yearly bill at the estimated unsubsidised tariff minus the bill at the current tariff, mean per home, %s to %s pesos a year by decile.",
                                              format(round(min(dec$brecha_hogar), -2), big.mark = ",", trim = TRUE), format(round(max(dec$brecha_hogar), -2), big.mark = ",", trim = TRUE))),
                                es_en("Ingreso: ingreso corriente del hogar en la ENIGH 2024, promedio anual de cada decil.", "Income: current household income in ENIGH 2024, yearly mean of each decile."),
                                es_en(sprintf("Si solo lo recibiera el 40%% más pobre: los hogares de los cuatro primeros deciles conservan su tarifa y el resto paga la tarifa estimada sin subsidio. Las casas donde los paneles no se pagan antes de 25 años bajarían de %.0f%% a %.0f%% y sobrarían $%s millones al año.", hoy25, foco$pct_mas_25, sobra),
                                      sprintf("If only the poorest 40%% received it: homes in the first four deciles keep their tariff and the rest pay the estimated unsubsidised tariff. Homes where panels do not pay back within 25 years would fall from %.0f%% to %.0f%%, and %s million pesos a year would be left over.", hoy25, foco$pct_mas_25, sobra)),
                                es_en(sprintf("Paneles: cuatro (2.4 kW, unos $%s con IVA, la instalación más chica que suelen vender las instaladoras) en %.0f%% de las casas, seis o más en las que consumen más de unos 350 kWh al mes. Con medición neta, créditos por 12 meses y sin intereses, batería, mantenimiento ni desgaste. Precio de instalación estimado para 2026.", pesos4, pct4),
                                      sprintf("Panels: four (2.4 kW, about %s pesos with tax, the smallest installation installers usually sell) in %.0f%% of homes, six or more in those using over about 350 kWh a month. With net metering, 12-month credits and no interest, battery, maintenance or wear. Installed price estimated for 2026.", pesos4, pct4)),
                                NOTA_COSTO, NOTA_CONS),
                              paste(F_CFE, F_CNE, F_ENIGH, F_ATLAS, F_PAQUETES, F_PRECIO), width = 205)) +
    theme_ls(mode) + CAP + theme(axis.text.x = element_text(colour = cc$ink))
}
save_ls(f6, "images/f6_peso_ingreso")
