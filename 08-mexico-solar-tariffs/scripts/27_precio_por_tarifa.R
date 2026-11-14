# 27_precio_por_tarifa.R -------------------------------------------------------------
# Average price paid by the average home of each tariff, summer and the rest of the year,
# 2026 prices, against the regulator's cost-based tariff at the same consumption.
# Whole bill over kWh, with IVA, for a home at its tariff's mean consumption (CFE 2025)
# and the tariff's monthly shape. Summer is April to September, as in scripts 18 to 21;
# each place's real summer starts in its own month. DAC: its mean home, Central region,
# with tariff 1's monthly shape.
#
# Run from the post root, after 23: Rscript scripts/27_precio_por_tarifa.R
# Output: data/derived/frontera/precio_por_tarifa.csv, images/f0_precio_tarifas_light.png

library(dplyr)
library(ggplot2)
library(purrr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

source("../_style/style.R")
TARIFFS <- c("1", "1A", "1B", "1C", "1D", "1E", "1F"); IVA <- 1.16; MIN_KWH <- 25; SUMMER <- 4:9; REGION <- "Central"

det <- read_csv("data/derived/precios_hogar_detalle.csv", show_col_types = FALSE,
                col_types = cols(parte = "c", captura = "c", region = "c", otros_valores = "c")) |>
  filter(anio == 2026) |>
  mutate(dia = as.integer(coalesce(str_match(parte, "^(\\d+)")[, 2], "1"))) |>
  group_by(tarifa, region, temporada, esquema, bloque, anio, mes) |>
  slice_max(dia, n = 1, with_ties = FALSE) |> ungroup() |>
  mutate(hasta = coalesce(kwh_hasta, Inf), desde0 = kwh_desde - 1)
print(det |> group_by(tarifa) |> summarise(meses = paste(sort(unique(mes)), collapse = " ")) |> as.data.frame())
MESES <- sort(unique(det$mes[det$tarifa == "1"]))
hog <- det |> filter(tarifa != "DAC") |> split(~ tarifa + mes + temporada, drop = TRUE, sep = "|")
season_of <- function(t, m) if (t == "1") "todo el año" else if (m %in% SUMMER) "verano" else "fuera de verano"
bill_t <- function(q, t, m) {
  b <- hog[[paste(t, m, season_of(t, m), sep = "|")]]
  stopifnot(!is.null(b))
  q <- max(q, MIN_KWH)
  sum(pmax(0, pmin(q, b$hasta) - b$desde0) * b$precio) * IVA
}
dac_raw <- det |> filter(tarifa == "DAC")
bill_d <- function(q, m) {
  mm <- max(dac_raw$mes[dac_raw$mes <= m & dac_raw$bloque != "cargo_fijo"])
  d <- dac_raw[dac_raw$mes == mm, ]
  (d$precio[d$bloque == "cargo_fijo"] + q * d$precio[d$bloque == "energia" & d$region == REGION]) * IVA
}
shape <- read_csv("data/derived/payback/perfil_mensual_tarifa.csv", show_col_types = FALSE)
shape_of <- function(t) { s <- shape[shape$tarifa == t, ]; s$indice[match(c(2:12, 1), s$mes)] }
nat <- read_csv("data/derived/nacional_tarifas_anual.csv", show_col_types = FALSE) |> filter(year == 2025)
stopifnot(setequal(nat$tarifa, c(TARIFFS, "DAC")), all(nat$meses == 12))
kwh_de <- setNames(nat$kwh_mes_cliente, nat$tarifa)
tc <- read_csv("data/derived/frontera/precio_mx.csv", show_col_types = FALSE)$tc
stopifnot(length(tc) == 1)

precio <- map(c(TARIFFS, "DAC"), \(t) {
  q <- kwh_de[[t]] * shape_of(if (t == "DAC") "1" else t)
  bills <- map_dbl(MESES, \(m) if (t == "DAC") bill_d(q[m], m) else bill_t(q[m], t, m))
  tibble(tarifa = t, mes = MESES, kwh = q[MESES], bill = bills)
}) |> list_rbind() |>
  mutate(temporada = if_else(mes %in% SUMMER, "verano", "resto del año")) |>
  group_by(tarifa, temporada) |>
  summarise(n_meses = n(), kwh_mes = mean(kwh), pesos_kwh = sum(bill) / sum(kwh), .groups = "drop") |>
  mutate(centavos = 100 * pesos_kwh / tc) |>
  left_join(nat |> transmute(tarifa, clientes = clientes_dic, kwh_ano = kwh_mes_cliente), by = "tarifa")
stopifnot(nrow(precio) == 16, !anyNA(precio$centavos))

# The regulator's cost-based tariff for each tariff's average home: DB1 up to 150 kWh a month,
# DB2 above, median of CFE's 17 divisions, fixed charge and IVA included, as in script 23
cne <- read_csv("data/derived/cne_db1_db2_2026_septiembre.csv", show_col_types = FALSE) |>
  group_by(cat, unidades) |> summarise(total = median(total), .groups = "drop")
tar <- \(ca, u) cne$total[cne$cat == ca & cne$unidades == u]
stopifnot(length(c(tar("DB1", "$/kWh"), tar("DB2", "$/kWh"), tar("DB1", "$/mes"), tar("DB2", "$/mes"))) == 4)
pesos_costo <- \(k) ifelse(k <= 150, tar("DB1", "$/mes") / k + tar("DB1", "$/kWh"), tar("DB2", "$/mes") / k + tar("DB2", "$/kWh")) * IVA
precio <- precio |> mutate(pesos_costo = pesos_costo(kwh_ano), centavos_costo = 100 * pesos_costo / tc)
write_csv(precio, "data/derived/frontera/precio_por_tarifa.csv")
print(as.data.frame(precio |> select(tarifa, temporada, n_meses, kwh_mes, pesos_kwh, centavos, centavos_costo) |> mutate(across(where(is.double), ~ round(.x, 2)))))

# Yearly average residential price in the US border states, EIA-861 2024, before tax, in pesos.
# EIA: sum parts A, B, C and D for revenue but only A, B and D for sales
eia <- suppressWarnings(suppressMessages(read_excel("data/raw/eia861/2024/Sales_Ult_Cust_2024.xlsx", sheet = 1,
                                                    col_names = FALSE, .name_repair = "minimal", skip = 3)))
us <- tibble(part = eia[[4]], st = eia[[7]], rev = as.numeric(eia[[10]]), mwh = as.numeric(eia[[11]])) |>
  filter(st %in% c("CA", "AZ", "NM", "TX")) |> group_by(st) |>
  summarise(cents = 100 * sum(rev, na.rm = TRUE) / sum(mwh[part != "C"], na.rm = TRUE), .groups = "drop") |>
  mutate(pesos = cents / 100 * tc)
stopifnot(nrow(us) == 4, all(between(us$cents, 10, 45)))
write_csv(us, "data/derived/frontera/precio_estados_eua.csv")
print(as.data.frame(us |> mutate(across(where(is.double), ~ round(.x, 2)))))
ca <- us$pesos[us$st == "CA"]; otros <- range(us$pesos[us$st != "CA"]); otros_m <- mean(us$pesos[us$st != "CA"])

kwh_rango <- range(kwh_de[TARIFFS])

d <- precio |> mutate(tarifa = factor(tarifa, c(TARIFFS, "DAC")),
                      temporada = factor(temporada, c("verano", "resto del año")))
pct_cli <- setNames(100 * nat$clientes_dic / sum(nat$clientes_dic), nat$tarifa)[c(TARIFFS, "DAC")]
stopifnot(abs(sum(pct_cli) - 100) < 1e-9)
nom <- setNames(names(pct_cli), names(pct_cli)); nom["DAC"] <- es_en("DAC: alto consumo,\nsin subsidio", "DAC: high use,\nno subsidy")
etiq <- setNames(paste0(nom, "\n(", ifelse(pct_cli < 1, sprintf("%.1f%%", pct_cli), sprintf("%.0f%%", pct_cli)), ")"), names(pct_cli))
dac_p <- weighted.mean(d$pesos_kwh[d$tarifa == "DAC"], d$kwh_mes[d$tarifa == "DAC"])
marca <- d |> distinct(tarifa, pesos_costo) |> mutate(x = as.integer(tarifa))
stopifnot(nrow(marca) == 8)
veces_dac <- dac_p / marca$pesos_costo[marca$tarifa == "DAC"]
sub_max <- max(d$pesos_kwh[d$tarifa != "DAC"] / d$pesos_costo[d$tarifa != "DAC"])
cat(sprintf("DAC pays %.2f times the cost-based tariff; highest subsidised ratio %.2f\n", veces_dac, sub_max))
stopifnot(sub_max < 1)
make <- function(mode) {
  cc <- pal(mode)
  ggplot(d, aes(x = tarifa, y = pesos_kwh, fill = temporada)) +
    geom_hline(yintercept = c(ca, otros_m), colour = cc$ink, linetype = "22", linewidth = 0.5) +
    geom_col(position = position_dodge(0.76), width = 0.72) +
    geom_label(aes(label = sprintf("%.1f", pesos_kwh), colour = temporada), position = position_dodge(0.76), vjust = -0.15,
               size = 3.6, family = STYLE_FONT, fontface = "bold", fill = cc$bg, linewidth = 0, label.padding = unit(1, "pt")) +
    geom_segment(data = marca, aes(x = x - 0.38, xend = x + 0.38, y = pesos_costo, yend = pesos_costo),
                 inherit.aes = FALSE, colour = cc$ink, linewidth = 1.3) +
    geom_label(data = marca, aes(x = x, y = pesos_costo, label = sprintf("%.1f", pesos_costo)), inherit.aes = FALSE,
               vjust = -0.15, size = 3.6, family = STYLE_FONT, fontface = "bold", colour = cc$ink, fill = cc$bg, linewidth = 0,
               label.padding = unit(1, "pt")) +
    annotate("label", x = 0.55, y = ca, hjust = 0, size = 3.8, family = STYLE_FONT, colour = cc$ink, fill = cc$bg, linewidth = 0,
             label = sprintf("California: %.1f", ca)) +
    annotate("label", x = 0.55, y = otros_m, hjust = 0, size = 3.8, family = STYLE_FONT, colour = cc$ink, fill = cc$bg, linewidth = 0,
             label = sprintf(es_en("Arizona, Nuevo México y Texas: %.1f a %.1f", "Arizona, New Mexico and Texas: %.1f to %.1f"), otros[1], otros[2])) +
    annotate("text", x = 0.55, y = 9, hjust = 0, size = 3.9, family = STYLE_FONT, colour = cc$accent,
             label = es_en("Verano", "Summer")) +
    annotate("text", x = 0.55, y = 8.3, hjust = 0, size = 3.9, family = STYLE_FONT, colour = cc$primary,
             label = es_en("Resto del año", "Rest of the year")) +
    annotate("text", x = 0.55, y = 7.6, hjust = 0, size = 3.9, family = STYLE_FONT, colour = cc$ink,
             label = es_en("Barra negra: tarifa estimada sin subsidio", "Black bar: estimated unsubsidised tariff")) +
    scale_fill_manual(values = c(verano = cc$accent, `resto del año` = cc$primary)) +
    scale_colour_manual(values = c(verano = cc$accent, `resto del año` = cc$primary)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), expand = c(0, 0)) +
    scale_x_discrete(labels = etiq) +
    labs(title = es_en("El hogar promedio en México está altamente subsidiado",
                       "The average home in Mexico is heavily subsidised"),
         subtitle = es_en("Precio promedio de la luz por tarifa, pesos por kWh", "Average electricity price by tariff, pesos per kWh"),
         x = es_en("Tarifa de la CFE (porcentaje de hogares)", "CFE tariff (share of homes)"), y = es_en("Pesos por kWh", "Pesos per kWh"),
         caption = caption_ls(c(es_en(sprintf("Precio: promedio anual de cada tarifa, con IVA y precios de 2026. Consumo promedio de 2025: de %.0f a %.0f kWh al mes en las tarifas 1 a 1F. %.0f kWh al mes en DAC.", kwh_rango[1], kwh_rango[2], kwh_de[["DAC"]]),
                                      sprintf("Price: yearly average of each tariff, with tax and 2026 prices. Mean consumption in 2025: %.0f to %.0f kWh a month on tariffs 1 to 1F. %.0f kWh a month on DAC.", kwh_rango[1], kwh_rango[2], kwh_de[["DAC"]])),
                                es_en("Tarifas 1 a 1F: subsidiadas, según el clima de cada población, con más subsidio en verano en las tarifas 1A a 1F. DAC (Doméstica de Alto Consumo): sin subsidio, se le cobra a quien supera un límite máximo de consumo. Verano: abril a septiembre.",
                                      "Tariffs 1 to 1F: subsidised, according to each town's climate, with more subsidy in summer on tariffs 1A to 1F. DAC (high-consumption household tariff): no subsidy, charged to homes above a maximum consumption limit. Summer: April to September."),
                                es_en("Tarifa estimada sin subsidio: costo de la luz por región tarifaria, según la Comisión Nacional de Energía (CNE).",
                                      "Estimated unsubsidised tariff: the cost of power by tariff region, according to Mexico's National Energy Commission (CNE)."),
                                es_en("Hogares: clientes residenciales de la CFE, diciembre de 2025.", "Homes: CFE residential customers, December 2025."),
                                es_en(sprintf("Estados Unidos: precio residencial promedio de 2024, sin impuestos, a %.2f pesos por dólar.", tc),
                                      sprintf("United States: 2024 average residential price, before tax, at %.2f pesos per dollar.", tc))),
                              es_en("Comisión Federal de Electricidad (CFE), tarifas para el suministro básico 2026 y estadística de Usuarios y Consumos por Municipio de diciembre de 2025. Comisión Nacional de Energía (CNE), Tarifas Finales de Suministro Básico de septiembre de 2026. Administración de Información Energética de Estados Unidos (EIA), formulario 861, 2024. Banco de la Reserva Federal de St. Louis, serie EXMXUS, pesos por dólar.",
                                    "Federal Electricity Commission (CFE), 2026 basic supply tariffs and the statistic Usuarios y Consumos por Municipio, December 2025. National Energy Commission (CNE), Tarifas Finales de Suministro Básico, September 2026. US Energy Information Administration (EIA), form 861, 2024. Federal Reserve Bank of St. Louis, series EXMXUS, pesos per dollar."), width = 205)) +
    theme_ls(mode) + theme(plot.caption = element_text(lineheight = 1.1), axis.text.x = element_text(colour = cc$ink, size = rel(0.95)))
}
save_ls(make, "images/f0_precio_tarifas")
