# 06_fig_ac.R -----------------------------------------------------------------
# Figure F3: in cities, the share of homes with a solar panel by income decile,
# for homes with and without air conditioning, in the series style
# (../_style/style.R), light and dark. Same x axis as F2, so the two read as a
# pair.
#
# Run from the post root, after 05: Rscript scripts/06_fig_ac.R

library(dplyr)
library(ggplot2)

source("../_style/style.R")

ac <- readRDS("data/derived/ac_2020.rds")
inc <- readRDS("data/derived/income_2020.rds")

d <- ac$by_dec |>
  filter(area == "urbano", grp != "no especificado") |>
  mutate(x = if_else(grp == "sin ingreso laboral", -0.6,
                     suppressWarnings(as.numeric(sub("D", "", grp)))),
         lo = pct * exp(-1.645 * se / pct), hi = pct * exp(1.645 * se / pct))
stopifnot(nrow(d) == 22, !anyNA(d$x))

rr <- range(ac$urb_ratio$ratio)
sh <- ac$share
message(sprintf("AC homes %.0f%% of homes, %.0f%% of panel homes; urban ratio %.1f-%.1f",
                100 * sh$homes_ac, 100 * sh$panels_ac, rr[1], rr[2]))

edge <- format(round(inc$cuts, -1), big.mark = ",", trim = TRUE)
brk <- c(-0.6, 1:10)
cur <- es_en("$", "")   # English ticks: the axis title already says pesos
lab <- c(es_en("Sin ingreso\nlaboral", "No labour\nincome"),
         paste0("1\n< ", cur, edge[1]),
         paste0(2:9, "\n< ", cur, edge[2:9]),
         paste0("10\n> ", cur, edge[9]))

make <- function(mode) {
  p <- pal(mode)
  col <- c(con = p$accent, sin = p$primary)
  ends <- d |> filter(x == 10)
  ggplot(d, aes(x = x, y = pct, colour = ac)) +
    annotate("rect", xmin = -1.1, xmax = -0.1, ymin = -Inf, ymax = Inf,
             fill = p$grid, alpha = 0.45) +
    geom_hline(yintercept = 0, colour = p$axis, linewidth = 0.4, linetype = "dotted") +
    geom_line(data = filter(d, x >= 1), linewidth = 0.8) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18, linewidth = 0.5) +
    geom_point(size = 2.3) +
    annotate("text", x = 10.35, y = ends$pct[ends$ac == "con"],
             label = es_en("Con aire\nacondicionado", "With air\nconditioning"), colour = p$accent, family = STYLE_FONT,
             hjust = 0, size = 3.7, fontface = "bold", lineheight = 0.95) +
    annotate("text", x = 10.35, y = ends$pct[ends$ac == "sin"],
             label = es_en("Sin aire\nacondicionado", "Without air\nconditioning"), colour = p$primary, family = STYLE_FONT,
             hjust = 0, size = 3.7, fontface = "bold", lineheight = 0.95) +
    annotate("text", x = 1, y = 4.9, hjust = 0, vjust = 1, colour = p$sub,
             family = STYLE_FONT, size = 3.4, lineheight = 1.2,
             label = sprintf(es_en(paste0("En cada decil, con aire acondicionado: %.1f a %.1f veces más paneles.\n",
                                          "El %.0f%% de las viviendas que tienen aire acondicionado corresponden al %.0f%% de las que tienen panel."),
                                   paste0("In every decile, homes with air conditioning have %.1f to %.1f times as many panels.\n",
                                          "Homes with air conditioning are %.0f%% of all homes but %.0f%% of homes with a panel.")),
                             rr[1], rr[2], 100 * sh$homes_ac, 100 * sh$panels_ac)) +
    scale_colour_manual(values = col) +
    scale_x_continuous(breaks = brk, labels = lab, expand = expansion(add = c(0.5, 1.9))) +
    scale_y_continuous(labels = function(v) paste0(v, "%"), limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.04))) +
    labs(x = es_en("Decil de ingreso laboral mensual del hogar ajustado por tamaño (pesos de agosto de 2026)",
                   "Monthly household labour income decile, adjusted for household size (August 2026 pesos)"),
         y = es_en("Viviendas urbanas con panel solar", "Urban homes with a solar panel"),
         title = es_en("Con el mismo ingreso, quien tiene aire acondicionado tiene más paneles", "At the same income, homes with air conditioning have more solar panels"),
         subtitle = es_en("Comparing households at the same income level, homes with air conditioning are far likelier to have solar panels · México, 2020", "Urban homes, Mexico, 2020"),
         caption = caption_ls(
           notes = es_en("Localidades de 2,500 habitantes o más. Deciles nacionales del ingreso laboral del hogar dividido entre la raíz cuadrada del número de integrantes, en pesos de agosto de 2026. Las líneas verticales son intervalos de confianza de 90%, que toman en cuenta cómo se tomó la muestra del censo. El aire acondicionado señala consumo eléctrico alto y también clima caluroso, con más sol y tarifas más subsidiadas, así que la figura muestra una asociación y no un efecto. La proporción de viviendas con aire acondicionado incluye el campo.",
                         "Localities of 2,500 people or more. National deciles of household labour income divided by the square root of household size, in August 2026 pesos. The vertical lines are 90% confidence intervals that account for how the census sample was drawn. Air conditioning signals heavy electricity use and also a hot climate, with more sun and more heavily subsidised tariffs, so the figure shows an association, not an effect. The share of homes with air conditioning includes rural areas."),
           source = es_en("INEGI, Censo de Población y Vivienda 2020, cuestionario ampliado.",
                          "INEGI, 2020 Population and Housing Census, extended questionnaire."))) +
    coord_cartesian(clip = "off") +
    theme_ls(mode) +
    theme(axis.text.x = element_text(lineheight = 1.1))
}

save_ls(make, "images/f3_ac_2020")
message("DONE!")
