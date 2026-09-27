# 04_fig_deciles.R ------------------------------------------------------------
# Figure F2: share of homes with a solar panel by decile of monthly labour
# income per person, Censo 2020, cities and countryside, in the series style
# (../_style/style.R), light and dark.
#
# Deciles are national, so the two lines compare the same income bands. Homes
# with no labour income sit apart on the left: they are not the poorest decile
# (the group includes pensioner households), and folding them into decile 1
# would bend the curve. Error bars are 90% intervals on the log scale, INEGI's
# convention, from the design-based SEs of 03.
#
# Run from the post root, after 03: Rscript scripts/04_fig_deciles.R

library(dplyr)
library(ggplot2)

source("../_style/style.R")

inc <- readRDS("data/derived/income_2020.rds")
d <- inc$by_grp |>
  filter(var == "panel", area %in% c("urbano", "rural"), grp != "no especificado") |>
  mutate(x = if_else(grp == "sin ingreso laboral", -0.6,
                     suppressWarnings(as.numeric(sub("D", "", grp)))),
         lo = pct * exp(-1.645 * se / pct), hi = pct * exp(1.645 * se / pct))
stopifnot(nrow(d) == 22, !anyNA(d$x))

ratio_urb <- with(d, pct[area == "urbano" & grp == "D10"] / pct[area == "urbano" & grp == "D01"])

edge <- format(round(inc$cuts, -1), big.mark = ",", trim = TRUE)
brk <- c(-0.6, 1:10)
cur <- es_en("$", "")   # English ticks: the axis title already says pesos
lab <- c(es_en("Sin ingreso\nlaboral", "No labour\nincome"),
         paste0("1\n< ", cur, edge[1]),
         paste0(2:9, "\n< ", cur, edge[2:9]),
         paste0("10\n> ", cur, edge[9]))

make <- function(mode) {
  p <- pal(mode)
  col <- c(urbano = p$primary, rural = p$accent)
  ends <- d |> filter(x == 10)
  ggplot(d, aes(x = x, y = pct, colour = area)) +
    annotate("rect", xmin = -1.1, xmax = -0.1, ymin = -Inf, ymax = Inf,
             fill = p$grid, alpha = 0.45) +
    geom_hline(yintercept = 0, colour = p$axis, linewidth = 0.4, linetype = "dotted") +
    geom_line(data = filter(d, x >= 1), linewidth = 0.8) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18, linewidth = 0.5) +
    geom_point(size = 2.3) +
    annotate("text", x = 10.35, y = ends$pct[ends$area == "urbano"],
             label = es_en("Ciudades", "Cities"), colour = p$primary, family = STYLE_FONT,
             hjust = 0, size = 3.9, fontface = "bold") +
    annotate("text", x = 10.35, y = ends$pct[ends$area == "rural"],
             label = es_en("Campo", "Rural areas"), colour = p$accent, family = STYLE_FONT,
             hjust = 0, size = 3.9, fontface = "bold") +
    annotate("text", x = 8.9, y = 2.55,
             label = sprintf(es_en("Decil 10 / decil 1 en ciudades: %.1f", "Decile 10 / decile 1 in cities: %.1f"), ratio_urb),
             colour = p$sub, family = STYLE_FONT, hjust = 1, size = 3.4) +
    scale_colour_manual(values = col) +
    scale_x_continuous(breaks = brk, labels = lab, expand = expansion(add = c(0.5, 1.4))) +
    scale_y_continuous(labels = function(v) paste0(v, "%"), limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.04))) +
    labs(x = es_en("Decil de ingreso laboral mensual del hogar ajustado por tamaño (pesos de agosto de 2026)",
                   "Monthly household labour income decile, adjusted for household size (August 2026 pesos)"),
         y = es_en("Viviendas con panel solar", "Homes with a solar panel"),
         title = es_en("El porcentaje de hogares con paneles solares incrementa bastante\nal llegar al 10% de hogares con más ingreso",
                       "The share of homes with solar panels jumps\nin the top 10% of household income"),
         subtitle = es_en("Solar panels take off only in the top tenth of household income · México, 2020", "Mexico, 2020"),
         caption = caption_ls(
           notes = es_en("Deciles nacionales, ponderados, del ingreso laboral del hogar dividido entre la raíz cuadrada del número de integrantes, en pesos de agosto de 2026 (INPC). Campo son localidades de menos de 2,500 habitantes. El ingreso laboral excluye pensiones y remesas, por eso los hogares sin ingreso laboral van aparte. Las líneas verticales son intervalos de confianza de 90%, que toman en cuenta cómo se tomó la muestra del censo. Se omite el 15% de viviendas sin ingreso especificado.",
                         "National weighted deciles of household labour income divided by the square root of household size, in August 2026 pesos (INPC). Rural areas are localities of fewer than 2,500 people. Labour income excludes pensions and remittances, so homes without labour income are shown apart. The vertical lines are 90% confidence intervals that account for how the census sample was drawn. The 15% of homes with unreported income are left out."),
           source = es_en("INEGI, Censo de Población y Vivienda 2020, cuestionario ampliado.",
                          "INEGI, 2020 Population and Housing Census, extended questionnaire."))) +
    coord_cartesian(clip = "off") +
    theme_ls(mode) +
    theme(axis.text.x = element_text(lineheight = 1.1))
}

save_ls(make, "images/f2_deciles_2020")
message("DONE!")
