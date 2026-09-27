# 08_fig_concentration.R ------------------------------------------------------
# Figure F4: concentration curves of solar panels by labour income, 2020, in
# the series style (../_style/style.R), light and dark. Cities and countryside,
# and the diagonal of equal adoption. (An AC curve was tried as a reference and
# dropped: it crossed the rural curve and F3 already carries AC. Its
# concentration index is in 07's output.) The shares printed in the figure come
# from the design-based top shares in 07, not from reading the curve.
#
# Run from the post root, after 07: Rscript scripts/08_fig_concentration.R

library(dplyr)
library(ggplot2)

source("../_style/style.R")

cc <- readRDS("data/derived/concentration_2020.rds")
top <- cc$top
get_top <- function(a, s, col = "pct") top[[col]][top$dom == a & top$stat == s]
t10u <- get_top("urbano", "top10"); t20u <- get_top("urbano", "top20")
t10r <- get_top("rural", "top10"); se10u <- get_top("urbano", "top10", "se")
message(sprintf("urban top10 %.1f%% (SE %.1f), top20 %.1f%%; rural top10 %.1f%%",
                t10u, se10u, t20u, t10r))

start <- function(d) bind_rows(tibble(cum_h = 0, cum_p = 0), d)
cu <- cc$curve |> filter(area == "urbano") |> select(cum_h, cum_p) |> start() |>
  mutate(area = "urbano")
cr <- cc$curve |> filter(area == "rural") |> select(cum_h, cum_p) |> start() |>
  mutate(area = "rural")
# Where the top 10% of urban homes begins, at the same cut as the estimate.
mk <- cc$curve |> filter(area == "urbano", mid <= 0.9) |> slice_tail(n = 1)

make <- function(mode) {
  p <- pal(mode)
  g <- seq(0, 100, 20)
  ggplot(bind_rows(cu, cr), aes(x = 100 * cum_h, y = 100 * cum_p, colour = area)) +
    # grid and x axis drawn only under the square, not under the table
    annotate("segment", x = 0, xend = 100, y = g, yend = g, colour = p$grid, linewidth = 0.35) +
    annotate("segment", x = g, xend = g, y = 0, yend = 100, colour = p$grid, linewidth = 0.35) +
    annotate("segment", x = 0, xend = 100, y = 0, yend = 0, colour = p$axis, linewidth = 0.45) +
    annotate("segment", x = 0, y = 0, xend = 100, yend = 100, colour = p$axis,
             linewidth = 0.4, linetype = "dotted") +
    annotate("text", x = 44, y = 47, label = es_en("Igualdad: la misma tasa en todo ingreso", "Equality: the same rate at every income"),
             angle = 45, colour = p$sub, family = STYLE_FONT, size = 3.1, vjust = 0) +
    geom_line(linewidth = 0.9) +
    # the top-10% bracket on the urban curve
    annotate("segment", x = 100 * mk$cum_h, xend = 100 * mk$cum_h,
             y = 100 * mk$cum_p, yend = 100, colour = p$primary, linewidth = 0.45) +
    annotate("point", x = 100 * mk$cum_h, y = 100 * mk$cum_p, colour = p$primary, size = 2.3) +
    annotate("text", x = 100 * mk$cum_h - 1.5, y = 94, hjust = 1,
             label = sprintf("%.0f%%", t10u),
             colour = p$primary, family = STYLE_FONT, size = 3.6, fontface = "bold") +
    annotate("text", x = 57, y = 27, label = es_en("Ciudades", "Cities"), colour = p$primary,
             family = STYLE_FONT, size = 3.9, fontface = "bold", hjust = 0) +
    annotate("text", x = 31, y = 21, label = es_en("Campo", "Rural"), colour = p$accent,
             family = STYLE_FONT, size = 3.9, fontface = "bold", hjust = 0.5) +
    # the three numbers, as a small table to the right of the square
    annotate("text", x = 114, y = 96, hjust = 0, vjust = 1, colour = p$ink,
             family = STYLE_FONT, size = 3.6, fontface = "bold",
             label = es_en("Parte de los hogares con panel", "Share of homes with a panel")) +
    annotate("text", x = 114, y = 88, hjust = 0, vjust = 1, colour = p$sub,
             family = STYLE_FONT, size = 3.4, lineheight = 1.6,
             label = paste(es_en(c("Ciudades, 10% de más ingreso", "Ciudades, 20% de más ingreso",
                                 "Campo, 10% de más ingreso"),
                               c("Cities, top 10% by income", "Cities, top 20% by income",
                                 "Rural areas, top 10% by income")), collapse = "\n")) +
    annotate("text", x = 180, y = 88, hjust = 1, vjust = 1, colour = p$ink,
             family = STYLE_FONT, size = 3.4, lineheight = 1.6,
             label = paste(sprintf("%.0f%%", c(t10u, t20u, t10r)), collapse = "\n")) +
    scale_colour_manual(values = c(urbano = p$primary, rural = p$accent)) +
    scale_x_continuous(breaks = seq(0, 100, 20), labels = function(v) paste0(v, "%"),
                       expand = expansion(add = c(0, 0))) +
    scale_y_continuous(breaks = seq(0, 100, 20), labels = function(v) paste0(v, "%"),
                       expand = expansion(add = c(0, 2))) +
    coord_fixed(xlim = c(0, 182), ylim = c(0, 100), clip = "off") +
    labs(x = es_en("% acumulado de hogares, del menor al mayor ingreso laboral ajustado por tamaño",
                   "Cumulative % of homes, from lowest to highest size-adjusted labour income"),
         y = es_en("% acumulado de hogares con panel", "Cumulative % of homes with a panel"),
         title = es_en("Más de un tercio de los paneles urbanos está en el 10% de hogares con más ingreso", "More than a third of urban solar panels are in the top 10% of homes by income"),
         subtitle = es_en("More than a third of urban solar panels sit in the top tenth of homes by income · México, 2020", "Mexico, 2020"),
         caption = caption_ls(
           notes = sprintf(es_en("Curvas de concentración. Los hogares se ordenan por ingreso laboral mensual dividido entre la raíz cuadrada del número de integrantes, dentro de ciudades (2,500 habitantes o más) y del campo, entre hogares con ingreso laboral positivo. Si los paneles se repartieran igual en todos los ingresos, la curva seguiría la diagonal. El %.0f%% tiene un error estándar de %.1f puntos.",
                                 "Concentration curves. Homes are ranked by monthly labour income divided by the square root of household size, within cities (2,500 people or more) and within rural areas, among homes with positive labour income. If panels were spread equally across incomes, the curve would follow the diagonal. The %.0f%% has a standard error of %.1f points."), t10u, se10u),
           source = es_en("INEGI, Censo de Población y Vivienda 2020, cuestionario ampliado.",
                          "INEGI, 2020 Population and Housing Census, extended questionnaire."))) +
    theme_ls(mode) +
    theme(axis.title.x = element_text(hjust = 0),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank())
}

save_ls(make, "images/f4_concentration_2020", height = 7.2)
message("DONE!")
