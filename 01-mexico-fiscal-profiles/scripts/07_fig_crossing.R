# 07_fig_crossing.R -----------------------------------------------------------
# Figure F4: the average deficit per person each year, 2025-2075, under the
# two fertility rates of 05_projection.R, with the year the lines cross.
#
# The point of the thread's last tweet: fewer births cost little for three
# decades (fewer children in school, the same elderly either way) and then
# the missing workers arrive.
#
# Run from the post root, after 05: Rscript scripts/07_fig_crossing.R
# English: FIG_LANG=en Rscript scripts/07_fig_crossing.R

library(dplyr)
library(tidyr)
library(ggplot2)

source("../_style/style.R")
source("scripts/_layout.R")

FIG_LANG <- Sys.getenv("FIG_LANG", "es")
stopifnot(FIG_LANG %in% c("es", "en"))
es_en <- function(es, en) if (FIG_LANG == "en") en else es

proj <- readRDS("data/derived/projection_2025_2075.rds")
d <- proj$totals |>
  filter(year >= 2025) |>
  transmute(tfr, year, deficit = -net_per_person)
stopifnot(all(d$deficit > 0), n_distinct(d$tfr) == 2)
# The two fertility rates come from 05 (survey and registry), never typed here.
LO <- proj$assumptions$scenarios[["low"]]
HI <- proj$assumptions$scenarios[["high"]]
num <- function(t) vapply(t, function(v) format(v, nsmall = 1), character(1))   # 1.2, 1.46

w <- d |> mutate(scen = if_else(tfr == LO, "lo", "hi")) |> select(-tfr) |>
  pivot_wider(names_from = scen, values_from = deficit)
worse <- w$lo > w$hi
CROSS <- min(w$year[sapply(seq_along(worse), function(i) all(worse[i:length(worse)]))])
stopifnot(CROSS > 2030, CROSS < 2075)
at <- function(t, y) d$deficit[d$tfr == t & d$year == y]
pesos <- function(v) paste0("$", format(round(v), big.mark = ",", trim = TRUE))
gap_max <- w |> mutate(g = hi - lo) |> slice_max(g, n = 1)
# The first children born under the scenario reach the payer ages (23 in F1)
# this year; before it the workers are the same people in both scenarios.
FEWER_WORKERS <- 2025 + proj$assumptions$payer_ages[1]
stopifnot(FEWER_WORKERS < CROSS)

make <- function(mode) {
  p <- pal(mode)
  cols <- setNames(c(p$accent, p$primary), c(LO, HI))
  ends <- d |> filter(year == 2075)
  ggplot(d, aes(year, deficit, colour = factor(tfr))) +
    geom_vline(xintercept = CROSS, colour = p$axis, linewidth = 0.4, linetype = "dotted") +
    geom_line(linewidth = 1.1) +
    geom_point(data = ends, size = 2.4) +
    geom_text(data = ends, aes(label = sprintf(es_en("%s hijos por mujer\n%s", "%s children per woman\n%s"),
                                               num(tfr), pesos(deficit))),
              hjust = 0, nudge_x = 0.8, family = STYLE_FONT, size = 3.6, lineheight = 0.95,
              fontface = "bold") +
    annotate("text", x = CROSS - 0.6, y = at(LO, 2075) * 0.62, hjust = 1, colour = p$sub,
             family = STYLE_FONT, size = 3.4, lineheight = 0.95,
             label = sprintf(es_en("Hasta %d, con menos hijos\nhabría menos escuela que pagar.\nDesde %d llegarían menos trabajadores\ny en %d las líneas se cruzarían",
                                   "Until %d, fewer children\nwould mean less school to pay for.\nFrom %d fewer workers would arrive\nand in %d the lines would cross"),
                             CROSS - 1, FEWER_WORKERS, CROSS)) +
    scale_colour_manual(values = cols, guide = "none") +
    scale_x_continuous(breaks = seq(2025, 2075, 10), limits = c(2025, 2086),
                       expand = expansion(add = c(1, 0))) +
    scale_y_continuous(labels = function(v) paste0("$", format(v, big.mark = ",", trim = TRUE)),
                       limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL,
         y = es_en("Déficit promedio por persona al año\n(pesos de agosto de 2026)",
                   "Average deficit per person per year\n(August 2026 pesos)"),
         title = sprintf(es_en("Con menos hijos el déficit por persona crecería más despacio\nhasta %d y después más rápido: %s en 2075 contra %s",
                               "With fewer children the deficit per person would grow more slowly\nuntil %d and faster after it: %s in 2075 against %s"),
                         CROSS - 1, pesos(at(LO, 2075)), pesos(at(HI, 2075))),
         subtitle = es_en(sprintf("With fewer children the deficit per person would grow more slowly until %d · México, 2025-2075", CROSS - 1),
                          "Mexico, 2025-2075"),
         caption = caption_ls(
           notes = c(
             es_en("La diferencia entre las contribuciones que la población paga y las transferencias que recibe, dividida entre toda la población.",
                   "The gap between the contributions the population pays and the transfers it receives, divided by the whole population."),
             es_en("No es el déficit del gobierno, porque faltan los impuestos que pagan las empresas y el gasto que no llega a una persona en particular.",
                   "It is not the government deficit, because taxes paid by companies and spending that reaches no particular person are left out."),
             sprintf(es_en("Este es un ejercicio ilustrativo en el cual cada mujer tiene siempre %s o %s hijos. La mortalidad es la misma que en 2025. No hay migración y lo que cada persona paga y recibe a cada edad se queda como en 2024.",
                           "This is an illustrative exercise in which each woman always has %s or %s children. Mortality is the same as in 2025. There is no migration, and what each person pays and receives at each age stays as in 2024."),
                     num(LO), num(HI)),
             sprintf(es_en("La mayor ventaja de tener menos hijos sería de %s por persona en %d.",
                           "The largest advantage of having fewer children would be %s per person in %d."),
                     pesos(gap_max$hi - gap_max$lo), gap_max$year),
             es_en("Cada decisión y su fuente están en el código, github.com/gsaabogado/post-de-x-y-linkedin.",
                   "Every decision and its source are in the code, github.com/gsaabogado/post-de-x-y-linkedin.")),
           source = es_en("INEGI, ENIGH 2024, Encuesta Intercensal 2025 y Estadística de Nacimientos Registrados 2025. CONAPO, Proyecciones de la población 2020-2070 (mortalidad y edades de la fecundidad). Totales de 2024 de la SHCP, el SAT, la Cuenta Pública y la SEP.",
                          "INEGI, ENIGH 2024, 2025 Intercensal Survey and Registered Births 2025. CONAPO, Population projections 2020-2070 (mortality and the age pattern of fertility). 2024 totals from SHCP, SAT, Cuenta Pública and SEP."),
           width = CAP_CHARS, width_list = CAP_CHARS)) +
    theme_ls(mode) +
    # numbered notes: tighter leading gives the panel its height back
    theme(plot.caption = element_text(lineheight = 1.1, size = rel(CAP_REL)))
}

save_ls(make, "images/f4_crossing_2075", width = FIG_W, height = FIG_H)
message(sprintf("cross %d; 2075: %s -> %s, %s -> %s; largest gap %s in %d",
                CROSS, num(LO), pesos(at(LO, 2075)), num(HI), pesos(at(HI, 2075)),
                pesos(gap_max$hi - gap_max$lo), gap_max$year))
message("DONE!")
