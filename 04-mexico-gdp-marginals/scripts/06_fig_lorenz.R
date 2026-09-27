# 06_fig_lorenz.R -------------------------------------------------------------
# Figure F2: how evenly output per person is spread across PLACES. Lorenz curve
# over the 0.25-degree cells, in the series style (../_style/style.R), light.
#
# Cells are sorted from the lowest to the highest GDP per person; x is the
# cumulative share of people, y the cumulative share of GDP. This is inequality
# BETWEEN places of about 28 km, not between people or households, so it is not
# comparable to an income Gini and the figure says so.
#
# The source pins each STATE to INEGI's GDP and predicts cells within a state
# with a random forest whose main input is population, trained on targets that
# assume flat GDP per head inside each unit (Rossi-Hansberg & Zhang 2025, NBER
# WP 33458, secs. 1.2-1.3, p.5 and p.10). Within-state spread is therefore
# compressed and the Gini is a floor; the note says so.
#
# Cells with GDP but no population (0 of them expected; counted below) would go
# last, where x no longer advances, so no GDP is dropped.
#
# Run from the post root: Rscript scripts/06_fig_lorenz.R

library(dplyr)
library(ggplot2)

source("../_style/style.R")
source("scripts/_money.R")

g <- readRDS("data/derived/grid_map.rds")
meta <- readRDS("data/derived/meta.rds")
stopifnot(abs(sum(g$gdp) / meta$total_gdp - 1) < 1e-6, !anyNA(g$gdp), !anyNA(g$pop))
message(sprintf("cells %d, with GDP and no people %d", nrow(g), sum(g$pop == 0)))

K <- mxn_factor(meta$total_gdp)
lz <- g |>
  mutate(pc = ifelse(pop > 0, gdp / pop, Inf)) |>
  arrange(pc) |>
  mutate(x = cumsum(pop) / sum(pop), y = cumsum(gdp) / sum(gdp))
lz <- bind_rows(tibble(x = 0, y = 0), select(lz, x, y, pc))
stopifnot(abs(tail(lz$y, 1) - 1) < 1e-9, abs(tail(lz$x, 1) - 1) < 1e-9)

gini <- 1 - sum(diff(lz$x) * (head(lz$y, -1) + tail(lz$y, -1)))
at <- function(p) approx(lz$x, lz$y, p, ties = "ordered")$y
b50 <- at(0.5)
t10 <- 1 - at(0.9)
# GDP per person at the 10th, 50th and 90th percentile of PEOPLE, pesos of Aug 2026
pct_pc <- sapply(c(0.1, 0.5, 0.9), function(q) lz$pc[which(lz$x >= q)[1]]) * K
message(sprintf("Gini %.3f; bottom 50%% of people -> %.1f%% of GDP; top 10%% -> %.1f%%",
                gini, 100 * b50, 100 * t10))
message(sprintf("GDP per person p10/p50/p90, pesos Aug 2026: %s; p90/p10 %.1f",
                paste(format(round(pct_pc, -3), big.mark = ","), collapse = " / "),
                pct_pc[3] / pct_pc[1]))

peso <- function(v) paste0(es_en("$", "MX$"), format(round(v, -3), big.mark = ",", trim = TRUE))

make <- function(mode) {
  p <- pal(mode)
  gr <- seq(0, 100, 20)
  ggplot(lz, aes(x = 100 * x, y = 100 * y)) +
    annotate("segment", x = 0, xend = 100, y = gr, yend = gr, colour = p$grid, linewidth = 0.35) +
    annotate("segment", x = gr, xend = gr, y = 0, yend = 100, colour = p$grid, linewidth = 0.35) +
    annotate("segment", x = 0, xend = 100, y = 0, yend = 0, colour = p$axis, linewidth = 0.45) +
    annotate("segment", x = 0, y = 0, xend = 100, yend = 100, colour = p$axis,
             linewidth = 0.4, linetype = "dotted") +
    annotate("text", x = 40, y = 43, label = es_en("Igualdad: el mismo PIB por persona en todo lugar", "Equality: the same GDP per person everywhere"),
             angle = 45, colour = p$sub, family = STYLE_FONT, size = 3.1, vjust = 0) +
    geom_line(colour = p$primary, linewidth = 0.9) +
    # bottom half of people
    annotate("segment", x = 50, xend = 50, y = 0, yend = 100 * b50, colour = p$accent, linewidth = 0.45) +
    annotate("point", x = 50, y = 100 * b50, colour = p$accent, size = 2.3) +
    annotate("text", x = 51.5, y = 100 * b50 - 3, hjust = 0, vjust = 1,
             label = sprintf(es_en("%.0f%% del PIB", "%.0f%% of GDP"), 100 * b50),
             colour = p$accent, family = STYLE_FONT, size = 3.6, fontface = "bold") +
    # top tenth of people
    annotate("segment", x = 90, xend = 90, y = 100 * (1 - t10), yend = 100, colour = p$primary, linewidth = 0.45) +
    annotate("point", x = 90, y = 100 * (1 - t10), colour = p$primary, size = 2.3) +
    annotate("text", x = 88.5, y = 94, hjust = 1,
             label = sprintf("%.0f%%", 100 * t10),
             colour = p$primary, family = STYLE_FONT, size = 3.6, fontface = "bold") +
    # the numbers, as a small table to the right of the square
    annotate("text", x = 114, y = 96, hjust = 0, vjust = 1, colour = p$ink,
             family = STYLE_FONT, size = 3.6, fontface = "bold", label = es_en("Parte del PIB", "Share of GDP")) +
    annotate("text", x = 114, y = 88, hjust = 0, vjust = 1, colour = p$sub,
             family = STYLE_FONT, size = 3.4, lineheight = 1.6,
             label = es_en(paste("50% de la población en los lugares\ncon menos PIB por persona",
                                 "10% en los lugares con más", sep = "\n"),
                           paste("50% of people, in the places\nwith the least GDP per person",
                                 "10% in the places with the most", sep = "\n"))) +
    annotate("text", x = 186, y = 88, hjust = 1, vjust = 1, colour = p$ink,
             family = STYLE_FONT, size = 3.4, lineheight = 1.6,
             label = paste(sprintf("%.0f%%", 100 * b50), "", sprintf("%.0f%%", 100 * t10), sep = "\n")) +
    annotate("text", x = 114, y = 62, hjust = 0, vjust = 1, colour = p$ink,
             family = STYLE_FONT, size = 3.6, fontface = "bold",
             label = es_en("PIB por persona al año, según el lugar", "GDP per person per year, by place")) +
    annotate("text", x = 114, y = 54, hjust = 0, vjust = 1, colour = p$sub,
             family = STYLE_FONT, size = 3.4, lineheight = 1.6,
             label = es_en(paste("10% de la población, menos de",
                                 "La mitad de la población, menos de",
                                 "10% de la población, más de", sep = "\n"),
                           paste("10% of people, less than",
                                 "Half of people, less than",
                                 "10% of people, more than", sep = "\n"))) +
    annotate("text", x = 186, y = 54, hjust = 1, vjust = 1, colour = p$ink,
             family = STYLE_FONT, size = 3.4, lineheight = 1.6,
             label = paste(peso(pct_pc), collapse = "\n")) +
    scale_x_continuous(breaks = gr, labels = function(v) paste0(v, "%"), expand = c(0, 0)) +
    scale_y_continuous(breaks = gr, labels = function(v) paste0(v, "%"),
                       expand = expansion(add = c(0, 2))) +
    coord_fixed(xlim = c(0, 188), ylim = c(0, 100), clip = "off") +
    labs(x = es_en("% acumulado de la población, del lugar con menos al lugar con más PIB por persona", "Cumulative % of people, from the place with the least to the most GDP per person"),
         y = es_en("% acumulado del PIB", "Cumulative % of GDP"),
         title = sprintf(es_en("La mitad de los mexicanos vive en lugares que producen el %.0f%% del PIB", "Half of Mexicans live in places that produce %.0f%% of GDP"),
                         100 * b50),
         subtitle = es_en(sprintf("Half of Mexicans live in places that produce %.0f%% of GDP · México, 2021",
                            100 * b50), "Mexico, 2021"),
         caption = caption_ls(
           notes = sprintf(es_en("Curva de Lorenz entre lugares. México se divide en celdas de 0.25 grados (unos 28 km por lado) y las celdas se ordenan por PIB por persona. Si cada lugar produjera lo mismo por persona, la curva seguiría la diagonal. Mide la desigualdad entre lugares, no entre personas, así que no es comparable con la desigualdad del ingreso de los hogares. Índice de Gini entre lugares, %.2f. El PIB de cada estado viene de INEGI, pero dentro de cada estado lo reparte un modelo que se apoya sobre todo en la población, así que las diferencias reales entre lugares probablemente son mayores. PIB de 2021 en pesos de agosto de 2026 (INPC).",
                           "Lorenz curve between places. Mexico is split into cells of 0.25 degrees (about 28 km a side) and the cells are ranked by GDP per person. If every place produced the same per person, the curve would follow the diagonal. It measures inequality between places, not between people, so it is not comparable with household income inequality. Gini index between places, %.2f. Each state's GDP comes from INEGI, but within each state a model spreads it mostly by population, so the real differences between places are probably larger. GDP of 2021 in pesos of August 2026 (INPC)."), gini),
           source = es_en("Rossi-Hansberg y Zhang (2025), NBER WP 33458, versión 2, 2021. INEGI, PIB e INPC.", "Rossi-Hansberg and Zhang (2025), NBER WP 33458, version 2, 2021. INEGI, GDP and INPC."), width = 140)) +
    theme_ls(mode) +
    theme(axis.title.x = element_text(hjust = 0),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank())
}

save_ls(make, "images/f2_lorenz_2021", height = 7.2)
message("DONE!")
