#### 07_fig_correlates.R
#### Fertility map post, figure 2: children per woman against four
#### observables, one dot per municipality, dot area = women 15-49.
#### Indigenous = self-identification (Luis, 2026-11-16), not language.
#### Associations between places, not effects. Line = local regression (loess)
#### weighted by women 15-49.
#### Run from the post folder: Rscript scripts/07_fig_correlates.R  (FIG_LANG=en)

library(tidyverse)
set.seed(20261116)
source("../_style/style.R")

eic <- readRDS("data/derived/eic_mun.rds")
cov <- readRDS("data/derived/covariates.rds")
NAT <- 1.23
REPL <- 2.1

d <- eic %>% select(cve_mun, tgf, f15_49) %>%
  inner_join(cov, by = "cve_mun") %>% filter(!is.na(tgf))
stopifnot(nrow(d) == 2471)

panels <- tribble(
  ~var,       ~es,                                                     ~en,
  "school_f", "Años de escuela de las mujeres de 15 años y más",        "Years of schooling, women 15 and over",
  "poverty",  "Porcentaje de la población en pobreza (2020)",           "Percent of people in poverty (2020)",
  "rural",    "Porcentaje que vive en localidades de menos de 2,500 personas (2020)", "Percent living in places under 2,500 people (2020)",
  "ind_self", "Porcentaje que se considera indígena",                  "Percent who consider themselves indigenous"
) %>% mutate(lab = if (FIG_LANG == "en") en else es,
             order = row_number())

long <- d %>% pivot_longer(all_of(panels$var), names_to = "var", values_to = "x") %>%
  filter(!is.na(x)) %>% left_join(panels %>% select(var, lab, order), by = "var")

# Weighted correlation per panel, by name
wcor <- function(x, y, w) cov.wt(cbind(x, y), wt = w, cor = TRUE)$cor[1, 2]
stats <- long %>% group_by(var, lab, order) %>%
  summarise(n = n(), r = wcor(x, tgf, f15_49), .groups = "drop")
print(stats)
long <- long %>% left_join(stats %>% select(var, r, n), by = "var") %>%
  mutate(strip = fct_reorder(sprintf("%s\n%s %.2f", str_wrap(lab, 36),
                                     es_en("Correlación:", "Correlation:"), r), order))

# Weighted smooth per panel
sm <- long %>% group_by(strip) %>% group_modify(function(g, k) {
  fit <- loess(tgf ~ x, data = g, weights = f15_49, span = 0.9)
  xs <- seq(quantile(g$x, 0.01), quantile(g$x, 0.99), length.out = 100)
  tibble(x = xs, y = predict(fit, newdata = tibble(x = xs)))
}) %>% ungroup()

make <- function(mode) {
  p <- pal(mode)
  ggplot(long, aes(x, tgf)) +
    geom_hline(yintercept = REPL, colour = p$accent, linewidth = 0.4, linetype = "dashed") +
    geom_hline(yintercept = NAT, colour = p$sub, linewidth = 0.4, linetype = "dotted") +
    geom_point(aes(size = f15_49), shape = 21, fill = p$primary, colour = p$bg,
               stroke = 0.15, alpha = 0.55) +
    geom_line(data = sm, aes(x, y), colour = p$accent, linewidth = 0.9) +
    facet_wrap(~strip, scales = "free_x", nrow = 1) +
    scale_size_area(max_size = 7) +
    scale_y_continuous(limits = c(0, 4.5), breaks = c(0, 1, NAT, 2.1, 3, 4),
                       labels = c("0", "1", "1.23", "2.1", "3", "4"), expand = c(0, 0)) +
    labs(x = NULL, y = es_en("Hijos por mujer, 2024", "Children per woman, 2024"),
         title = es_en("Donde las mujeres estudian menos y la vida es más rural,\nnacen más hijos",
                       "Where women have less schooling and life is more rural,\nmore children are born"),
         subtitle = es_en("More children are born where women have less schooling and life is more rural",
                          "Each dot is a municipality"),
         caption = caption_ls(
           notes = c(
             es_en("Cada punto es un municipio; su tamaño, el número de mujeres de 15 a 49 años. La línea roja es una regresión local, ponderada por ese número.",
                   "Each dot is a municipality; its size, the number of women aged 15 to 49. The red line is a local regression, weighted by that number."),
             sprintf(es_en("Línea discontinua: 2.1 hijos por mujer, el nivel que mantiene la población a largo plazo. Línea punteada: el país en conjunto (%.2f).",
                           "Dashed line: 2.1 children per woman, the level that keeps a population stable in the long run. Dotted line: the country as a whole (%.2f)."), NAT),
             es_en("Son asociaciones entre lugares: no muestran que una cosa cause la otra.",
                   "These are associations between places: they do not show that one thing causes the other."),
             es_en("Pobreza y tamaño de localidad son de 2020; los 12 municipios creados después no aparecen en esos paneles.",
                   "Poverty and place size are from 2020; the 12 municipalities created since do not appear in those panels.")),
           source = es_en("Instituto Nacional de Estadística y Geografía (INEGI), Encuesta Intercensal 2025, principales resultados por localidad de 50 000 y más habitantes (22 de septiembre de 2026), y Censo de Población y Vivienda 2020, principales resultados por localidad; Consejo Nacional de Evaluación de la Política de Desarrollo Social (CONEVAL), pobreza a nivel municipio 2020.",
                          "Instituto Nacional de Estadística y Geografía (INEGI), Encuesta Intercensal 2025, main results by locality of 50,000 or more inhabitants (22 September 2026), and Censo de Población y Vivienda 2020, main results by locality; Consejo Nacional de Evaluación de la Política de Desarrollo Social (CONEVAL), municipal poverty 2020."),
           width = 185)) +
    theme_ls(mode) +
    theme(strip.text = element_text(size = rel(0.72), lineheight = 1.1),
          panel.spacing.x = unit(1.2, "lines"),
          plot.caption = element_text(lineheight = 1.15))
}

save_ls(make, "images/f2_correlates")
message("DONE!")
