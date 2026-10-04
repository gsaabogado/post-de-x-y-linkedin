# 02_fig_profiles.R -----------------------------------------------------------
# Figure F1: taxes paid and public benefits received per person at each age,
# ENIGH 2024, in the series style (../_style/style.R).
#
# Taxes stack upwards and benefits downwards; the line is their difference.
# Values are carried to pesos of August 2026 (scripts/_money.R). The loess of
# 01 dips a hair below zero where a profile starts or ends (education at age 0,
# labour taxes before 15); those are floored at zero here, since nobody pays a
# negative tax or receives negative schooling.
#
# Run from the post root, after 01: Rscript scripts/02_fig_profiles.R
# English render for LinkedIn: FIG_LANG=en Rscript scripts/02_fig_profiles.R

library(dplyr)
library(tidyr)
library(ggplot2)

source("../_style/style.R")
source("scripts/_money.R")
source("scripts/_layout.R")

TAXES <- c("excise_tax", "consumption_tax", "capital_tax", "labour_tax")
BENEFITS <- c("cash_transfers", "healthcare", "education")

prof <- readRDS("data/derived/age_profiles_2024.rds")$smooth |>
  select(age, all_of(c(TAXES, BENEFITS))) |>
  mutate(across(all_of(c(TAXES, BENEFITS)), ~ pmax(.x, 0) * PESO_FACTOR))
stopifnot(nrow(prof) == 86, !anyNA(prof))

net <- prof |>
  transmute(age,
            net = labour_tax + capital_tax + consumption_tax + excise_tax -
              education - healthcare - cash_transfers)

# Ages at which a person pays more than they receive, read from the data.
payer <- net$age[net$net > 0]
stopifnot(length(payer) > 0, all(diff(payer) == 1))
age_in <- min(payer)
age_out <- max(payer)

# Where the balance crosses zero, by linear interpolation between two ages.
cross_at <- function(a) {
  y <- net$net[match(c(a, a + 1), net$age)]
  a + y[1] / (y[1] - y[2])
}
crossings <- data.frame(age = c(cross_at(age_in - 1), cross_at(age_out)), net = 0)

bars <- prof |>
  pivot_longer(-age, names_to = "category", values_to = "value") |>
  mutate(value = if_else(category %in% BENEFITS, -value, value),
         category = factor(category, levels = c(TAXES, BENEFITS)))

# Hand-drawn keys: one block names the upper half, one the lower half.
key_tax <- data.frame(
  category = rev(TAXES), y = c(84000, 72000, 60000, 48000),
  label = c(es_en("ISR del trabajo y de negocios, cuotas de seguridad social", "Income tax on work and own business, social security"),
            es_en("ISR de rentas e intereses", "Income tax on rents and interest"),
            "IVA" |> es_en("VAT"),
            es_en("IEPS (gasolina, alcohol, tabaco, refrescos)", "Excise taxes (fuel, alcohol, tobacco, soft drinks)")))
key_ben <- data.frame(
  category = rev(BENEFITS), y = c(-72000, -84000, -96000),
  label = c(es_en("Educación pública", "Public education"),
            es_en("Salud pública", "Public health care"),
            es_en("Pensiones y programas sociales", "Pensions and social programmes")))
KEY_H <- 6500   # key height, pesos
KEY_W <- 1.5    # key width, years of age

make <- function(mode) {
  p <- pal(mode)
  fills <- c(labour_tax = p$primary, capital_tax = p$div[2], consumption_tax = p$div[3],
             excise_tax = colorRampPalette(c(p$div[3], p$bg))(3)[2],
             education = p$div[7], healthcare = p$accent, cash_transfers = p$div[5])
  key <- function(d, x0, head, y_head) list(
    annotate("text", x = x0, y = y_head, label = head, colour = p$ink,
             family = STYLE_FONT, fontface = "bold", hjust = 0, size = 3.9),
    geom_rect(data = d, aes(xmin = x0, xmax = x0 + KEY_W, ymin = y - KEY_H / 2,
                            ymax = y + KEY_H / 2, fill = category)),
    geom_text(data = d, aes(x = x0 + KEY_W + 0.9, y = y, label = label), colour = p$sub,
              family = STYLE_FONT, hjust = 0, size = 3.5))
  ggplot() +
    geom_col(data = bars, aes(x = age, y = value, fill = category), width = 0.82) +
    geom_hline(yintercept = 0, colour = p$axis, linewidth = 0.4, linetype = "dotted") +
    # the balance, with a ground-coloured halo so it survives the dark bars
    geom_line(data = net, aes(x = age, y = net), colour = p$bg, linewidth = 2.4) +
    geom_line(data = net, aes(x = age, y = net), colour = p$ink, linewidth = 0.9) +
    geom_point(data = crossings, aes(x = age, y = net), shape = 21, size = 2.6,
               stroke = 0.9, colour = p$ink, fill = p$bg) +
    key(key_tax, 1, es_en("Contribuciones que paga", "Contributions paid"), 96000) +
    key(key_ben, 27, es_en("Transferencias que recibe", "Transfers received"), -60000) +
    annotate("segment", x = 62, xend = 62 + KEY_W + 0.6, y = 84000, yend = 84000,
             colour = p$ink, linewidth = 0.9) +
    annotate("text", x = 62 + KEY_W + 1.5, y = 84000, colour = p$sub, family = STYLE_FONT,
             hjust = 0, size = 3.5, lineheight = 0.95,
             label = es_en("Balance: contribuciones\nmenos transferencias", "Balance: contributions\nminus transfers")) +
    scale_fill_manual(values = fills) +
    scale_x_continuous(breaks = c(seq(0, 80, 10), 85), labels = c(seq(0, 80, 10), "85+"),
                       expand = expansion(add = c(0.8, 0.8))) +
    scale_y_continuous(breaks = seq(-140000, 60000, 20000),
                       labels = function(v) paste0(es_en("$", ""), format(abs(v), big.mark = ",", trim = TRUE)),
                       limits = c(-150000, 104000), expand = expansion(0)) +
    labs(x = es_en("Edad", "Age"),
         y = es_en("Pesos por persona al año\n(pesos de agosto de 2026)",
                   "Pesos per person per year\n(August 2026 pesos)"),
         title = sprintf(es_en("Solo entre los %d y los %d años los mexicanos pagan\nmás contribuciones de lo que reciben del gobierno",
                               "Mexicans pay more in contributions than they receive\nfrom the government only between ages %d and %d"),
                         age_in, age_out),
         subtitle = es_en(sprintf("Mexicans pay more in contributions than they receive from the government only between ages %d and %d · México, 2024",
                                  age_in, age_out), "Mexico, 2024"),
         caption = caption_ls(
           notes = es_en(c(
             "Promedio por persona. La línea marca la diferencia entre contribuciones y transferencias.",
             "Los círculos marcan las edades donde contribuciones y transferencias se igualan.",
             "Contribuciones y transferencias son calculadas a partir de lo que reportan los hogares en la Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH).",
             "Se ajusta cada monto para que sea equivalente al total reportado oficial en 2024 con respecto al ISR de personas, cuotas al IMSS y al ISSSTE, IVA, IEPS, pensiones contributivas, nueve programas sociales, gasto público en educación y gasto en salud de cada institución.",
             "El ISR sobre salarios y las cuotas solo se asignan a los trabajadores con seguridad social. Las pensiones incluyen las privadas, que la encuesta no separa.",
             "No se incluyen los impuestos que pagan las empresas ni el gasto que no llega a una persona en particular, como seguridad o carreteras.",
             "Curvas suavizadas, la última barra agrupa a quienes tienen 85 años o más.",
             "Cada decisión y su fuente están en el código, github.com/gsaabogado/post-de-x-y-linkedin."),
                         c(
             "Average per person. The line marks the difference between contributions and transfers.",
             "The circles mark the ages where contributions and transfers are equal.",
             "Contributions and transfers are calculated from what households report in the National Survey of Household Income and Expenditure (ENIGH).",
             "Each amount is adjusted so that it equals the official total reported for 2024 for income tax paid by individuals, social security contributions to IMSS and ISSSTE, VAT, excise taxes, contributory pensions, nine social programmes, public spending on education and each institution's health spending.",
             "Income tax on wages and social security contributions are assigned only to workers with social security. Pensions include private ones, which the survey cannot separate.",
             "Taxes paid by companies and spending that reaches no particular person, such as security or roads, are left out.",
             "Smoothed curves, the last bar groups everyone aged 85 or older.",
             "Every decision and its source are in the code, github.com/gsaabogado/post-de-x-y-linkedin.")),
           source = es_en("INEGI, Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2024. Totales de 2024 de la SHCP, el SAT, la Cuenta Pública y la SEP.",
                          "INEGI, National Survey of Household Income and Expenditure (ENIGH) 2024. 2024 totals from SHCP, SAT, Cuenta Pública and SEP."),
           width = CAP_CHARS, width_list = CAP_CHARS)) +
    theme_ls(mode) +
    # numbered notes: tighter leading gives the panel its height back
    theme(plot.caption = element_text(lineheight = 1.1, size = rel(CAP_REL)))
}

save_ls(make, "images/f1_fiscal_profiles_2024", width = FIG_W, height = FIG_H)
message(sprintf("payer ages %d-%d; crossings %.1f and %.1f; peak %.0f at %d; trough %.0f at %d",
                age_in, age_out, crossings$age[1], crossings$age[2],
                max(net$net), net$age[which.max(net$net)],
                min(net$net), net$age[which.min(net$net)]))
# Headline numbers of F1, for the texts (scripts/08_dollars.R).
saveRDS(list(age_in = age_in, age_out = age_out,
             peak = max(net$net), peak_age = net$age[which.max(net$net)],
             trough = min(net$net), trough_age = net$age[which.min(net$net)]),
        "data/derived/f1_headline.rds")
message("DONE!")
