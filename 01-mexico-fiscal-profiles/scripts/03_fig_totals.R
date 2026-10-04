# 03_fig_totals.R -------------------------------------------------------------
# Figure F2: taxes paid and public benefits received IN TOTAL at each age,
# ENIGH 2024, in the series style (../_style/style.R).
#
# Same profiles as F1 (02_fig_profiles.R), each multiplied by the number of
# people of that age in the survey (`pop`, the sum of ENIGH expansion factors,
# smoothed by age below). Billions of pesos of August 2026.
#
# Bars run by single age to 90 (Luis, 2026-10-04: one bar for everyone aged 85
# or older held 1.25 million people and read as a break in the series; he then
# cut the figure at 90). The profiles of 01 stop at an open 85+ row, since the
# survey has few cases at each of those ages (486 at 85, 22 at 99), so every
# age from 85 on takes the 85+ amounts per person and only the head count
# varies. People older than 90 are computed (single ages to 99, then 100 and
# older) and counted in every total, but not drawn; a note says so.
#
# Run from the post root, after 01: Rscript scripts/03_fig_totals.R
# English render for LinkedIn: FIG_LANG=en Rscript scripts/03_fig_totals.R

library(dplyr)
library(tidyr)
library(ggplot2)

source("../_style/style.R")
source("scripts/_money.R")
source("scripts/_children.R")
source("scripts/_people.R")
source("scripts/_layout.R")

# FIG_LANG=en Rscript scripts/xx.R writes *_light_en.png; the default (es) writes
# the Spanish files. Not LANG: that is the shell locale.
FIG_LANG <- Sys.getenv("FIG_LANG", "es")
stopifnot(FIG_LANG %in% c("es", "en"))
es_en <- function(es, en) if (FIG_LANG == "en") en else es

TAXES <- c("excise_tax", "consumption_tax", "capital_tax", "labour_tax")
BENEFITS <- c("cash_transfers", "healthcare", "education")
BN <- 1e9
OPEN_AGE <- 85  # 01 caps age at 85: this row holds everyone aged 85 or older
TOP_AGE <- 100  # last row computed: 100 and older
SHOW_AGE <- 90  # last bar drawn

prof85 <- readRDS("data/derived/age_profiles_2024.rds")$smooth
stopifnot(nrow(prof85) == 86, all(prof85$pop > 0), max(prof85$age) == OPEN_AGE)

# Head counts by single age up to TOP_AGE, smoothed for age heaping
# (scripts/_people.R, the people the projection of 05 starts from). The
# profiles of 01 only keep the open 85+ sum.
by_sex <- people_2024(open_age = OPEN_AGE, top_age = TOP_AGE)
people <- data.frame(age = pmin(as.integer(names(by_sex$m)), TOP_AGE),
                     pop = unname(by_sex$m + by_sex$f)) |>
  count(age, wt = pop, name = "pop")
stopifnot(nrow(people) == TOP_AGE + 1, all(people$age == 0:TOP_AGE), all(people$pop > 0),
          abs(sum(people$pop) - sum(prof85$pop)) < 1)   # the people of 01

# Every age from 85 on takes the 85+ amounts per person.
prof <- people |>
  mutate(profile_age = pmin(age, OPEN_AGE)) |>
  left_join(select(prof85, -pop), by = c("profile_age" = "age")) |>
  select(-profile_age)
stopifnot(nrow(prof) == TOP_AGE + 1, !anyNA(prof))

# Ages 0-4 from registered births: the survey undercounts small children
# (scripts/_children.R, the same children the projection starts from).
kids <- children_0_4_2024()
prof$pop[match(0:4, prof$age)] <- kids$m[as.character(0:4)] + kids$f[as.character(0:4)]
stopifnot(!anyNA(prof$pop), all(prof$pop > 0))

# Per person, floored at zero as in F1, then times the people of each age.
prof <- prof |>
  select(age, pop, all_of(c(TAXES, BENEFITS))) |>
  mutate(across(all_of(c(TAXES, BENEFITS)), ~ pmax(.x, 0) * PESO_FACTOR * pop / BN))
stopifnot(!anyNA(prof))

net <- prof |>
  transmute(age,
            net = labour_tax + capital_tax + consumption_tax + excise_tax -
              education - healthcare - cash_transfers)

# Ages whose total balance is positive; the same as F1, since a positive
# head count never flips a sign.
payer <- net$age[net$net > 0]
stopifnot(length(payer) > 0, all(diff(payer) == 1))
age_in <- min(payer)
age_out <- max(payer)

# Totals by broad group, for the title and the notes.
group_net <- function(ages) sum(net$net[net$age %in% ages])
net_payers <- group_net(age_in:age_out)
net_young  <- group_net(0:(age_in - 1))
net_old    <- group_net((age_out + 1):TOP_AGE)
tax_all <- sum(prof[TAXES])
ben_all <- sum(prof[BENEFITS])
stopifnot(abs(net_payers + net_young + net_old - (tax_all - ben_all)) < 1e-6)
pop_all <- sum(prof$pop) / 1e6
pop_open <- sum(prof$pop[prof$age >= OPEN_AGE]) / 1e6
pop_hidden <- sum(prof$pop[prof$age > SHOW_AGE]) / 1e6
net_hidden <- group_net((SHOW_AGE + 1):TOP_AGE)

# Billions of pesos written as "billones" (10^12) in Spanish, trillions in English.
tril <- function(v) sprintf("%.1f", abs(v) / 1000)

bars <- prof |>
  select(-pop) |>
  pivot_longer(-age, names_to = "category", values_to = "value") |>
  mutate(value = if_else(category %in% BENEFITS, -value, value),
         category = factor(category, levels = c(TAXES, BENEFITS)))

# Hand-drawn keys: one block names the upper half, one the lower half.
key_tax <- data.frame(
  category = rev(TAXES), y = c(140, 126, 112, 98),
  label = c(es_en("ISR del trabajo y de negocios, cuotas de seguridad social", "Income tax on work and own business, social security"),
            es_en("ISR de rentas e intereses", "Income tax on rents and interest"),
            "IVA" |> es_en("VAT"),
            es_en("IEPS (gasolina, alcohol, tabaco, refrescos)", "Excise taxes (fuel, alcohol, tobacco, soft drinks)")))
key_ben <- data.frame(
  category = rev(BENEFITS), y = c(-84, -98, -112),
  label = c(es_en("Educación pública", "Public education"),
            es_en("Salud pública", "Public health care"),
            es_en("Pensiones y programas sociales", "Pensions and social programmes")))
KEY_H <- 7.5    # key height, billions of pesos
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
    geom_col(data = filter(bars, age <= SHOW_AGE), aes(x = age, y = value, fill = category), width = 0.82) +
    geom_hline(yintercept = 0, colour = p$axis, linewidth = 0.4, linetype = "dotted") +
    # the balance, with a ground-coloured halo so it survives the dark bars
    geom_line(data = filter(net, age <= SHOW_AGE), aes(x = age, y = net), colour = p$bg, linewidth = 2.4) +
    geom_line(data = filter(net, age <= SHOW_AGE), aes(x = age, y = net), colour = p$ink, linewidth = 0.9) +
    key(key_tax, 1, es_en("Contribuciones que pagan", "Contributions paid"), 154) +
    key(key_ben, 27, es_en("Transferencias que reciben", "Transfers received"), -70) +
    annotate("segment", x = 62, xend = 62 + KEY_W + 0.6, y = 140, yend = 140,
             colour = p$ink, linewidth = 0.9) +
    annotate("text", x = 62 + KEY_W + 1.5, y = 140, colour = p$sub, family = STYLE_FONT,
             hjust = 0, size = 3.5, lineheight = 0.95,
             label = es_en("Balance: contribuciones\nmenos transferencias", "Balance: contributions\nminus transfers")) +
    scale_fill_manual(values = fills) +
    scale_x_continuous(breaks = seq(0, SHOW_AGE, 10),
                       expand = expansion(add = c(0.8, 0.8))) +
    scale_y_continuous(breaks = seq(-150, 150, 50),
                       labels = function(v) paste0(es_en("$", ""), format(abs(v), big.mark = ",", trim = TRUE)),
                       limits = c(-152, 160), expand = expansion(0)) +
    labs(x = es_en("Edad", "Age"),
         y = es_en("Miles de millones de pesos al año\n(pesos de agosto de 2026)",
                   "Billions of pesos per year\n(August 2026 pesos)"),
         title = sprintf(es_en("Los mexicanos de %d a %d años aportan %s billones de pesos\nnetos al año. Los más jóvenes y los mayores reciben %s billones",
                               "Mexicans aged %d to %d put in a net %s trillion pesos a year.\nThe young and the old receive %s trillion"),
                         age_in, age_out, tril(net_payers), tril(net_young + net_old)),
         subtitle = es_en(sprintf("Mexicans aged %d to %d put in a net %s trillion pesos a year · México, 2024",
                                  age_in, age_out, tril(net_payers)), "Mexico, 2024"),
         caption = caption_ls(
           notes = c(
             sprintf(es_en("Total de cada edad, el promedio por persona multiplicado por el número de personas de esa edad. %.1f millones en total, según la encuesta, suavizado por edad. Los menores de 5 años se calculan en base a los nacimientos registrados.",
                           "Total at each age, the average per person times the number of people of that age. %.1f million in all, according to the survey, smoothed by age. Children under 5 are calculated from registered births."),
                     pop_all),
             sprintf(es_en("La figura cuenta %s billones de contribuciones y %s billones de transferencias. La diferencia no es el déficit del gobierno, porque faltan los impuestos que pagan las empresas y el gasto que no llega a una persona en particular, como seguridad o carreteras.",
                           "The figure counts %s trillion in contributions and %s trillion in transfers. The difference is not the government deficit, because taxes paid by companies and spending that reaches no particular person, such as security or roads, are left out."),
                     tril(tax_all), tril(ben_all)),
             es_en("Cada monto se ajusta para que sume el total oficial de 2024. Curvas suavizadas.",
                   "Each amount is adjusted to add up to the official 2024 total. Smoothed curves."),
             sprintf(es_en("La figura llega hasta los %d años, las personas de más edad (%.1f millones) no se muestran pero sí cuentan en los totales.",
                           "The figure stops at age %d, older people (%.1f million) are not shown but do count in the totals."),
                     SHOW_AGE, pop_hidden),
             es_en("Cada decisión y su fuente están en el código, github.com/gsaabogado/post-de-x-y-linkedin.",
                   "Every decision and its source are in the code, github.com/gsaabogado/post-de-x-y-linkedin.")),
           source = es_en("INEGI, Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2024 y Estadística de Nacimientos Registrados 2025. Totales de 2024 de la SHCP, el SAT, la Cuenta Pública y la SEP.",
                          "INEGI, National Survey of Household Income and Expenditure (ENIGH) 2024 and Registered Births 2025. 2024 totals from SHCP, SAT, Cuenta Pública and SEP."),
           width = CAP_CHARS, width_list = CAP_CHARS)) +
    theme_ls(mode) +
    # numbered notes: tighter leading gives the panel its height back
    theme(plot.caption = element_text(lineheight = 1.1, size = rel(CAP_REL)))
}

save_ls(make, "images/f2_fiscal_totals_2024", width = FIG_W, height = FIG_H)
message(sprintf("payer ages %d-%d; net payers %.1f, young %.1f, old %.1f; taxes %.1f, benefits %.1f (bn, Aug 2026); population %.2f m, 85+ %.2f m; not drawn (older than %d): %.3f m, net %.1f",
                age_in, age_out, net_payers, net_young, net_old, tax_all, ben_all, pop_all, pop_open, SHOW_AGE, pop_hidden, net_hidden))
# Headline numbers of F2 in billions of pesos, for the texts (scripts/08_dollars.R).
saveRDS(list(net_payers = net_payers, net_young = net_young, net_old = net_old,
             tax_all = tax_all, ben_all = ben_all, pop_all = pop_all),
        "data/derived/f2_headline.rds")
message("DONE!")
