# 28_fig_adopcion.R -----------------------------------------------------------------
# Figure 1 of the thread: urban homes with solar panels by national income tenth (Censo
# 2020, post 06), Mexico's national rate, and reference lines for other countries.
# Income is household labour income adjusted for size. Urban homes only, because the
# highest rural rates are probably off-grid systems. The country values come only from
# government sources, listed in data/raw/paises/hogares_con_paneles.csv with measure, year
# and url; the US value is recomputed here from EIA-861. They do not share one definition.
# Run from the post root: Rscript scripts/28_fig_adopcion.R
# FIG_LANG=en writes the English version.

library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)

source("../_style/style.R")
readRenviron(".Renviron")
num <- \(x) suppressWarnings(as.numeric(x))

EIC <- file.path(path.expand(Sys.getenv("MLPV_IN")), "Census/intercensal2025/raw/conjunto_de_datos_eic2025_105_csv.zip")
eic <- read_csv(unz(EIC, "conjunto_de_datos/conjunto_datos_eic2025_105.csv"),
                col_types = cols(.default = "c"), locale = locale(encoding = "latin1")) |>
  filter(ESTIMADOR == "Valor", CVEGEO == "000000000")
nac25 <- num(eic$PCN_VPH_PANEL)
ing <- readRDS("../06-mexico-solar-income/data/derived/income_2020.rds")
nac20 <- ing$nat
print(nac20)
nac20 <- if (is.list(nac20) && "pct" %in% names(nac20)) nac20$pct[1] else as.numeric(unlist(nac20))[1]
stopifnot(length(nac25) == 1, !is.na(nac25), length(nac20) == 1, abs(nac20 - 0.806) < 0.01)

inc <- ing$by_grp |>
  filter(var == "panel", area == "urbano", str_detect(grp, "^D\\d\\d$")) |>
  transmute(n = as.integer(sub("D", "", grp)), pct,
            et = case_when(n == 1 ~ es_en("1, menor ingreso", "1, lowest income"),
                           n == 10 ~ es_en("10, mayor ingreso", "10, highest income"),
                           TRUE ~ as.character(n)),
            g = if_else(n == 10, "accent", "primary")) |> arrange(desc(n)) |>
  mutate(et = factor(et, levels = et))
stopifnot(nrow(inc) == 10, !anyNA(inc$pct))
top <- inc$pct[inc$n == 10]

pa <- read_csv("data/raw/paises/hogares_con_paneles.csv", show_col_types = FALSE)
stopifnot(nrow(pa) == 4, !anyNA(pa$pct))
hoja <- \(f, sh) suppressWarnings(suppressMessages(read_excel(f, sheet = sh, col_names = FALSE, .name_repair = "minimal", skip = 3)))
nm <- hoja("data/raw/eia861/2024/Net_Metering_2024.xlsx", "States")
nm <- nm[!is.na(nm[[2]]) & nchar(nm[[2]]) == 2, ]
sa <- hoja("data/raw/eia861/2024/Sales_Ult_Cust_2024.xlsx", 1)
sa <- sa[!is.na(sa[[7]]) & nchar(sa[[7]]) == 2 & sa[[4]] != "C" & sa[[8]] != "Behind the Meter", ]
us <- 100 * sum(num(nm[[12]]), na.rm = TRUE) / sum(num(sa[[12]]), na.rm = TRUE)
cat(sprintf("US: %.2f%%\n", us))
stopifnot(abs(us - pa$pct[pa$pais == "Estados Unidos"]) < 0.1)
val <- \(p) pa$pct[pa$pais == p]; ano <- \(p) pa$anio[pa$pais == p]
lin <- tibble(p = c("Suiza", "Brasil", "Alemania", "Estados Unidos"), y = c(10, 9, 8, 7),
              n = c(es_en("Suiza", "Switzerland"), es_en("Brasil", "Brazil"), es_en("Alemania", "Germany"), es_en("Estados Unidos", "United States"))) |>
  rowwise() |> mutate(x = val(p), et = sprintf("%s: %.1f%%, %d", n, x, ano(p))) |> ungroup()

make <- function(mode) {
  cc <- pal(mode)
  ggplot(inc, aes(y = et, x = pct, fill = g)) +
    geom_vline(xintercept = lin$x, colour = cc$support, linetype = "22", linewidth = 0.5) +
    geom_vline(xintercept = c(nac20, nac25), colour = cc$accent, linetype = "dotted", linewidth = 0.6) +
    geom_col(width = 0.68) +
    geom_label(aes(label = sprintf("%.1f%%", pct + 1e-9)), hjust = -0.1, colour = cc$ink, fill = cc$bg, size = 3.6, family = STYLE_FONT,
               linewidth = 0, label.padding = unit(1, "pt")) +
    geom_label(data = lin, aes(x = x + 0.05, y = y, label = et), inherit.aes = FALSE, hjust = 0, colour = cc$ink, fill = cc$bg,
               size = 3.7, family = STYLE_FONT, linewidth = 0, label.padding = unit(1.5, "pt")) +
    annotate("label", x = 1.75, y = 3, hjust = 0, colour = cc$accent, fill = cc$bg, size = 3.7, family = STYLE_FONT, linewidth = 0,
             label = sprintf(es_en("México, todas las casas: %.1f%% en 2020 y %.1f%% en 2025", "Mexico, all homes: %.1f%% in 2020 and %.1f%% in 2025"), nac20, nac25)) +
    scale_fill_manual(values = c(primary = cc$primary, accent = cc$accent)) +
    scale_x_continuous(limits = c(0, 8.4), breaks = 0:8, labels = \(x) paste0(x, "%"), expand = c(0, 0)) +
    labs(title = es_en(sprintf("En México, solo %.1f%% de los hogares tienen paneles solares", nac25),
                       sprintf("In Mexico, only %.1f%% of homes have solar panels", nac25)),
         subtitle = es_en("Porcentaje de hogares urbanos con paneles en México por decil de ingreso, 2020",
                          "Share of urban homes with panels in Mexico by income decile, 2020"),
         x = es_en("Porcentaje de hogares con paneles", "Share of homes with panels"), y = es_en("Decil de ingreso de los hogares urbanos", "Income decile of urban homes"),
         caption = caption_ls(c(es_en("Ingreso: ingreso laboral de los hogares urbanos ajustado por tamaño. No incluye otros ingresos. Los datos provienen del Censo 2020 porque los datos de 2025 no traen ingreso.",
                                      "Income: labour income of urban households adjusted for size. Other income is not counted. The data come from the 2020 census because the 2025 data carry no income."),
                                es_en("Estados Unidos: clientes residenciales con paneles y medición neta. Alemania: hogares que venden electricidad solar. Brasil: familias que reciben créditos de generación distribuida, entre viviendas ocupadas. Suiza: sistemas en casas.",
                                      "United States: residential customers with panels and net metering. Germany: households selling solar electricity. Brazil: families receiving distributed-generation credits, over occupied dwellings. Switzerland: systems on homes.")),
                              es_en("México: Instituto Nacional de Estadística y Geografía (INEGI), Censo de Población y Vivienda 2020, cuestionario ampliado, y Encuesta Intercensal 2025. Estados Unidos: Administración de Información Energética (EIA), formulario 861, 2024. Alemania: Oficina Federal de Estadística (Destatis), Encuesta de Ingresos y Gastos 2023, comunicado N017 de 2026. Brasil: Agencia Nacional de Energía Eléctrica (ANEEL), julio de 2025, e Instituto Brasileño de Geografía y Estadística (IBGE), Censo 2022. Suiza: Oficina Federal de Energía, Statistik Sonnenenergie 2024, y Oficina Federal de Estadística, hogares a fines de 2025.",
                                    "Mexico: National Institute of Statistics and Geography (INEGI), Censo de Población y Vivienda 2020, extended questionnaire, and Encuesta Intercensal 2025. United States: Energy Information Administration (EIA), form 861, 2024. Germany: Federal Statistical Office (Destatis), Income and Consumption Survey 2023, press release N017 of 2026. Brazil: National Electric Energy Agency (ANEEL), July 2025, and Brazilian Institute of Geography and Statistics (IBGE), Censo 2022. Switzerland: Federal Office of Energy, Statistik Sonnenenergie 2024, and Federal Statistical Office, households at end 2025."), width = 205)) +
    theme_ls(mode) +
    theme(plot.caption = element_text(lineheight = 1.1), panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour = cc$ink, size = rel(0.9)))
}
save_ls(make, "images/f1_adopcion")
