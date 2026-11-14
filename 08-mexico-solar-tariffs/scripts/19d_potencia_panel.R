# 19d_potencia_panel.R --------------------------------------------------------------
# Typical panel wattage by year, to size systems in whole panels.
# 2012-2022: median wattage printed in Mexico's customs lines (ANAM, MLPV_IN), read from
# the description, brand or model of lines that name a panel or module.
# 2023-2026: Ember, capacity over number of panels shipped from China to Mexico (customs
# lines thin out after the 2022 tariff code change and fall on small panels).
# 2008-2011: held at 2012. Lines before 2012 are few and fall on small off-grid panels.
# Run from the post root: Rscript scripts/19d_potencia_panel.R
# Output: data/derived/panel_w_anual.csv

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

readRenviron(".Renviron")
IN <- path.expand(Sys.getenv("MLPV_IN"))

adu <- read_tsv(file.path(IN, "Trade/anam_customs/panels/D_8541_rows.tsv.gz"), show_col_types = FALSE,
                locale = locale(encoding = "latin1"), col_types = cols(.default = "c"),
                quote = "", progress = FALSE) |>
  transmute(anio = as.integer(str_sub(label, 1, 4)),
            txt = str_c(coalesce(descripcion, ""), coalesce(marca, ""), coalesce(modelo, ""), sep = " ")) |>
  filter(!is.na(anio), str_detect(txt, regex("panel|m.dulo|modulo|solar|fotovolt", ignore_case = TRUE))) |>
  mutate(w = as.numeric(str_match(txt, regex("(?<![\\d.])([1-7]\\d{2})(?:\\.\\d+)?\\s*-?\\s*(?:W|WP|WATTS?|VATIOS?)\\b",
                                             ignore_case = TRUE))[, 2])) |>
  filter(!is.na(w)) |>
  group_by(anio) |> summarise(w_aduana = median(w), lineas = n(), .groups = "drop")

emb <- read_csv(file.path(IN, "Trade/ember_china/ember_china_solar_exports_mx.csv"), show_col_types = FALSE) |>
  filter(`Commodity type` == "Panels") |>
  mutate(anio = as.integer(str_sub(as.character(Date), 1, 4))) |>
  group_by(anio) |>
  summarise(w_ember = sum(`Capacity (MW)`) * 1e6 / sum(`Amount (items)`), .groups = "drop")

s <- tibble(anio = 2008:2026) |>
  left_join(adu, by = "anio") |>
  left_join(emb, by = "anio") |>
  mutate(w = case_when(anio < 2012 ~ w_aduana[anio == 2012],
                       anio <= 2022 ~ w_aduana,
                       TRUE ~ w_ember),
         w = cummax(w),                       # panels do not shrink: Ember dips in 2024
         fuente = case_when(anio < 2012 ~ "se mantiene 2012",
                            anio <= 2022 ~ "aduana", TRUE ~ "ember"))
stopifnot(nrow(s) == 19, !anyNA(s$w))
write_csv(s, "data/derived/panel_w_anual.csv")
print(s |> mutate(across(c(w, w_ember), ~ round(.x))), n = 20)
