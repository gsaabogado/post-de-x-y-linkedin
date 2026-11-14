# 05_parse_cfe_2026.R -----------------------------------------------------------
# Turns the 2026 readings of CFE's tariff pages (data/raw/cfe/cfe_tarifas_2026.txt,
# produced in a browser by 04_cfe_extract.js) into two tables in data/derived/:
#   cfe_bloques_2026.csv  tariffs 1 to 1F: one row per tariff, season, month, block
#   cfe_dac_2026.csv      DAC: one row per month and region
# and prints the monthly bill of a tariff 1 home in the Central region at a few
# consumption levels, to show the step into DAC. Bills exclude the 16% IVA.
#
# Run from the post root: Rscript scripts/05_parse_cfe_2026.R

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

MONTHS <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")

raw <- read_lines("data/raw/cfe/cfe_tarifas_2026.txt")
head_i <- which(str_detect(raw, "^# Tarifa"))
stopifnot(length(head_i) == 14)
blocks_raw <- lapply(seq_along(head_i), function(i) {
  end <- if (i < length(head_i)) head_i[i + 1] - 1 else length(raw)
  h <- str_split(raw[head_i[i]], " ")[[1]]
  tibble(page = h[2], start = h[3], line = raw[(head_i[i] + 1):end])
}) |> bind_rows()

# tariffs 1 to 1F ---------------------------------------------------------------
tar <- blocks_raw |>
  filter(page != "TarifaDAC") |>
  mutate(tarifa = str_remove(page, "Tarifa"),
         mes = match(str_sub(line, 1, 3), MONTHS),
         cells = str_split(str_sub(line, 5), " ")) |>
  unnest_longer(cells, indices_to = "bloque_n") |>
  separate(cells, c("precio_kwh", "kwh_bloque"), sep = "/") |>
  mutate(precio_kwh = as.numeric(precio_kwh),
         kwh_bloque = if_else(kwh_bloque == "r", NA_real_, suppressWarnings(as.numeric(kwh_bloque)))) |>
  group_by(tarifa, start, mes) |>
  mutate(n_bloques = n(),
         desde_kwh = lag(cumsum(replace_na(kwh_bloque, 0)), default = 0)) |>
  ungroup()
stopifnot(!anyNA(tar$mes), !anyNA(tar$precio_kwh))

# a month is "verano" for a summer start when its basic block is the summer one
# (larger than 75 kWh); tariff 1 has no seasons
tar <- tar |>
  group_by(tarifa, start, mes) |>
  mutate(temporada = case_when(tarifa == "1" ~ "todo el año",
                               first(kwh_bloque) > 75 ~ "verano",
                               TRUE ~ "fuera de verano")) |>
  ungroup() |>
  distinct(tarifa, temporada, mes, bloque_n, precio_kwh, kwh_bloque, desde_kwh)
# the same month and season must read the same whichever summer start was asked
stopifnot(nrow(tar) == nrow(distinct(tar, tarifa, temporada, mes, bloque_n)))
write_csv(tar, "data/derived/cfe_bloques_2026.csv")

# DAC ---------------------------------------------------------------------------
dac <- blocks_raw |>
  filter(page == "TarifaDAC") |>
  mutate(mes = match(str_sub(line, 1, 3), MONTHS), cells = str_split(str_sub(line, 5), " ")) |>
  unnest_longer(cells) |>
  separate(cells, c("region", "cargo_fijo_mes", "p1", "p2"), sep = ":", fill = "right") |>
  mutate(across(c(cargo_fijo_mes, p1, p2), as.numeric),
         precio_kwh_verano = if_else(is.na(p2), NA_real_, p1),
         precio_kwh = if_else(is.na(p2), p1, p2)) |>   # BC and BCS: p2 is the price outside summer
  select(mes, region, cargo_fijo_mes, precio_kwh, precio_kwh_verano)
stopifnot(nrow(dac) == 60, !anyNA(dac$precio_kwh))
write_csv(dac, "data/derived/cfe_dac_2026.csv")

# the step into DAC: tariff 1, Central region, October 2026 ----------------------
MES <- 10
t1 <- tar |> filter(tarifa == "1", mes == MES) |> arrange(bloque_n)
bill_t1 <- function(kwh) sum(pmax(0, pmin(kwh, c(t1$desde_kwh[-1], Inf)) - t1$desde_kwh) * t1$precio_kwh)
d <- dac |> filter(mes == MES, region == "CEN")
bill_dac <- function(kwh) d$cargo_fijo_mes + kwh * d$precio_kwh
ex <- tibble(kwh = c(75, 140, 200, 250)) |>
  mutate(recibo = sapply(kwh, bill_t1), tarifa = "1", por_kwh = recibo / kwh,
         ultimo_kwh = sapply(kwh, function(k) t1$precio_kwh[max(which(t1$desde_kwh < k))])) |>
  bind_rows(tibble(kwh = c(250, 300), tarifa = "DAC") |>
              mutate(recibo = sapply(kwh, bill_dac), por_kwh = recibo / kwh, ultimo_kwh = d$precio_kwh))
print(t1 |> select(bloque_n, desde_kwh, precio_kwh))
print(d)
print(ex |> mutate(across(c(recibo, por_kwh), ~ round(.x, 2))))
