# 12_parse_wayback_monthly.R -----------------------------------------------------
# Reads the single-month pages of CFE's old website held in data/raw/wayback/pages/
# (codes 1, 1A ... 1E, years 1999 to 2002; downloaded by 10_fetch_wayback_cfe.R).
# These are the only printed prices found for the years before the yearly tables
# start (February 2002).
#
# A page shows ONE month of one tariff as sentences: "Consumo básico $ 0.418 por cada
# uno de los primeros 75 kilowatts-hora." Seasonal tariffs print two sets, one under
# a summer heading and one under an out-of-summer heading.
#
# Which month a page shows:
#   - when the address carries "mes=N", that month (mes_origen = "direccion");
#   - when it does not, the page showed the calendar month of the day it was asked,
#     in the year of the address: the three captures of tariff 1 for 2000 without a
#     month print three different prices, each equal to the page asked with the month
#     of its capture date. Those rows get mes_origen = "fecha de captura" and are
#     checked below against the pages with an explicit month.
#   The month list's own "selected" mark always says ENERO and is not used.
#
# Writes data/derived/wayback_paginas_mensuales.csv, one row per page and price:
#   file, tarifa, anio, mes, mes_origen, captura, temporada, concepto, rango, precio
# and prints, for each tariff and year, the months read.
#
# Run from the post root, after 10: Rscript scripts/12_parse_wayback_monthly.R

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

PAGES <- "data/raw/wayback/pages"
MONTHLY <- c("1", "1A", "1B", "1C", "1D", "1E")

read_text <- function(f) {
  raw <- readBin(f, "raw", file.size(f))
  txt <- rawToChar(raw[raw != as.raw(0)])
  if (validUTF8(txt)) Encoding(txt) <- "UTF-8" else txt <- iconv(txt, "latin1", "UTF-8", sub = "")
  x <- str_replace_all(txt, regex("<(script|style)[^>]*>.*?</\\1>", dotall = TRUE, ignore_case = TRUE), " ")
  x <- str_replace_all(x, "<[^>]+>", " ")
  x <- str_replace_all(x, c("&nbsp;" = " ", "&aacute;" = "á", "&eacute;" = "é", "&iacute;" = "í",
                            "&oacute;" = "ó", "&uacute;" = "ú", "&ntilde;" = "ñ"))
  str_squish(x)
}

PRICE_RE <- regex(paste0("(Consumo (?:b[aá]sico|intermedio(?: bajo| alto)?|excedente)|Cargo fijo)[^$]{0,40}\\$ ?",
                         "(\\d+\\.\\d+)\\s*(por cada[^.$]{0,110})?"), ignore_case = TRUE)
SEASON_RE <- regex("temporada (?:de )?fuera de verano|fuera de verano|temporada de verano|en verano", ignore_case = TRUE)

parse_monthly <- function(file) {
  t <- read_text(file.path(PAGES, file))
  a <- str_locate(t, "Cuotas aplicables")[1, "start"]
  if (is.na(a)) return(NULL)
  b <- str_locate(str_sub(t, a), regex("M[ií]nimo mensual", ignore_case = TRUE))[1, "start"]
  seg <- str_sub(t, a, if (is.na(b)) a + 4000 else a + b - 1)
  pr <- str_locate_all(seg, PRICE_RE)[[1]]
  if (!nrow(pr)) return(NULL)
  mt <- str_match_all(seg, PRICE_RE)[[1]]
  se <- str_locate_all(seg, SEASON_RE)[[1]]
  se_txt <- str_to_lower(str_extract_all(seg, SEASON_RE)[[1]])
  season_at <- function(pos) {
    k <- which(se[, "start"] < pos)
    if (!length(k)) return(NA_character_)
    if (str_detect(se_txt[max(k)], "fuera")) "fuera de verano" else "verano"
  }
  tibble(file = file,
         temporada = map_chr(pr[, "start"], season_at),
         concepto = str_to_lower(mt[, 2]),
         rango = str_squish(mt[, 4]),
         precio = as.numeric(mt[, 3]))
}

files <- tibble(file = list.files(PAGES, pattern = "\\.html$")) |>
  mutate(tarifa = str_match(file, "^([A-Z0-9]+)_")[, 2],
         anio = as.integer(str_match(file, "^[A-Z0-9]+_(\\d{4})_")[, 2]),
         mes_dir = as.integer(str_match(file, "^[A-Z0-9]+_\\d{4}_(\\d{2})_")[, 2]),
         captura = str_match(file, "_(\\d{14})\\.html$")[, 2]) |>
  filter(tarifa %in% MONTHLY) |>
  mutate(mes_origen = if_else(mes_dir == 0L, "fecha de captura", "direccion"),
         mes = if_else(mes_dir == 0L, as.integer(str_sub(captura, 5, 6)), mes_dir))
stopifnot(nrow(files) > 0, !anyNA(files$mes), all(files$mes %in% 1:12))

rows <- map(files$file, parse_monthly) |> bind_rows()
tab <- files |> select(file, tarifa, anio, mes, mes_origen, captura) |> inner_join(rows, by = "file")
cat("pages:", nrow(files), " with prices:", n_distinct(tab$file), "\n")
stopifnot(!anyNA(tab$precio))
write_csv(tab, "data/derived/wayback_paginas_mensuales.csv")

# does a page without a month agree with the page that names the month of its capture?
key <- c("tarifa", "anio", "mes", "temporada", "concepto", "rango")
dir_v <- tab |> filter(mes_origen == "direccion") |> distinct(across(all_of(key)), precio)
cap_v <- tab |> filter(mes_origen == "fecha de captura") |> distinct(across(all_of(key)), precio)
chk <- inner_join(cap_v, dir_v, by = key, suffix = c("_captura", "_direccion"))
cat("prices read both ways:", nrow(chk), " equal:", sum(chk$precio_captura == chk$precio_direccion), "\n")

# values that disagree within the same tariff, month and concept
dis <- tab |> distinct(across(all_of(key)), precio) |> count(across(all_of(key))) |> filter(n > 1)
cat("tariff-month-concept cells with more than one value:", nrow(dis), "\n")

print(tab |> distinct(tarifa, anio, mes) |> arrange(mes) |>
        summarise(meses = paste(mes, collapse = " "), .by = c(tarifa, anio)) |> arrange(tarifa, anio), n = 40)
