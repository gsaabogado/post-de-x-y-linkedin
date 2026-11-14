# 13_join_prices.R ---------------------------------------------------------------
# One monthly table of household electricity prices, 1999 to 2026, from three sources:
#   1999 to January 2002   single-month pages of CFE's old site (12_parse_wayback_monthly.R)
#   8 Feb 2002 to 2015     yearly tables of CFE's old site (11_parse_wayback_yearly.R),
#                          read at the level of each capture and consolidated here
#   2016 to 2026           CFE's own site (07_parse_cfe_history.R), expanded from its
#                          compact series to one row per tariff, season and block
# 2016 is in both CFE's site and the old site's tables: every price both hold must agree,
# and CFE's own reading is kept.
#
# Writes
#   data/derived/precios_hogar_detalle.csv, one row per month (or part of a month) and price:
#     anio, mes, parte      month; parte = days of a part-month ("08-28"), empty otherwise
#     tarifa                1, 1A ... 1F, DAC
#     region                DAC region; "todas" for the DAC fixed charge; empty for 1 to 1F
#     temporada             verano, fuera de verano, todo el año
#     esquema               which price list: "hasta N" and "más de N" (homes below or above
#                           N kWh a month, February 2002 to June 2012), "única" otherwise
#     bloque                basico, intermedio, intermedio_bajo, intermedio_alto, excedente;
#                           DAC: cargo_fijo, energia, energia_1_500, energia_adicional
#     kwh_desde, kwh_hasta  the block's range in kWh a month (kwh_hasta empty when open)
#     precio, unidad        pesos per kWh, or pesos per month for the DAC fixed charge
#     fuente                cfe_sitio, wayback_anual, wayback_mensual
#     captura, capturas, otros_valores, antes_del_mes   (Wayback rows) the capture whose value
#                           is kept, how many captures show the price, values printed by
#                           earlier captures, and whether the kept capture predates the month
#                           (a price printed in advance, not one seen in force)
#   data/derived/precios_hogar_series.csv: July 2012 on, in the series names of
#     cfe_precios_mensual.csv (anio, mes, serie, precio, fuente). Before July 2012 the
#     structure differs (two price lists per tariff), so those months exist in the
#     detailed table only. Summer series cover every month to 2016 (the old tables print
#     all twelve) and February to October from 2017 (CFE's site; see 07).
#
# Rules (decided 2026-10-14, confirmed by Luis 2026-10-17)
#   - Labels are normalised at the level of each capture, then each price takes the value
#     of its LATEST capture.
#   - Left out upstream: 72 DAC02 pages in the agreement layout and the 2 earliest captures
#     (script 11); the 24 table-layout single-month pages (script 12).
#   - Blocks are identified by tariff, season, price list and range, not by the label word,
#     because CFE mislabelled rows (list in `fix_bloque` below).
#   - From July 2012 only captures taken on or after 1 July 2012 count: earlier captures
#     printed planned prices of the two-list structure, which was abolished that day.
#   - A page's first column (December of the year before) is a reprint; it is dropped
#     wherever the page of that year holds the month, after checking the two agree.
#   - No month is filled by compounding factors or carried forward.
#
# Run from the post root, after 07, 11 and 12: Rscript scripts/13_join_prices.R

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

CUT_2012 <- "20120701"                # first day of the three-block structure
BLOQUES <- c("basico", "intermedio", "intermedio_bajo", "intermedio_alto", "excedente")
KEY <- c("tarifa", "region", "temporada", "esquema", "bloque", "anio", "mes", "parte")
ym <- function(anio, mes) as.integer(anio) * 100L + as.integer(mes)

tab <- read_csv("data/derived/wayback_tablas_anuales.csv",
                col_types = cols(captura = col_character(), parte = col_character(),
                                 anio = col_integer(), mes = col_integer(), anio_pagina = col_integer(),
                                 .default = col_guess()))
stopifnot(nrow(tab) > 0, !anyNA(tab$captura), all(nchar(tab$captura) == 14))

# 1. Tariffs 1 to 1F, yearly tables ------------------------------------------------
hog <- tab |>
  filter(code %in% c("DOMESTICAS2003", "DOMESTICAS2", "DOMESTICAS")) |>
  mutate(t = ym(anio, mes),
         etq = etiqueta |> str_replace("^Bsico", "Básico") |> str_replace_all(" ?- ?", "-"),
         palabra = str_to_lower(str_squish(str_extract(etq, "^[^\\d]+"))),
         rango = str_remove_all(str_extract(etq, "[\\d,]+-[\\d,]+"), ","),
         lim = as.integer(str_remove(str_match(esquema, "(\\d[\\d,]*) kWh")[, 2], ",")),
         lista = case_when(str_detect(esquema, "consumos? hasta \\d") ~ "hasta",
                           str_detect(esquema, "consumos? superior") ~ "más de"),
         temporada = coalesce(temporada, "todo el año"))
stopifnot(all(hog$palabra %in% c("básico", "intermedio", "intermedio alto", "intermedio bajo", "excedente")),
          !anyNA(hog$tarifa))

# from July 2012: only captures taken from 1 July 2012, and a single price list
n0 <- nrow(hog)
hog <- filter(hog, !(t >= 201207L & captura < CUT_2012))
cat("rows of July 2012 on from captures taken before 1 July 2012, left out:", n0 - nrow(hog), "\n")
stopifnot(!any(hog$t >= 201207L & hog$lista %in% "hasta"))
hog <- hog |> mutate(lista = if_else(t >= 201207L, NA_character_, lista),
                     esquema = if_else(is.na(lista), "única", paste(lista, lim, "kWh")))

# Blocks. Before July 2012 by the label word, with CFE's mislabels fixed:
#   (a) "Intermedio 1-75": tariff 1B out of summer, upper list, 2002 to 2013; it is the
#       basic block (range from 1, basic price).
#   (b) "Excedente" in tariff 1F's LOWER summer list (up to 750, then 1,200 kWh), 2002 to
#       2012: the list has two rows and this one is the intermediate (price 0.469 to 0.708,
#       equal to 1E's summer intermediate; the true excess was 1.63 to 2.70).
#   (c) the captures of the 2012 page taken after June relabel the old intermediate rows
#       of December 2011 to June 2012 with the new ranges ("Intermedio bajo 176-400" for
#       1D's 176-600); same prices, so the block is the old list's "intermedio" and the
#       range is taken from captures made before July 2012 (see `rango_cons`).
#   Not a mislabel: 1E and 1F out of summer, list up to 250 kWh, print básico 1-75,
#   intermedio 76-200 and an excess block (201-250); the agreement of 7 February 2002
#   (DOF) prints exactly that list.
# From July 2012 by the range's first kWh, against the blocks of 2026 (unchanged since).
b26 <- read_csv("data/derived/cfe_bloques_2026.csv", show_col_types = FALSE) |>
  distinct(tarifa, temporada, bloque_n, kwh_bloque, desde_kwh) |>
  group_by(tarifa, temporada) |>
  mutate(nb = n(),
         bloque = case_when(bloque_n == 1 ~ "basico", bloque_n == nb ~ "excedente",
                            nb == 3 ~ "intermedio", bloque_n == 2 ~ "intermedio_bajo", TRUE ~ "intermedio_alto"),
         kwh_desde = desde_kwh + 1L, kwh_hasta = desde_kwh + kwh_bloque) |>
  ungroup() |> select(tarifa, temporada, bloque, kwh_desde, kwh_hasta)
stopifnot(nrow(b26) == 43)

fix_bloque <- function(palabra, rango, tarifa, temporada, lista, t) {
  case_when(
    palabra == "básico" ~ "basico",
    palabra == "intermedio" & str_detect(coalesce(rango, ""), "^1-") ~ "basico",                    # (a)
    palabra == "excedente" & tarifa == "1F" & temporada == "verano" & lista %in% "hasta" ~ "intermedio",  # (b)
    palabra == "excedente" ~ "excedente",
    # 1F summer, upper list, from January 2003: two intermediate blocks
    tarifa == "1F" & temporada == "verano" & t >= 200301L & t < 201207L & rango %in% "301-1200" ~ "intermedio_bajo",
    tarifa == "1F" & temporada == "verano" & t >= 200301L & t < 201207L & rango %in% "1201-2500" ~ "intermedio_alto",
    t < 201207L ~ "intermedio")                                                                     # (c)
}
old <- hog |> filter(t < 201207L) |>
  mutate(bloque = fix_bloque(palabra, rango, tarifa, temporada, lista, t))
new <- hog |> filter(t >= 201207L) |>
  mutate(kwh_desde = case_when(palabra == "excedente" ~ NA_integer_,
                               TRUE ~ as.integer(str_extract(rango, "^\\d+")))) |>
  left_join(b26 |> select(tarifa, temporada, bloque, k_desde = kwh_desde), by = c("tarifa", "temporada"),
            relationship = "many-to-many") |>
  filter((palabra == "excedente" & bloque == "excedente") | (palabra != "excedente" & k_desde == kwh_desde)) |>
  select(-k_desde, -kwh_desde)
stopifnot(!anyNA(old$bloque),
          nrow(new) == nrow(filter(hog, t >= 201207L)))   # every row of July 2012 on found exactly one block

# rows of the new structure must print the ranges of 2026
chk <- new |> filter(!is.na(rango)) |> distinct(tarifa, temporada, bloque, rango) |>
  left_join(b26, by = c("tarifa", "temporada", "bloque")) |>
  filter(rango != paste0(kwh_desde, "-", kwh_hasta))
if (nrow(chk)) { print(chk); stop("ranges from July 2012 differ from 2026") }
hog <- bind_rows(old, new) |> mutate(region = NA_character_)

# 2. DAC, yearly tables ------------------------------------------------------------
# The season is the heading's ("CUOTAS MENSUALES EN VERANO"), never the row's wording:
# the 2010 page prints "Cuotas por energía consumida en fuera de verano" under the
# summer heading of Baja California, at the summer price (3.085 against 2.654).
# Energy before 22 January 2005: two blocks (1-500 kWh, "kWh Adicionales"); from that day
# one price. Two 2006 captures label the single price "1-500 kWh".
# Noroeste: two seasons until 15 August 2005, one table from 16 August 2005, which the
# pages print under the out-of-summer heading; captures of November and December 2005
# printed August whole (no split) and 0 as a placeholder for September to December out
# of summer. Those whole-month August rows and the zeros are left out.
dac <- tab |>
  filter(code %in% c("DACANUAL2003", "DAC")) |>
  mutate(t = ym(anio, mes), tarifa = "DAC",
         region = region |> str_replace(" Y ", " y "),
         bloque = case_when(str_detect(etiqueta, "Cargo fijo|/mes") ~ "cargo_fijo",
                            t < 200501L | (t == 200501L & parte %in% "01-21") ~
                              case_when(etiqueta == "1-500 kWh" ~ "energia_1_500",
                                        etiqueta == "kWh Adicionales" ~ "energia_adicional"),
                            etiqueta == "kWh Adicionales" ~ NA_character_,
                            TRUE ~ "energia"),
         temporada = coalesce(temporada, "todo el año"),
         lista = NA_character_, esquema = "única", rango = NA_character_)
stopifnot(!anyNA(dac$bloque), !anyNA(dac$region),
          all(dac$temporada[dac$region %in% c("Baja California", "Baja California Sur")] != "todo el año"))
noro_entero <- dac$region == "Noroeste" & dac$t == 200508L & is.na(dac$parte)
cero <- dac$precio == 0
stopifnot(all(dac$region[cero] == "Noroeste"), all(dac$t[cero] %in% 200508:200512))
cat("DAC Noroeste: whole-month August 2005 rows left out:", sum(noro_entero), "; zero placeholders left out:", sum(cero & !noro_entero), "\n")
dac <- dac[!noro_entero & !cero, ] |>
  mutate(temporada = if_else(region == "Noroeste" & (t > 200508L | (t == 200508L & parte %in% "16-31")),
                             "todo el año", temporada))
# the fixed charge is one amount for every region and season in each capture and month
fx <- dac |> filter(bloque == "cargo_fijo") |>
  summarise(n = n_distinct(precio), .by = c(file, anio, mes, parte))
stopifnot(all(fx$n == 1))
dac <- dac |> mutate(region = if_else(bloque == "cargo_fijo", "todas", region),
                     temporada = if_else(bloque == "cargo_fijo", "todo el año", temporada))

# 3. Consolidate the yearly tables ------------------------------------------------
w <- bind_rows(hog, dac) |> mutate(grupo = if_else(tarifa == "DAC", "DAC", tarifa))
stopifnot(!anyNA(w$bloque))

# one price per capture and key (two raw labels of one block on one page must agree)
dup <- w |> summarise(n = n_distinct(precio), .by = c(file, all_of(KEY))) |> filter(n > 1)
if (nrow(dup)) { print(dup); stop("one capture prints two prices for one key") }
w <- distinct(w, file, across(all_of(KEY)), .keep_all = TRUE)

# the December-of-last-year column, where the year's own page holds the month
prev <- w |> filter(anio < anio_pagina)
own <- w |> filter(anio == anio_pagina)
held <- distinct(own, grupo, anio, mes)
latest <- function(d) d |> group_by(across(all_of(KEY))) |> slice_max(captura, n = 1, with_ties = FALSE) |> ungroup()
cmp <- inner_join(latest(semi_join(prev, held, by = c("grupo", "anio", "mes"))) |> select(all_of(KEY), p_prev = precio),
                  latest(own) |> select(all_of(KEY), p_own = precio), by = KEY)
cat("December reprints compared with their own year's page:", nrow(cmp), " differ:", sum(cmp$p_prev != cmp$p_own), "\n")
if (any(cmp$p_prev != cmp$p_own)) print(filter(cmp, p_prev != p_own), n = 50)
# a reprint stays as one more capture of the same price where its key is on the year's own
# page (its later capture date can show the price in force); it goes where the key is not
# (the next year's page prints December under the next year's structure, e.g. tariff 1F's
# lists up to 1,200 kWh for December 2002, when the limit was 750)
otro <- semi_join(prev, held, by = c("grupo", "anio", "mes")) |> anti_join(own, by = KEY)
cat("December reprints under a structure the year's own page does not have, left out:", nrow(otro), "\n")
print(count(otro, grupo, anio))
sin_pagina <- anti_join(prev, held, by = c("grupo", "anio", "mes"))
cat("December reprints with no page of their own year (kept):", nrow(sin_pagina), "\n")
print(count(sin_pagina, grupo, anio, mes))
w <- bind_rows(own, anti_join(prev, otro, by = c("file", KEY)))

# the two codes of 2002 (DOMESTICAS, DOMESTICAS2): where both have a capture taken during
# or after the month, they must agree
dia1 <- function(anio, mes, parte) sprintf("%d%02d%02d", anio, mes, as.integer(coalesce(str_match(parte, "^(\\d+)-")[, 2], "1")))
cc <- w |> filter(code %in% c("DOMESTICAS", "DOMESTICAS2"), captura >= dia1(anio, mes, parte)) |>
  group_by(across(all_of(KEY)), code) |> slice_max(captura, n = 1, with_ties = FALSE) |>
  group_by(across(all_of(KEY))) |> filter(n() == 2) |> summarise(n = n_distinct(precio), .groups = "drop")
cat("2002 prices seen in force under both codes:", nrow(cc), " differ:", sum(cc$n > 1), "\n")
stopifnot(all(cc$n == 1))

# the latest capture's value; the range from captures made before the structure changed
# (see (c) above), else from the latest
anual <- w |>
  mutate(antes_cut = t < 201207L & captura < CUT_2012) |>
  group_by(across(all_of(KEY))) |>
  arrange(captura, .by_group = TRUE) |>
  summarise(captura_ultima = last(captura), capturas = n_distinct(captura),
            otros_valores = { u <- unique(as.character(precio)); paste(u[!u %in% as.character(last(precio))], collapse = " ") },
            rango = if (any(antes_cut & !is.na(rango))) last(rango[antes_cut & !is.na(rango)]) else last(rango),
            lim = last(lim), lista = last(lista),
            precio = last(precio), .groups = "drop") |>
  rename(captura = captura_ultima) |>
  mutate(antes_del_mes = captura < dia1(anio, mes, parte), fuente = "wayback_anual")
stopifnot(!anyNA(anual$precio), all(anual$precio > 0))
cat("yearly tables, prices:", nrow(anual), " printed before their month (kept, flagged):", sum(anual$antes_del_mes), "\n")
print(anual |> filter(antes_del_mes) |> count(anio, tarifa_dac = tarifa == "DAC"), n = 30)

# 4. Single-month pages, before 8 February 2002 --------------------------------------
# One page = one tariff and month, printed as sentences in page order. A seasonal tariff
# may print two sets; a set starts at its basic charge. Before February 2002 no page
# prints a season heading: the basic block gives it away (75 kWh out of summer; 100,
# 125, 150, 175 or 300 kWh in summer for 1A to 1E). Pages of February 2002 on are left
# out: the yearly tables hold those months, and their pages mix the two structures.
men0 <- read_csv("data/derived/wayback_paginas_mensuales.csv",
                 col_types = cols(captura = col_character(), anio = col_integer(), mes = col_integer(), .default = col_guess()))
men <- men0 |>
  filter(ym(anio, mes) < 200202L) |>
  group_by(file) |>
  mutate(set = cumsum(str_detect(concepto, "básico")),
         n_rango = as.integer(str_extract(rango, "\\d+"))) |>
  group_by(file, set) |>
  mutate(base = first(n_rango[str_detect(concepto, "básico")])) |>
  ungroup()
sin_rango <- men |> filter(is.na(base))
cat("single-month pages: sets without a printed basic range, left out:", n_distinct(sin_rango$file, sin_rango$set),
    "(", nrow(sin_rango), "prices )\n")
men <- men |>
  filter(!is.na(base), set > 0) |>
  mutate(temporada = case_when(tarifa == "1" ~ "todo el año", base == 75 ~ "fuera de verano", TRUE ~ "verano"),
         bloque = case_when(str_detect(concepto, "básico") ~ "basico", str_detect(concepto, "intermedio bajo") ~ "intermedio_bajo",
                            str_detect(concepto, "intermedio alto") ~ "intermedio_alto", str_detect(concepto, "intermedio") ~ "intermedio",
                            str_detect(concepto, "excedente") ~ "excedente")) |>
  group_by(file, set) |>
  mutate(kwh_hasta = if_else(bloque == "excedente", NA_integer_, cumsum(if_else(bloque == "excedente", 0L, coalesce(n_rango, 0L)))),
         kwh_desde = lag(kwh_hasta, default = 0L) + 1L) |>
  ungroup()
stopifnot(!anyNA(men$bloque), !any(men$bloque != "excedente" & is.na(men$n_rango)),
          all(men$tarifa != "1" | men$base == 75))
mensual <- men |>
  mutate(region = NA_character_, esquema = "única", parte = NA_character_, t = ym(anio, mes)) |>
  group_by(across(all_of(KEY))) |> arrange(captura, .by_group = TRUE) |>
  summarise(captura_ultima = last(captura), capturas = n_distinct(captura),
            otros_valores = { u <- unique(as.character(precio)); paste(u[!u %in% as.character(last(precio))], collapse = " ") },
            kwh_desde = last(kwh_desde), kwh_hasta = last(kwh_hasta), precio = last(precio), .groups = "drop") |>
  rename(captura = captura_ultima) |>
  mutate(antes_del_mes = captura < dia1(anio, mes, parte), fuente = "wayback_mensual")
cat("single-month pages, prices:", nrow(mensual), " with more than one value:", sum(mensual$otros_valores != ""), "\n")
stopifnot(all(mensual$otros_valores == ""), all(mensual$precio > 0))

# 5. Ranges of the yearly tables in kWh ----------------------------------------------
# Printed ranges where the row has one; a lower list's last block ends at the list's
# limit; an excess block is open.
ORD <- setNames(seq_along(BLOQUES), BLOQUES)
anual <- anual |>
  mutate(r1 = as.integer(str_extract(rango, "^\\d+")), r2 = as.integer(str_extract(rango, "\\d+$"))) |>
  group_by(tarifa, region, temporada, esquema, anio, mes, parte) |>
  arrange(coalesce(ORD[bloque], 0L), .by_group = TRUE) |>
  mutate(ultimo = row_number() == n(),
         kwh_hasta = case_when(!is.na(r2) ~ r2, ultimo & lista %in% "hasta" ~ lim, TRUE ~ NA_integer_),
         kwh_desde = if_else(tarifa == "DAC", NA_integer_, lag(kwh_hasta, default = 0L) + 1L)) |>
  ungroup()
# DAC energy blocks before 22 January 2005
anual <- anual |>
  mutate(kwh_desde = case_when(bloque == "energia_1_500" ~ 1L, bloque == "energia_adicional" ~ 501L, TRUE ~ kwh_desde),
         kwh_hasta = if_else(bloque == "energia_1_500", 500L, kwh_hasta))
bad <- anual |> filter(tarifa != "DAC", is.na(kwh_hasta), bloque != "excedente")
if (nrow(bad)) { print(bad |> count(tarifa, temporada, esquema, bloque, anio)); stop("a closed block has no range") }
stopifnot(!any(anual$tarifa != "DAC" & is.na(anual$kwh_desde)))
# July 2012 on: the ranges of 2026 (checked above to equal the printed ones)
anual <- anual |>
  rows_update(anual |> filter(ym(anio, mes) >= 201207L, tarifa != "DAC") |>
                select(all_of(KEY)) |>
                left_join(b26, by = c("tarifa", "temporada", "bloque")),
              by = KEY)

# 6. CFE's site, 2016 to 2026, expanded to tariffs and blocks ------------------------
SFX <- c("1A", "1B", "1C", "1D", "1E", "1F")
mapa <- bind_rows(
  tibble(serie = "t1_basico", tarifa = c("1", SFX), temporada = c("todo el año", rep("fuera de verano", 6)), bloque = "basico"),
  tibble(serie = "t1_intermedio", tarifa = c("1", SFX), temporada = c("todo el año", rep("fuera de verano", 6)), bloque = "intermedio"),
  tibble(serie = "excedente", tarifa = c("1", SFX, SFX), temporada = c("todo el año", rep("fuera de verano", 6), rep("verano", 6)), bloque = "excedente"),
  tibble(serie = "verano_1A1D_basico", tarifa = c("1A", "1B", "1C", "1D"), temporada = "verano", bloque = "basico"),
  tibble(serie = "verano_1A1D_intermedio", tarifa = c("1A", "1B", "1C", "1D"), temporada = "verano",
         bloque = c("intermedio", "intermedio", "intermedio_bajo", "intermedio_bajo")),
  tibble(serie = "verano_1C1D_intermedio_alto", tarifa = c("1C", "1D"), temporada = "verano", bloque = "intermedio_alto"),
  tibble(serie = "verano_1E1F_basico", tarifa = c("1E", "1F"), temporada = "verano", bloque = "basico"),
  tibble(serie = "verano_1E1F_intermedio_bajo", tarifa = c("1E", "1F"), temporada = "verano", bloque = "intermedio_bajo"),
  tibble(serie = "verano_1E_intermedio_alto", tarifa = "1E", temporada = "verano", bloque = "intermedio_alto"),
  tibble(serie = "verano_1F_intermedio_alto", tarifa = "1F", temporada = "verano", bloque = "intermedio_alto"),
  tibble(serie = "dac_cargo_fijo", tarifa = "DAC", region = "todas", temporada = "todo el año", bloque = "cargo_fijo"),
  tibble(serie = "dac_bc_verano", tarifa = "DAC", region = "Baja California", temporada = "verano", bloque = "energia"),
  tibble(serie = "dac_bc_fuera", tarifa = "DAC", region = "Baja California", temporada = "fuera de verano", bloque = "energia"),
  tibble(serie = "dac_bcs_verano", tarifa = "DAC", region = "Baja California Sur", temporada = "verano", bloque = "energia"),
  tibble(serie = "dac_bcs_fuera", tarifa = "DAC", region = "Baja California Sur", temporada = "fuera de verano", bloque = "energia"),
  tibble(serie = "dac_central", tarifa = "DAC", region = "Central", temporada = "todo el año", bloque = "energia"),
  tibble(serie = "dac_noroeste", tarifa = "DAC", region = "Noroeste", temporada = "todo el año", bloque = "energia"),
  tibble(serie = "dac_norte_noreste", tarifa = "DAC", region = "Norte y Noreste", temporada = "todo el año", bloque = "energia"),
  tibble(serie = "dac_sur_peninsular", tarifa = "DAC", region = "Sur y Peninsular", temporada = "todo el año", bloque = "energia")) |>
  mutate(esquema = "única")
stopifnot(nrow(mapa) == nrow(distinct(mapa, tarifa, region, temporada, bloque)),
          nrow(anti_join(filter(mapa, tarifa != "DAC"), b26, by = c("tarifa", "temporada", "bloque"))) == 0,
          nrow(anti_join(b26, mapa, by = c("tarifa", "temporada", "bloque"))) == 0)

cfe <- read_csv("data/derived/cfe_precios_mensual.csv", show_col_types = FALSE) |>
  mutate(anio = as.integer(anio), mes = as.integer(mes))
stopifnot(setequal(unique(cfe$serie), mapa$serie))
sitio <- cfe |> inner_join(mapa, by = "serie", relationship = "many-to-many") |>
  left_join(b26, by = c("tarifa", "temporada", "bloque")) |>
  mutate(parte = NA_character_, fuente = "cfe_sitio")
stopifnot(nrow(sitio) == sum(table(mapa$serie)[cfe$serie]))

# the expansion must give back CFE's 2026 tables, block by block
b26m <- read_csv("data/derived/cfe_bloques_2026.csv", show_col_types = FALSE) |> mutate(mes = as.integer(mes)) |>
  mutate(kwh_desde = desde_kwh + 1L) |> select(tarifa, temporada, mes, kwh_desde, precio_kwh)
e26 <- sitio |> filter(anio == 2026L, tarifa != "DAC") |> inner_join(b26m, by = c("tarifa", "temporada", "mes", "kwh_desde"))
cat("2026, expanded prices matched to CFE's block table:", nrow(e26), " differ:", sum(e26$precio != e26$precio_kwh), "\n")
stopifnot(nrow(e26) == nrow(b26m), all(e26$precio == e26$precio_kwh))   # the 2026 table holds summer for February to October

# 2016 in both: every price both hold must agree
o16 <- inner_join(anual |> filter(anio == 2016L) |> select(all_of(KEY), p_wb = precio),
                  sitio |> filter(anio == 2016L) |> select(all_of(KEY), p_cfe = precio), by = KEY)
cat("2016, prices in both CFE's site and the old tables:", nrow(o16), " differ:", sum(o16$p_wb != o16$p_cfe), "\n")
if (any(o16$p_wb != o16$p_cfe)) print(filter(o16, p_wb != p_cfe), n = 40)
stopifnot(nrow(o16) > 500, all(o16$p_wb == o16$p_cfe))
solo_wb16 <- anti_join(anual |> filter(anio == 2016L), sitio, by = KEY)
cat("2016 prices only in the old tables (kept; CFE's site lacks summer prices outside February to October):",
    nrow(solo_wb16), "\n")
stopifnot(all(solo_wb16$temporada == "verano"), all(solo_wb16$mes %in% c(1, 11, 12)))

# 7. Join and write -----------------------------------------------------------------
det <- bind_rows(
  mensual,
  anual |> filter(anio < 2016L | (anio == 2016L & !paste(tarifa, region, temporada, bloque, mes) %in%
                                    paste(sitio$tarifa, sitio$region, sitio$temporada, sitio$bloque, sitio$mes)[sitio$anio == 2016L])),
  sitio) |>
  mutate(unidad = if_else(bloque == "cargo_fijo", "pesos por mes", "pesos por kWh"),
         region = if_else(tarifa == "DAC", region, NA_character_)) |>
  select(anio, mes, parte, tarifa, region, temporada, esquema, bloque, kwh_desde, kwh_hasta,
         precio, unidad, fuente, captura, capturas, otros_valores, antes_del_mes) |>
  arrange(tarifa, region, temporada, esquema, match(bloque, c(BLOQUES, "cargo_fijo", "energia_1_500", "energia_adicional", "energia")), anio, mes, parte)
stopifnot(!anyNA(det$precio), all(det$precio > 0), nrow(det) == nrow(distinct(det, across(all_of(KEY)))),
          all(det$bloque %in% c(BLOQUES, "cargo_fijo", "energia", "energia_1_500", "energia_adicional")))
# within each list, prices rise block by block; a block mapped to the wrong row breaks this
# in every month at once, which the jump check below cannot see
ORD2 <- c(basico = 1, intermedio = 2, intermedio_bajo = 2, intermedio_alto = 3, excedente = 4,
          energia_1_500 = 1, energia_adicional = 2)
orden <- det |> filter(bloque %in% names(ORD2)) |>
  mutate(o = ORD2[bloque]) |>
  group_by(tarifa, region, temporada, esquema, anio, mes, parte) |>
  arrange(o, .by_group = TRUE) |>
  summarise(n = n(), sube = all(diff(precio) > 0), dup = any(duplicated(o)), .groups = "drop")
cat("lists checked for prices rising block by block:", nrow(orden), " failing:", sum(!orden$sube | orden$dup), "\n")
if (any(!orden$sube | orden$dup)) { print(filter(orden, !sube | dup), n = 40); stop("prices do not rise block by block") }
write_csv(det, "data/derived/precios_hogar_detalle.csv", na = "")

# series names from July 2012: every row a series collapses must carry one price
ser <- det |> filter(ym(anio, mes) >= 201207L) |>
  inner_join(mapa |> select(-esquema), by = c("tarifa", "region", "temporada", "bloque")) |>
  summarise(n = n_distinct(precio), precio = first(precio), fuente = first(fuente), .by = c(anio, mes, serie))
if (any(ser$n > 1)) { print(filter(ser, n > 1), n = 40); stop("tariffs collapsed into one series disagree") }
ser <- ser |> select(-n) |> arrange(serie, anio, mes)
write_csv(ser, "data/derived/precios_hogar_series.csv")
cat("written: precios_hogar_detalle.csv (", nrow(det), "rows ), precios_hogar_series.csv (", nrow(ser), "rows )\n")

# 8. Checks printed for the record ----------------------------------------------------
# months per year: tariff 1's basic price and DAC Central's energy price
cov <- det |> filter((tarifa == "1" & bloque == "basico") | (tarifa == "DAC" & region == "Central" & bloque %in% c("energia", "energia_1_500"))) |>
  distinct(tarifa, anio, mes) |> count(tarifa, anio) |> pivot_wider(names_from = tarifa, values_from = n, names_prefix = "meses_")
print(cov, n = 40)

# month-over-month changes within each price; anything large away from a known rule change
CAMBIOS <- c(200202L, 200204L, 200301L, 200501L, 200508L, 200710L, 200801L, 201207L, 201501L, 201601L, 201901L)
mm <- det |>
  mutate(t = ym(anio, mes), dia = as.integer(coalesce(str_match(parte, "^(\\d+)")[, 2], "1"))) |>
  arrange(t, dia) |>
  group_by(tarifa, region, temporada, esquema, bloque) |>
  mutate(t_prev = lag(t), p_prev = lag(precio), cambio = precio / p_prev - 1,
         gap = (t %/% 100L - t_prev %/% 100L) * 12L + (t %% 100L - t_prev %% 100L)) |>
  ungroup() |> filter(!is.na(p_prev))
salto <- mm |> filter(gap <= 1L, !t %in% CAMBIOS,
                      (tarifa != "DAC" & abs(cambio) > 0.015) | (tarifa == "DAC" & abs(cambio) > 0.10))
# the agreement of 7 February 2002 raises the upper list's intermediate charge by 1.023 a month
f1023 <- salto$esquema != "única" & str_detect(salto$esquema, "^más de") & salto$bloque %in% c("intermedio", "intermedio_bajo") &
  abs(salto$precio / salto$p_prev - 1.023) < 0.0025
cat("changes from one month to the next above 1.5% (tariffs 1 to 1F) or 10% (DAC) away from known rule changes:", nrow(salto),
    "\n  of which the 1.023 monthly rise of the upper list's intermediate charge:", sum(f1023),
    " years", paste(range(salto$anio[f1023]), collapse = "-"), "\n  the rest:\n")
print(salto[!f1023, ] |> select(tarifa, region, temporada, esquema, bloque, anio, mes, parte, p_prev, precio, cambio, fuente), n = 80)
# gaps: months with no price between two that have one (tariff 1's basic price, DAC Central)
hueco <- mm |> filter(gap > 1L, (tarifa == "1" & bloque == "basico") | (region %in% "Central" & str_detect(bloque, "^energia"))) |>
  distinct(tarifa, desde = t_prev, hasta = t) |> arrange(tarifa, desde)
print(hueco |> summarise(huecos = paste(paste0(desde, "→", hasta), collapse = " "), .by = tarifa))
