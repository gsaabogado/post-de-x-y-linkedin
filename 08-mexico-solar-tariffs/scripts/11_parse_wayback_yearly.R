# 11_parse_wayback_yearly.R ------------------------------------------------------
# Reads the yearly price tables of CFE's old website held in data/raw/wayback/pages/
# (downloaded by 10_fetch_wayback_cfe.R): codes DOMESTICAS2003, DOMESTICAS2, DOMESTICAS
# (tariffs 1 to 1F by month) and DACANUAL2003, DAC02, DAC (high-consumption tariff).
#
# A page is a stack of small tables. Each has a row of month names ("Dic./2009", "Ene.",
# ..., "Dic.") and rows of the form  label | price | price | ...  one price per month
# shown. Above them sit lines that say what the table is: "Tarifa 1A", "TEMPORADA DE
# VERANO", "Para consumo hasta 150 kWh mensuales", "REGION BAJA CALIFORNIA SUR". The
# script walks the page from top to bottom, remembers those lines, and writes one row
# per price with everything remembered at that point. Nothing is filled in or assumed:
# a price row longer than its month row stops the script.
#
# Writes data/derived/wayback_tablas_anuales.csv, one row per page, table row and month:
#   file, code, anio_pagina, captura (timestamp of the capture)
#   tarifa, region, temporada, esquema (consumption range the table applies to)
#   concepto (line just above the row when the label alone does not say what it is),
#   etiqueta (the row's label), fila (order of the row on the page), anio, mes,
#   parte (days of a part-month such as "22-31", empty for a whole month), precio
# and wayback_tablas_anuales_consolidado.csv: for each price, the value of the latest
# capture, the captures that show it, the other values printed earlier, and whether the
# latest capture predates the month (antes_del_mes; see the note above `cons`).
#
# Run from the post root, after 10: Rscript scripts/11_parse_wayback_yearly.R

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

PAGES <- "data/raw/wayback/pages"
YEARLY <- c("DOMESTICAS2003", "DACANUAL2003", "DOMESTICAS2", "DOMESTICAS", "DAC02", "DAC")
MES_ABR <- c(Ene = 1, Feb = 2, Mar = 3, Abr = 4, May = 5, Jun = 6, Jul = 7, Ago = 8, Sep = 9, Oct = 10, Nov = 11, Dic = 12)

read_page <- function(f) {
  raw <- readBin(f, "raw", file.size(f))
  raw <- raw[raw != as.raw(0)]
  txt <- rawToChar(raw)
  if (validUTF8(txt)) Encoding(txt) <- "UTF-8" else txt <- iconv(txt, "latin1", "UTF-8", sub = "")
  txt
}

# the page as ordered rows of cells. A row ends at a row, paragraph or line-break tag,
# never at a line break of the source: the 2012 tables write the cells of July to
# December on separate source lines. Empty cells are kept ("") because they hold a
# column: the rows of the structure that began on 1 July 2012 leave December 2011 to
# June 2012 blank. Comments are dropped (the 2012 pages comment out a paragraph).
flatten <- function(html) {
  x <- str_replace_all(html, regex("<(script|style)[^>]*>.*?</\\1>", dotall = TRUE, ignore_case = TRUE), " ")
  x <- str_replace_all(x, regex("<!--.*?-->", dotall = TRUE), " ")
  x <- str_replace_all(x, "[\r\n\t]+", " ")
  x <- str_replace_all(x, regex("</t[dh]>", ignore_case = TRUE), "\u001f")
  x <- str_replace_all(x, regex("<(/tr|/p|br|/div|/table|table|/h\\d|/li|tr)\\b[^>]*>", ignore_case = TRUE), "\n")
  x <- str_replace_all(x, "<[^>]+>", " ")
  x <- str_replace_all(x, c("&nbsp;" = " ", "&aacute;" = "á", "&eacute;" = "é", "&iacute;" = "í",
                            "&oacute;" = "ó", "&uacute;" = "ú", "&ntilde;" = "ñ", "&amp;" = "&",
                            "&Aacute;" = "Á", "&Eacute;" = "É", "&Iacute;" = "Í", "&Oacute;" = "Ó",
                            "&Uacute;" = "Ú", "&#243;" = "ó", "&#237;" = "í", "&#225;" = "á", "&#233;" = "é"))
  lines <- str_split(x, "\n")[[1]]
  cells <- map(lines, \(l) {
    v <- str_squish(str_split(l, "\u001f")[[1]])
    if (length(v) > 1 && v[length(v)] == "") v <- v[-length(v)]   # text after the last cell
    v
  })
  cells[map_lgl(cells, \(v) any(v != ""))]
}

NUM_RE <- "^\\$? ?\\d{1,3}(\\.\\d+)?$"
MONTH_RE <- "^(Ene|Feb|Mar|Abr|May|Jun|Jul|Ago|Sep|Oct|Nov|Dic)\\.?(/ ?(\\d{4}))?$"
# part of a month, filed under that month with its days in `parte`: "del 8 al 28 de feb"
# on the 2002 tables (the agreement of 7 February 2002 took effect the next day),
# "8-28 Feb." on the DAC pages of 2002, "9 de Abr" for tariff 1F (created by the
# agreement of 8 April 2002), "Ene. 01-21" and "Ene. 22-31" on the DAC pages of 2005
# (the agreement of 21 January 2005 changed DAC from the 22nd)
PART_RE  <- regex("^(?:del \\d+ al \\d+ de|\\d+ ?- ?\\d+|\\d+ de) (ene|feb|mar|abr|may|jun|jul|ago|sep|oct|nov|dic)", ignore_case = TRUE)
PART2_RE <- "^(Ene|Feb|Mar|Abr|May|Jun|Jul|Ago|Sep|Oct|Nov|Dic)\\.? (\\d{1,2}) ?- ?(\\d{1,2})$"
month_cell <- function(c) {
  m <- str_match(c, MONTH_RE)
  if (!is.na(m[1])) return(c(m[c(2, 4)], NA))
  dias <- str_extract_all(c, "\\d+")[[1]]
  p <- str_match(c, PART_RE)
  if (!is.na(p[1])) return(c(str_to_title(p[2]), NA, paste0(sprintf("%02d", as.integer(dias)), collapse = "-")))
  p <- str_match(c, PART2_RE)
  if (!is.na(p[1])) return(c(p[2], NA, sprintf("%02d-%02d", as.integer(p[3]), as.integer(p[4]))))
  NULL
}
last_day <- function(yr, mo) as.integer(format(seq(as.Date(sprintf("%d-%02d-01", yr, mo)), by = "month", length.out = 2)[2] - 1, "%d"))

parse_yearly <- function(file, anio_pagina) {
  L <- flatten(read_page(file.path(PAGES, file)))
  st <- list(tarifa = NA_character_, region = NA_character_, temporada = NA_character_,
             esquema = NA_character_, concepto = NA_character_)
  months <- list(); out <- list(); fila <- 0L
  used <- FALSE                       # TRUE once the current month row has served a price row
  for (row in L) {
    cells <- row[row != ""]                                      # what the row says
    one <- paste(cells, collapse = " ")
    # month names: one per line or several in one row ("CARGOS POR" and "Rango de
    # consumo" head the label column, and "CARGOS POR" recurs mid-row in 2005)
    hdr <- cells[!str_detect(cells, "^(CARGOS POR|Rango de consumo)")]
    mc <- map(hdr, month_cell)
    if (length(hdr) && !any(map_lgl(mc, is.null))) {
      for (m in mc) {
        yr <- if (!is.na(m[2])) as.integer(m[2]) else anio_pagina
        mo <- MES_ABR[[m[1]]]
        parte <- m[3]
        if (!is.na(parte) && !str_detect(parte, "-")) parte <- sprintf("%s-%02d", parte, last_day(yr, mo))  # "9 de Abr"
        # a month name after a price row, or one that carries a year ("Dic./2009"),
        # opens a new month row
        if (used || !is.na(m[2])) { months <- list(); used <- FALSE }
        months[[length(months) + 1]] <- c(yr, mo, parte)
      }
      next
    }
    # a price row: a label, then one cell per month, empty where the month has no price.
    # A cell holding several prices (a closing tag missing, DAC 2005) counts as several;
    # on the DAC pages of 2005 a second label in the middle starts the prices of the new
    # structure (from 22 January 2005) and takes no month.
    li <- which(row != "")[1]
    after <- row[-seq_len(li)]
    after <- unlist(map(after, \(x) if (str_detect(x, "^(\\d+\\.\\d+ )+\\d+\\.\\d+$")) str_split(x, " ")[[1]] else x))
    filled <- after[after != ""]
    is_num <- str_detect(filled, NUM_RE)
    if (!str_detect(row[li], NUM_RE) && length(filled) >= 1 && is_num[1] && is_num[length(filled)] && sum(!is_num) <= 1) {
      lab <- after != "" & !str_detect(after, NUM_RE)
      etq_pos <- c(row[li], after[lab])[cumsum(lab) + 1][!lab]     # label in force at each position
      pos <- after[!lab]
      if (length(pos) > length(months))
        stop(file, ": row '", row[li], "' has ", length(pos), " columns for ", length(months), " months")
      k <- which(pos != "")
      v <- as.numeric(str_remove_all(pos[k], "[$ ]"))
      fila <- fila + 1L; used <- TRUE
      mm <- do.call(rbind, months[k])
      out[[length(out) + 1]] <- tibble(tarifa = st$tarifa, region = st$region, temporada = st$temporada,
        esquema = st$esquema, concepto = st$concepto, etiqueta = etq_pos[k], fila = fila,
        anio = as.integer(mm[, 1]), mes = as.integer(mm[, 2]), parte = mm[, 3], precio = v)
      next
    }
    if (length(cells) > 1) next                                  # layout rows, notes
    # "CARGOS POR" heads a month row, and appears twice in the 2005 DAC headers
    if (str_detect(one, "^CARGOS POR")) { if (used) { months <- list(); used <- FALSE }; next }
    if (str_detect(one, regex("^Tarifa (1[A-F]?|DAC)\\b", ignore_case = TRUE)) && nchar(one) < 40) {
      st$tarifa <- str_to_upper(str_match(one, regex("^Tarifa (1[A-F]?|DAC)", ignore_case = TRUE))[2])
      st$temporada <- NA_character_; st$esquema <- NA_character_; st$concepto <- NA_character_
    } else if (str_detect(one, regex("^REGI[OÓ]N ", ignore_case = TRUE)) && nchar(one) < 80) {
      st$region <- str_to_title(str_remove(one, regex("^REGI[OÓ]N ", ignore_case = TRUE)))
      st$temporada <- NA_character_; st$esquema <- NA_character_; st$concepto <- NA_character_
    } else if (str_detect(one, regex("^(CUOTAS MENSUALES|TEMPORADA) FUERA DE VERAN[O0]", ignore_case = TRUE)) && nchar(one) < 60) {
      st$temporada <- "fuera de verano"; st$esquema <- NA_character_; st$concepto <- NA_character_
    } else if (str_detect(one, regex("^(CUOTAS MENSUALES EN|TEMPORADA DE) VERAN[O0]", ignore_case = TRUE)) && nchar(one) < 60) {
      st$temporada <- "verano"; st$esquema <- NA_character_; st$concepto <- NA_character_
    } else if (str_detect(one, regex("^Para consumos? ", ignore_case = TRUE)) && nchar(one) < 80) {
      st$esquema <- one; st$concepto <- NA_character_
    } else if (nchar(one) < 90) {
      st$concepto <- one
    }
  }
  bind_rows(out)
}

files <- tibble(file = list.files(PAGES, pattern = "\\.html$")) |>
  mutate(code = str_match(file, "^([A-Z0-9]+)_")[, 2],
         anio_pagina = as.integer(str_match(file, "^[A-Z0-9]+_(\\d{4})_")[, 2]),
         captura = str_match(file, "_(\\d{14})\\.html$")[, 2]) |>
  filter(code %in% YEARLY)
stopifnot(nrow(files) > 0, !anyNA(files$anio_pagina), !anyNA(files$captura))

# Most DAC02 pages (2002 to 2004) reprint the agreement of 7 February 2002 instead:
# "Cuotas aplicables en el mes de ...", one month per page, regions in rows and charges
# in columns, no row of month names. The month-column pages of DAC and DACANUAL2003
# cover the same months, so these are left out here (a cross-check for later).
acuerdo <- map_lgl(files$file, \(f) str_detect(read_page(file.path(PAGES, f)), regex("aplicables en el mes", ignore_case = TRUE)))
cat("pages in the agreement layout, left out:", sum(acuerdo), "\n"); print(count(files[acuerdo, ], code, anio_pagina))
files <- files[!acuerdo, ]

# The two earliest captures (18 and 22 February 2002) use a first layout: months in
# capitals ("MAR"), "T A R I F A 1" merged with its consumption range, region merged
# with season. Later captures of the same tables (7 of DOMESTICAS, 21 of DOMESTICAS2,
# 31 of DAC) cover the same months, and early captures carried errors that CFE
# corrected later (0.560 for 0.654 in February 2002), so these two are left out.
EARLY <- c("DOMESTICAS_2002_00_20020218060302.html", "DAC_2002_00_20020222164008.html")
stopifnot(all(EARLY %in% files$file))
files <- filter(files, !file %in% EARLY)

tab <- pmap(files, \(file, code, anio_pagina, captura) {
  p <- parse_yearly(file, anio_pagina)
  if (!nrow(p)) return(NULL)
  mutate(p, file = file, code = code, anio_pagina = anio_pagina, captura = captura, .before = 1)
}) |> bind_rows()
empty <- files$file[!files$file %in% tab$file]
cat("pages:", nrow(files), " with prices:", n_distinct(tab$file), " without:", length(empty), "\n")
stopifnot(!anyNA(tab$precio), !anyNA(tab$anio), all(tab$mes %in% 1:12))
write_csv(tab, "data/derived/wayback_tablas_anuales.csv")

# one value per price: the LATEST capture's. Where captures disagree, the later one is
# right in every case checked (2026-10-14): CFE printed the household prices of the
# whole year in advance, so captures of June 2007 show the planned rises of October to
# December 2007 that the decree of 27 September 2007 cancelled (0.639 against 0.637);
# and early printings were corrected later (0.560 for 0.654 in tariff 1, February
# 2002, until April 2002; DAC "kWh adicionales" of December 2004, 2.811 against 2.559,
# in March 2005). `antes_del_mes` flags a price whose latest capture predates its
# month (or part-month): a planned price, not one seen in force.
key <- c("code", "tarifa", "region", "temporada", "esquema", "concepto", "etiqueta", "anio", "mes", "parte")
cons <- tab |>
  group_by(across(all_of(key))) |>
  arrange(captura, .by_group = TRUE) |>
  # `precio` is summarised last: summarise() reads later names as already summarised
  summarise(captura_ultima = last(captura),
            capturas_total = n_distinct(captura), capturas_precio = n_distinct(captura[precio == last(precio)]),
            valores_distintos = n_distinct(precio),
            otros_valores = { u <- unique(as.character(precio)); paste(u[!u %in% as.character(last(precio))], collapse = " ") },
            precio = last(precio), .groups = "drop") |>
  mutate(dia1 = as.integer(coalesce(str_match(parte, "^(\\d+)-")[, 2], "1")),
         antes_del_mes = as.Date(substr(captura_ultima, 1, 8), "%Y%m%d") < as.Date(sprintf("%d-%02d-%02d", anio, mes, dia1))) |>
  select(-dia1)
stopifnot(nrow(cons) == nrow(distinct(tab, across(all_of(key)))), !anyNA(cons$antes_del_mes))
write_csv(cons, "data/derived/wayback_tablas_anuales_consolidado.csv")
cat("prices:", nrow(cons), " with more than one value across captures:", sum(cons$valores_distintos > 1),
    " latest capture before the month:", sum(cons$antes_del_mes), "\n")
print(tab |> distinct(code, anio, mes) |> count(code, anio, name = "meses") |>
        pivot_wider(names_from = code, values_from = meses) |> arrange(anio), n = 30)
