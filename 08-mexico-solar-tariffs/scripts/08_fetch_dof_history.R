# 08_fetch_dof_history.R ---------------------------------------------------------
# Tariff agreements for household electricity before 2017, from the Diario Oficial
# de la Federación (sidof.segob.gob.mx, which answers scripts from anywhere).
#
# CFE's archive page "Acuerdos modificatorios de las tarifas"
# (cfe.gob.mx/hogar/tarifas/Pages/Acuerdosdetarifasant.aspx, read 2026-10-13) lists
# the agreements by date of publication, without links. For each date below the
# script asks the DOF's index of that day, keeps the notes whose title is about
# electricity tariffs, and downloads their text into data/raw/dof/hist/.
# WINDOWS adds runs of days searched for agreements the list gives without a date
# (Acuerdo 015/2014) or does not give at all (the yearly agreements for 2016 and 2017).
#
# The server sends no Content-Length (compressed, chunked), so the check is on
# content: each file must hold its own title and a closing signature ("Rúbrica").
# Writes data/raw/dof/hist/MANIFEST.csv (date, note id, title, file, bytes, md5).
#
# Run from the post root: Rscript scripts/08_fetch_dof_history.R

library(dplyr)
library(httr2)
library(jsonlite)
library(purrr)
library(readr)
library(stringr)
library(tibble)

OUT <- "data/raw/dof/hist"
UA  <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/126.0 Safari/537.36"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# dates printed on CFE's list (household section)
CFE_DATES <- as.Date(c(
  "2002-02-07", "2002-04-08", "2003-01-17", "2003-04-02", "2004-03-30", "2005-01-21",
  "2005-12-28", "2007-06-04", "2007-09-27", "2007-10-01", "2007-12-26", "2011-12-28",
  "2012-04-30", "2012-05-08", "2012-06-29", "2013-04-30", "2013-10-01", "2014-02-28"))
# dates the agreements themselves cite: the yearly adjustment agreements published
# on 30-12-1999, 31-12-2000 and 31-12-2001, before CFE's list starts
CITED_DATES <- as.Date(c("1999-12-30", "2000-12-31", "2001-12-31"))
# notes kept by id because their title does not name electricity: the presidential
# decree of 27-09-2007 that froze household tariffs for the rest of that year
EXTRA_NOTES <- tibble::tibble(fecha = as.Date("2007-09-27"), cod_nota = 5002036L,
  titulo = "Decreto por el que se suspenden los aumentos de precios y tarifas en diversos energéticos",
  dependencia = "PRESIDENCIA DE LA REPUBLICA")
# runs of days around each new year, for agreements without a date on the list
WINDOWS <- list(c("2014-12-15", "2015-01-09"), c("2015-12-15", "2016-01-08"),
                c("2016-12-15", "2017-01-06"))
win_days <- do.call(c, map(WINDOWS, \(w) seq(as.Date(w[1]), as.Date(w[2]), by = "day")))
days <- sort(unique(c(CFE_DATES, CITED_DATES, win_days)))
stopifnot(inherits(days, "Date"), as.Date("2002-02-07") %in% days)

TITLE_RE <- regex("energ[ií]a el[eé]ctrica|tarifas? (final|dom[eé]stic|el[eé]ctric)|suministro b[aá]sico",
                  ignore_case = TRUE)

get_txt <- function(url) {
  request(url) |> req_user_agent(UA) |> req_timeout(120) |> req_retry(max_tries = 3) |>
    req_perform() |> resp_body_string(encoding = "UTF-8")
}

day_notes <- function(d) {
  j <- fromJSON(get_txt(paste0("https://sidof.segob.gob.mx/dof/sidof/notas/", format(d, "%d-%m-%Y"))),
                simplifyVector = FALSE)
  stopifnot(identical(j$messageCode, 200L))
  ed <- c(j$NotasMatutinas, j$NotasVespertinas, j$NotasExtraordinarias)
  tibble(fecha = d,
         cod_nota = map_int(ed, \(n) as.integer(n$codNota %||% NA)),
         titulo = map_chr(ed, \(n) str_squish(n$titulo %||% "")),   # a few index entries have no title
         dependencia = map_chr(ed, \(n) n$codOrgaDos %||% NA_character_))
}

idx <- map(days, \(d) { Sys.sleep(0.3); day_notes(d) }) |> bind_rows()
cat("days asked:", length(days), " notes listed:", nrow(idx), "\n")
hits <- idx |> filter(str_detect(titulo, TITLE_RE)) |> bind_rows(EXTRA_NOTES) |>
  distinct(cod_nota, .keep_all = TRUE) |> arrange(fecha, cod_nota)
missing_days <- c(CFE_DATES, CITED_DATES)[!c(CFE_DATES, CITED_DATES) %in% hits$fecha]
if (length(missing_days)) cat("CFE dates with no matching title:", format(missing_days), "\n")

fetch_note <- function(fecha, cod_nota, titulo, ...) {
  f <- file.path(OUT, sprintf("%s_%d.html", format(fecha, "%Y-%m-%d"), cod_nota))
  txt <- get_txt(paste0("https://sidof.segob.gob.mx/notas/docFuente/", cod_nota))
  writeLines(txt, f, useBytes = TRUE)
  plain <- str_squish(str_replace_all(txt, "<[^>]+>", " "))
  key <- str_sub(str_squish(titulo), 1, 40)
  ok_title <- str_detect(str_to_lower(plain), fixed(str_to_lower(key)))
  ok_end <- str_detect(plain, "R[uú]brica")
  tibble(fecha = fecha, cod_nota = cod_nota, titulo = titulo, file = basename(f),
         bytes = file.size(f), md5 = unname(tools::md5sum(f)), ok_title = ok_title, ok_end = ok_end)
}
man <- pmap(hits, \(fecha, cod_nota, titulo, ...) { Sys.sleep(0.3); fetch_note(fecha, cod_nota, titulo) }) |>
  bind_rows()
stopifnot(nrow(man) == nrow(hits), !anyNA(man$bytes), all(man$bytes > 2000))
write_csv(man, file.path(OUT, "MANIFEST.csv"))
print(man |> transmute(fecha, cod_nota, kb = round(bytes / 1024), ok_title, ok_end,
                       titulo = str_sub(titulo, 1, 95)), n = 80, width = 170)
if (!all(man$ok_title & man$ok_end)) cat("LOOK: some files lack their title or a closing signature\n")
