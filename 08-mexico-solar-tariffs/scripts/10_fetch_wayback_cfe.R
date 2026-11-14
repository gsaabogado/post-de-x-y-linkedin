# 10_fetch_wayback_cfe.R ---------------------------------------------------------
# Household electricity prices before 2016, as CFE's old website printed them, from
# the captures kept by the Internet Archive's Wayback Machine.
#
# CFE's site today holds prices for 2016 onward only. Its old site (2002 to 2018, at
# www.cfe.gob.mx/www2, aplicaciones.cfe.gob.mx and app.cfe.gob.mx) answered
#   .../Tarifas/Tarifas.asp?Tarifa=<code>&Anio=<year>[&mes=<month>]
# and the Wayback Machine captured many of those answers. Codes kept here:
#   domesticas2003, DACAnual2003   one table per year, every tariff by month (2003 on)
#   domesticas2, domesticas, DAC02, DAC   the same for 2002, the year DAC was created
#   1, 1A ... 1E                   one tariff, one month per page (years up to 2002)
#
# Steps: (1) ask the Wayback index (CDX) for every capture of those addresses and save
# the two listings; (2) keep one capture per code, year, month asked and content digest;
# (3) download each as archived (the "id_" form returns the original bytes) into
# data/raw/wayback/pages/, one request at a time, and write MANIFEST.csv.
# A file is accepted when the transfer completes and it holds a closing html tag;
# files already on disk are not asked again, so the script can be rerun after a stop.
#
# web.archive.org answered 429 to every request made through a VPN with a Mexican
# exit (2026-10-13) and answered normally without it.
#
# Run from the post root: Rscript scripts/10_fetch_wayback_cfe.R

library(dplyr)
library(httr2)
library(purrr)
library(readr)
library(stringr)
library(tibble)

OUT <- "data/raw/wayback"
UA  <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/126.0 Safari/537.36"
dir.create(file.path(OUT, "pages"), recursive = TRUE, showWarnings = FALSE)

YEARLY  <- c("DOMESTICAS2003", "DACANUAL2003", "DOMESTICAS2", "DOMESTICAS", "DAC02", "DAC")
MONTHLY <- c("1", "1A", "1B", "1C", "1D", "1E")
YEARS_YEARLY  <- 2002:2016     # 2016 overlaps CFE's own archive: a check on both
YEARS_MONTHLY <- 1999:2002

get_txt <- function(url, tries = 5) {
  request(url) |> req_user_agent(UA) |> req_timeout(300) |>
    req_retry(max_tries = tries, backoff = \(i) 20 * i, is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
    req_perform()
}

# (1) the index ------------------------------------------------------------------
cdx_fields <- "original,timestamp,statuscode,digest,length"
cdx_queries <- c(
  app_cfe = paste0("https://web.archive.org/cdx/search/cdx?url=app.cfe.gob.mx/Aplicaciones/CCFE/Tarifas/*",
                   "&output=txt&fl=", cdx_fields, "&filter=statuscode:200&limit=150000"),
  cfe_domain = paste0("https://web.archive.org/cdx/search/cdx?url=cfe.gob.mx&matchType=domain",
                      "&output=txt&fl=", cdx_fields, "&filter=statuscode:200",
                      "&filter=original:.*(arifa|ARIFA).*&to=20091231&limit=150000"))
for (nm in names(cdx_queries)) {
  f <- file.path(OUT, paste0("cdx_", nm, ".txt"))
  if (!file.exists(f)) {
    writeLines(resp_body_string(get_txt(cdx_queries[[nm]])), f)
    Sys.sleep(3)
  }
}
cdx <- map(names(cdx_queries), \(nm) read_delim(file.path(OUT, paste0("cdx_", nm, ".txt")), delim = " ",
        col_names = c("original", "timestamp", "status", "digest", "length"),
        col_types = "ccccc", quote = "")) |> bind_rows() |> distinct()
stopifnot(nrow(cdx) > 20000, !anyNA(cdx$timestamp), all(str_detect(cdx$timestamp, "^\\d{14}$")))
cat("captures listed:", nrow(cdx), "\n")

# (2) the captures wanted ----------------------------------------------------------
par_of <- function(u, key) str_match(u, regex(paste0("[?&]", key, "=([^&]*)"), ignore_case = TRUE))[, 2]
want <- cdx |>
  filter(str_detect(original, regex("/tarifas/tarifas(_casa)?\\.asp\\?", ignore_case = TRUE)),
         !str_detect(original, fixed("404;"))) |>            # "page not found" answers
  mutate(code = str_to_upper(par_of(original, "tarifa")),
         anio = suppressWarnings(as.integer(par_of(original, "anio"))),
         mes  = suppressWarnings(as.integer(par_of(original, "mes")))) |>
  filter(!is.na(anio),
         (code %in% YEARLY & anio %in% YEARS_YEARLY) | (code %in% MONTHLY & anio %in% YEARS_MONTHLY)) |>
  arrange(code, anio, mes, timestamp) |>
  group_by(code, anio, mes, digest) |>
  slice_tail(n = 1) |>                                       # same content: keep the latest capture
  ungroup() |>
  mutate(file = sprintf("%s_%d_%s_%s.html", code, anio, ifelse(is.na(mes), "00", sprintf("%02d", mes)), timestamp))
stopifnot(nrow(want) == nrow(distinct(want, file)), nrow(want) > 100)
cat("captures to hold:", nrow(want), "\n")
print(count(want, code, anio) |> tidyr::pivot_wider(names_from = anio, values_from = n, values_fill = 0), width = 200)

# (3) download -----------------------------------------------------------------------
fetch_one <- function(original, timestamp, file, ...) {
  f <- file.path(OUT, "pages", file)
  if (!file.exists(f) || file.size(f) < 500) {
    resp <- tryCatch(get_txt(paste0("https://web.archive.org/web/", timestamp, "id_/", original)),
                     error = function(e) NULL)
    Sys.sleep(1.5)
    if (is.null(resp)) return(tibble(file = file, bytes = NA_real_, md5 = NA_character_, complete = FALSE))
    writeBin(resp_body_raw(resp), f)
  }
  raw <- readBin(f, "raw", file.size(f))
  txt <- iconv(rawToChar(raw[raw != as.raw(0)]), "latin1", "UTF-8", sub = "")
  tibble(file = file, bytes = file.size(f), md5 = unname(tools::md5sum(f)),
         complete = str_detect(txt, regex("</html>", ignore_case = TRUE)))
}
got <- pmap(want, fetch_one, .progress = FALSE) |> bind_rows()
man <- want |> left_join(got, by = "file") |>
  select(file, code, anio, mes, timestamp, digest, original, bytes, md5, complete)
stopifnot(nrow(man) == nrow(want))
write_csv(man, file.path(OUT, "MANIFEST.csv"))
cat("on disk:", sum(!is.na(man$bytes)), "of", nrow(man), "; not complete:", sum(!man$complete), "\n")
print(man |> group_by(code) |> summarise(files = n(), on_disk = sum(!is.na(bytes)), complete = sum(complete),
                                         years = paste(range(anio), collapse = "-")), n = 20)
