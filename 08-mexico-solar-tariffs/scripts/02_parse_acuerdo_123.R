# 02_parse_acuerdo_123.R --------------------------------------------------------
# Reads Acuerdo 123/2017 (DOF 30-11-2017, data/raw/dof/) and writes three tables
# to data/derived/: the blocks and prices of the household tariffs 1 to 1F as set
# in December 2017, the DAC entry limits, and the DAC charges by region.
# Prices are those of the agreement; Acuerdo 134/2018 raises the 1 to 1F charges
# every month since January 2019, so these are NOT today's prices.
#
# Run from the post root, after 01: Rscript scripts/02_parse_acuerdo_123.R

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

html <- read_file("data/raw/dof/acuerdo_123_2017_tarifas_domesticas.html")
txt <- html |>
  str_replace_all("(?s)<style.*?</style>|<script.*?</script>", " ") |>
  str_replace_all("<(br|/p|/div|/tr)[^>]*>", "\n") |>
  str_replace_all("<[^>]+>", " ") |>
  str_replace_all("&nbsp;| ", " ") |>
  str_replace_all("[ \t]+", " ")

sec <- str_split(txt, "\n ?TARIFA (?=1|DAC)")[[1]][-1]
names(sec) <- str_extract(sec, "^(1[A-F]?|DAC)")
stopifnot(identical(names(sec), c("1", "1A", "1B", "1C", "1D", "1E", "1F", "DAC")))

# blocks ------------------------------------------------------------------------
blocks <- lapply(names(sec)[1:7], function(tf) {
  lines <- str_split(sec[[tf]], "\n")[[1]] |> str_squish()
  season <- NA_character_
  out <- list()
  for (l in lines) {
    if (str_detect(l, "^2\\.1 Temporada de verano")) season <- "verano"
    if (str_detect(l, "^2\\.2 Temporada fuera de verano")) season <- "fuera de verano"
    if (str_detect(l, "^Consumo (básico|intermedio|excedente)")) {
      out[[length(out) + 1]] <- tibble(
        tarifa = tf,
        temporada = if (tf == "1") "todo el año" else season,
        bloque = str_extract(l, "^Consumo [a-záé ]+(?=:)") |> str_remove("Consumo "),
        precio_kwh = as.numeric(str_extract(l, "(?<=\\$)[0-9.]+")),
        kwh_bloque = as.numeric(str_extract(l, "(?<=(primeros|siguientes) )[0-9,]+") |> str_remove(","))
      )
    }
  }
  bind_rows(out)
}) |> bind_rows() |>
  group_by(tarifa, temporada) |>
  mutate(desde_kwh = lag(cumsum(kwh_bloque), default = 0),
         hasta_kwh = if_else(is.na(kwh_bloque), Inf, cumsum(replace_na(kwh_bloque, 0)))) |>
  ungroup()
stopifnot(!anyNA(blocks$precio_kwh), !anyNA(blocks$temporada),
          nrow(blocks) == 3 + 6 * 2 * 3 + 4,          # 1: 3; 1A-1F: 3 per season, plus one more summer block in 1C-1F
          all(blocks$precio_kwh[blocks$bloque == "excedente"] == 2.802))

# DAC ---------------------------------------------------------------------------
dac <- sec[["DAC"]]
limits <- str_match_all(dac, "Tarifa (1[A-F]?): ([0-9,]+) \\([^)]*\\) kWh/mes")[[1]]
limits <- tibble(tarifa = limits[, 2], limite_dac_kwh_mes = as.numeric(str_remove(limits[, 3], ",")))
stopifnot(nrow(limits) == 7)

fixed <- as.numeric(str_match(dac, "6\\.1 Cargo fijo\\s+\\$([0-9.]+)")[, 2])
ch <- str_match_all(dac, "\n\\s*([A-Z][^\n$]*?)\\s*\n\\s*\\$([0-9.]+) \\([^)]*\\) por cada kilowatt-hora")[[1]]
dac_charges <- tibble(region = str_squish(ch[, 2]), precio_kwh = as.numeric(ch[, 3]), cargo_fijo_mes = fixed)
stopifnot(nrow(dac_charges) == 8, !is.na(fixed))

write_csv(blocks, "data/derived/tarifas_1_1F_dic2017.csv")
write_csv(limits, "data/derived/dac_limites.csv")
write_csv(dac_charges, "data/derived/dac_cargos_dic2017.csv")
print(blocks, n = 50); print(limits); print(dac_charges)
