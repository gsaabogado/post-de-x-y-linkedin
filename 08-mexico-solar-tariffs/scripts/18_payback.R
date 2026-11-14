# 18_payback.R -------------------------------------------------------------------
# Payback of a home solar system under net metering, by tariff and consumption,
# with 2026 prices. Month by month on the actual blocks, never on a yearly average
# price: summer blocks in 1C to 1F are large and cheap exactly when the sun is
# strongest.
#
# Inputs (all in hand):
#   data/derived/cfe_bloques_2026.csv, cfe_dac_2026.csv   prices by month and block
#   data/derived/dac_limites.csv                          DAC entry limit by tariff
#   data/derived/mun_tarifas_anual.csv, mun_tarifas_2017.geojson   weights, polygons
#   CFE_MUN_DIR files 2023-2025, sheets Ventas and Usuarios   seasonal shape by tariff
#   Global Solar Atlas monthly PVOUT (MLPV_IN), specific output of a fixed,
#   optimally tilted system, kWh per kWp per day
#
# Parameters, NOT sourced yet: installed cost K (pesos per kWp) and a fixed cost F
# per installation. Results are bands over them.
# Assumptions stated in full in notes.md (section 2026-10-31).
#
# Run from the post root: Rscript scripts/18_payback.R
# Output: data/derived/payback/

library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(sf)
library(stringr)
library(terra)
library(tidyr)

set.seed(1)
readRenviron(".Renviron")
OUT <- "data/derived/payback"
dir.create(OUT, showWarnings = FALSE)

IVA       <- 1.16        # the 16% tax on the bill
MIN_KWH   <- 25          # monthly minimum billed (notes.md, 2026-10-12)
SUMMER    <- 4:9         # six months, April start; the true start varies by locality
PANEL_KW  <- 0.55        # one module; systems come in whole modules
MIN_PANEL <- 2           # smallest system considered
DAYS      <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
PROFILES  <- c("plano", "cfe")              # flat, or the tariff's real seasonal shape
KS        <- c(15000, 20000, 25000, 30000)  # pesos per kWp, parameter
FIXED     <- c(0, 15000)                    # pesos per installation, parameter
CGRID     <- c(50, 75, 91, 100, 108, 125, 138, 140, 150, 175, 200, 217, 225, 250, 267, 300,
               348, 350, 400, 404, 450, 500, 600, 750, 850, 1000, 1250, 1500, 2000, 2500, 3000)
TARIFFS   <- c("1", "1A", "1B", "1C", "1D", "1E", "1F")

# prices ---------------------------------------------------------------------------
blq <- read_csv("data/derived/cfe_bloques_2026.csv", show_col_types = FALSE)
dac <- read_csv("data/derived/cfe_dac_2026.csv", show_col_types = FALSE)
lim <- read_csv("data/derived/dac_limites.csv", col_types = "cd")
stopifnot(setequal(lim$tarifa, TARIFFS), all(1:12 %in% blq$mes), all(1:10 %in% dac$mes))

# DAC is printed to October: November and December carry October forward
dac12 <- bind_rows(dac, dac |> filter(mes == 10) |> mutate(mes = 11),
                   dac |> filter(mes == 10) |> mutate(mes = 12))
stopifnot(nrow(dac12) == 12 * n_distinct(dac$region))

# one table of blocks per tariff and month, in the season that month belongs to
blocks_of <- function(t, m) {
  season <- if (t == "1") "todo el año" else if (m %in% SUMMER) "verano" else "fuera de verano"
  b <- blq[blq$tarifa == t & blq$mes == m & blq$temporada == season, ]
  stopifnot(nrow(b) >= 3)
  b[order(b$bloque_n), ]
}
BLOCKS <- map(setNames(TARIFFS, TARIFFS), \(t) map(1:12, \(m) blocks_of(t, m)))

bill_tariff <- function(q, t, m) {
  b <- BLOCKS[[t]][[m]]
  q <- max(q, MIN_KWH)
  sum(pmax(0, pmin(q, c(b$desde_kwh[-1], Inf)) - b$desde_kwh) * b$precio_kwh) * IVA
}

bill_dac <- function(q, region, m) {
  d <- dac12[dac12$mes == m & dac12$region == region, ]
  stopifnot(nrow(d) == 1)
  p <- if (m %in% SUMMER && !is.na(d$precio_kwh_verano)) d$precio_kwh_verano else d$precio_kwh
  (d$cargo_fijo_mes + q * p) * IVA
}

# solar output by tariff -----------------------------------------------------------
pv_file <- file.path(OUT, "pvout_mun.csv")
if (!file.exists(pv_file)) {
  zip  <- file.path(path.expand(Sys.getenv("MLPV_IN")), "Solar/global_solar_atlas/raw",
                    "Mexico_GISdata_LTAym_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF.zip")
  dir  <- "Mexico_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/monthly"
  r    <- rast(sprintf("/vsizip/%s/%s/PVOUT_%02d.tif", zip, dir, 1:12))
  names(r) <- sprintf("m%02d", 1:12)
  sf_use_s2(FALSE)
  mun  <- st_read("data/derived/mun_tarifas_2017.geojson", quiet = TRUE) |> select(cvegeo)
  mun  <- st_cast(st_collection_extract(st_make_valid(mun), "POLYGON"), "POLYGON")
  mun$id <- match(mun$cvegeo, sort(unique(mun$cvegeo)))
  # burn municipality ids onto the solar grid once, then average every layer by id
  # (polygon-by-polygon extraction out of a zip took over eight minutes and had not ended)
  ids  <- rasterize(vect(mun), r[[1]], field = "id", touches = FALSE)
  z    <- as_tibble(zonal(r, ids, fun = "mean", na.rm = TRUE)) |> filter(!is.na(m01))
  key  <- st_drop_geometry(mun) |> distinct(cvegeo, id)
  pv   <- inner_join(key, z, by = "id") |> select(-id)
  # municipalities smaller than a cell take the value at their centre
  miss <- setdiff(key$cvegeo, pv$cvegeo)
  cat("Municipalities with no cell centre inside:", length(miss), "\n")
  if (length(miss) > 0) {
    pts <- st_point_on_surface(mun[mun$cvegeo %in% miss, ]) |> distinct(cvegeo, .keep_all = TRUE)
    pv  <- bind_rows(pv, bind_cols(cvegeo = pts$cvegeo,
                                   terra::extract(r, vect(pts), ID = FALSE)))
  }
  stopifnot(nrow(pv) == nrow(key), n_distinct(pv$cvegeo) == nrow(key))
  write_csv(pv, pv_file)
}
pv <- read_csv(pv_file, col_types = cols(cvegeo = "c", .default = "d"))

mun <- read_csv("data/derived/mun_tarifas_anual.csv", col_types = cols(cvegeo = "c")) |>
  filter(year == 2025) |>
  select(cvegeo, clientes, tarifa_principal)
stopifnot(nrow(mun) > 2000)
mpv <- inner_join(mun, pv, by = "cvegeo") |>
  filter(if_all(starts_with("m"), ~ !is.na(.x)), tarifa_principal %in% TARIFFS)
cat("Customers kept with a solar reading:",
    round(100 * sum(mpv$clientes) / sum(mun$clientes), 1), "%\n")

# kWh per kWp in each calendar month, customer-weighted over the tariff's municipalities
yield <- mpv |>
  group_by(tarifa_principal) |>
  summarise(across(starts_with("m"), ~ weighted.mean(.x, clientes)), .groups = "drop") |>
  pivot_longer(starts_with("m"), names_to = "mm", values_to = "kwh_kwp_dia") |>
  mutate(mes = as.integer(sub("m", "", mm)), kwh_kwp = kwh_kwp_dia * DAYS[mes]) |>
  rename(tarifa = tarifa_principal) |>
  select(tarifa, mes, kwh_kwp)
stopifnot(nrow(yield) == 12 * length(TARIFFS))
write_csv(yield, file.path(OUT, "yield_tarifa_mes.csv"))
print(yield |> group_by(tarifa) |> summarise(kwh_kwp_ano = round(sum(kwh_kwp))))

# seasonal shape of consumption by tariff -------------------------------------------
# kWh per customer by month, national, 2023-2025. CFE books sales when it bills, and
# homes are billed every two months, so a month's sales mostly reflect the two months
# before it: the shape is moved back one month.
MESES <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto",
           "Septiembre", "Octubre", "Noviembre", "Diciembre")
sh_file <- file.path(OUT, "perfil_mensual_tarifa.csv")
if (!file.exists(sh_file)) {
  raw_dir <- path.expand(Sys.getenv("CFE_MUN_DIR"))
  tmp <- file.path(tempdir(), "cfe_mun"); dir.create(tmp, showWarnings = FALSE)
  files <- list.files(raw_dir, "^cfe_usuarios_municipio_(2023|2024|2025).*\\.(zip|xlsx)$", full.names = TRUE)
  stopifnot(length(files) == 3)
  xlsx <- map_chr(files, \(f) {
    if (str_ends(f, "xlsx")) return(f)
    d <- file.path(tmp, tools::file_path_sans_ext(basename(f))); unzip(f, exdir = d)
    x <- list.files(d, "\\.xlsx$", full.names = TRUE, recursive = TRUE); stopifnot(length(x) == 1); x
  })
  sheet_long <- function(f, sheet) {
    raw <- read_excel(f, sheet, col_names = FALSE, .name_repair = "minimal")
    hr <- which(apply(raw, 1, \(r) any(r %in% "Tarifa")))[1]
    h <- as.character(unlist(raw[hr, ]))
    names(raw) <- make.unique(ifelse(is.na(h) | h == "", paste0("x", seq_along(h)), h))
    yr <- as.integer(str_match(paste(unlist(raw[seq_len(hr), 1]), collapse = " "), "Totales (\\d{4})")[, 2])
    stopifnot(!is.na(yr))
    raw[-seq_len(hr), ] |>
      filter(!is.na(`Cve Mun`), `Cve Mun` != "999", !is.na(`Cve Inegi`),
             !str_detect(coalesce(`Entidad Federativa`, ""), "Nacional|Totales"),
             Tarifa %in% c("01", TARIFFS, "DAC")) |>
      select(Tarifa, any_of(MESES)) |>
      pivot_longer(-Tarifa, names_to = "mes_nom", values_to = "v") |>
      mutate(year = yr, v = as.numeric(v), tarifa = sub("^01$", "1", Tarifa)) |>
      group_by(year, tarifa, mes_nom) |> summarise(v = sum(v), .groups = "drop")
  }
  sh <- map(xlsx, \(f) {
    inner_join(sheet_long(f, "Ventas") |> rename(kwh = v),
               sheet_long(f, "Usuarios") |> rename(clientes = v),
               by = c("year", "tarifa", "mes_nom"))
  }) |> list_rbind() |>
    mutate(mes = match(mes_nom, MESES), kwh_cliente = kwh / clientes)
  stopifnot(!anyNA(sh$mes), n_distinct(sh$year) == 3, n_distinct(sh$mes) == 12)
  shape <- sh |>
    group_by(tarifa, mes) |> summarise(kwh_cliente = mean(kwh_cliente), .groups = "drop") |>
    group_by(tarifa) |> mutate(indice = kwh_cliente / mean(kwh_cliente)) |> ungroup()
  write_csv(shape, sh_file)
}
shape <- read_csv(sh_file, show_col_types = FALSE)
stopifnot(nrow(shape) == 12 * (length(TARIFFS) + 1))
print(shape |> mutate(i = round(indice, 2)) |> select(tarifa, mes, i) |>
        pivot_wider(names_from = mes, values_from = i), n = 10)

# the shape of a tariff, moved back one month, mean 1
shape_of <- function(t) {
  s <- shape[shape$tarifa == t, ]
  s$indice[match(c(2:12, 1), s$mes)]
}

# one household --------------------------------------------------------------------
load_of <- function(c_mean, t, perfil) {
  if (perfil == "plano") rep(c_mean, 12) else c_mean * shape_of(t)
}

# net metering over two years, month by month; the second year is the steady state.
# Credits are in kWh. Sizing is capped at the year's consumption, so none lapses.
billed_year <- function(cons, gen) {
  credit <- 0
  q <- numeric(12)
  for (y in 1:2) for (m in 1:12) {
    net <- cons[m] - gen[m] - credit
    q[m] <- max(net, 0)
    credit <- max(-net, 0)
  }
  q
}

annual <- function(q, t, on_dac, region) {
  sum(map_dbl(1:12, \(m) if (on_dac) bill_dac(q[m], region, m) else bill_tariff(q[m], t, m)))
}

# every whole-module size up to the year's consumption, for one home
sizes_for <- function(t, c_mean, perfil, region = "CEN") {
  cons <- load_of(c_mean, t, perfil)
  y    <- yield$kwh_kwp[yield$tarifa == t][order(yield$mes[yield$tarifa == t])]
  l    <- lim$limite_dac_kwh_mes[lim$tarifa == t]
  dac0 <- mean(cons) > l
  b0   <- annual(cons, t, dac0, region)
  n_max <- max(floor(sum(cons) / (sum(y) * PANEL_KW)), 0)
  if (n_max < MIN_PANEL) n_max <- MIN_PANEL      # keep the smallest system on the table
  map(MIN_PANEL:n_max, \(n) {
    s <- n * PANEL_KW
    q <- billed_year(cons, s * y)
    # a home on DAC: its 12-month average on net consumption decides, or gross does
    dac_net <- dac0 && mean(q) > l
    tibble(tarifa = t, kwh_mes = c_mean, perfil = perfil, region = region, en_dac = dac0,
           paneles = n, kwp = s, bill0 = b0,
           ahorro_neto  = b0 - annual(q, t, dac_net, region),
           ahorro_bruto = b0 - annual(q, t, dac0, region),
           cubre = min(sum(s * y) / sum(cons), 1), sale_dac_neto = dac0 && !dac_net)
  }) |> list_rbind()
}

grid <- expand_grid(t = TARIFFS, c_mean = CGRID, perfil = PROFILES)
res  <- pmap(grid, \(t, c_mean, perfil) sizes_for(t, c_mean, perfil)) |> list_rbind()
stopifnot(!anyNA(res$ahorro_neto), nrow(res) > length(TARIFFS))

# payback under each cost pair, then the size that pays back soonest
pay <- expand_grid(K = KS, F = FIXED) |>
  mutate(d = map2(K, F, \(k, f) res |> mutate(costo = f + k * kwp,
                                              pb_neto  = costo / ahorro_neto,
                                              pb_bruto = costo / ahorro_bruto))) |>
  unnest(d)
write_csv(pay, file.path(OUT, "payback_todos.csv"))

best <- pay |>
  group_by(K, F, tarifa, kwh_mes, perfil) |>
  slice_min(pb_neto, n = 1, with_ties = FALSE) |>
  ungroup()
write_csv(best, file.path(OUT, "payback_mejor.csv"))

# the first look -------------------------------------------------------------------
avg <- read_csv("data/derived/nacional_tarifas_anual.csv", show_col_types = FALSE) |>
  filter(year == 2025, tarifa %in% TARIFFS) |>
  transmute(tarifa, kwh_prom = kwh_mes_cliente, clientes = clientes_dic)
print(avg)

show <- function(k, f, r) {
  best |>
    filter(K == k, F == f, perfil == r) |>
    select(tarifa, kwh_mes, en_dac, paneles, kwp, bill0, ahorro_neto, ahorro_bruto,
           pb_neto, pb_bruto, sale_dac_neto) |>
    mutate(across(c(bill0, ahorro_neto, ahorro_bruto), round), across(c(pb_neto, pb_bruto), ~ round(.x, 1)))
}
options(tibble.width = Inf, tibble.print_max = 400)
print(show(20000, 0, "cfe") |> filter(kwh_mes %in% c(91, 108, 138, 217, 267, 348, 404, 250, 300, 400, 850, 1000)))
