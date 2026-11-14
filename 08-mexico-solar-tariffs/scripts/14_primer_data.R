# 14_primer_data.R ---------------------------------------------------------------
# Tables for the primer on household electricity prices (the page Luis reads to see
# how pricing changed since the 2000s). Everything comes from tables already on disk:
#   precios_hogar_detalle.csv (script 13), the INPC from Banxico (INPC_CSV in .Renviron),
#   the regulator's cost-based tariffs (script 03), CFE customers by municipality and
#   tariff 2010-2017 (MLPV_IN), municipal polygons (script 06), Acuerdo 123/2017 (DOF).
#
# Writes to data/derived/primer/:
#   inpc.csv            INPC by month and the factor to pesos of the latest INPC month
#   facturas.csv        monthly bill (before VAT) of fixed example homes, nominal and real
#   cuotas_t1.csv       tariff 1 block prices by month, nominal and real
#   dac_regiones.csv    DAC energy price by region and month, nominal and real
#   costo_vs_precio.csv tariff 1 bill against the regulator's cost-based bill, by division
#   clientes_tarifa.csv household customers, kWh and kWh per customer by tariff and year
#   mun_region_dac.csv  each municipality's DAC region (Acuerdo 123/2017, numeral 6.3.1)
#   escalon_dac.csv     bill at each tariff's DAC limit, on the tariff and on DAC, by month
#
# A bill here is the energy charge only: blocks times prices, plus the DAC fixed
# charge; no VAT, no other line of the receipt. A month split in parts takes the part
# in force at its end. Real values are in pesos of the latest INPC month; months after
# it have no real value.
#
# Run from the post root, after 13: Rscript scripts/14_primer_data.R

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(sf)

readRenviron(".Renviron")
OUT <- "data/derived/primer"
dir.create(OUT, showWarnings = FALSE)
ym <- function(a, m) as.integer(a) * 100L + as.integer(m)

# INPC --------------------------------------------------------------------------
txt <- readBin(path.expand(Sys.getenv("INPC_CSV")), "raw", 1e6) |> rawToChar() |> iconv("latin1", "UTF-8")
m <- str_match_all(txt, "(\\d{2})/(\\d{2})/(\\d{4}),([\\d.]+)")[[1]]
inpc <- tibble(anio = as.integer(m[, 4]), mes = as.integer(m[, 3]), inpc = as.numeric(m[, 5]))
stopifnot(nrow(inpc) > 400, !anyNA(inpc$inpc),
          inpc$inpc[inpc$anio == 2021 & inpc$mes == 12] == 117.308)
ULT <- slice_tail(inpc, n = 1)
inpc <- mutate(inpc, a_pesos_ult = ULT$inpc / inpc)
write_csv(inpc, file.path(OUT, "inpc.csv"))
cat("INPC to", ULT$anio, ULT$mes, ":", ULT$inpc, "\n")

# prices: one row per key and month, the part in force at the month's end ---------
det <- read_csv("data/derived/precios_hogar_detalle.csv", show_col_types = FALSE,
                col_types = cols(parte = "c", captura = "c", region = "c", otros_valores = "c")) |>
  mutate(dia = as.integer(coalesce(str_match(parte, "^(\\d+)")[, 2], "1"))) |>
  group_by(tarifa, region, temporada, esquema, bloque, anio, mes) |>
  slice_max(dia, n = 1, with_ties = FALSE) |> ungroup()

# bill of a home of tariff `tar`, season `temp`, at `kwh` a month, every month it can be priced
factura_hogar <- function(tar, temp, kwh) {
  d <- det |> filter(tarifa == tar, temporada == temp)
  lim <- as.integer(str_extract(d$esquema, "\\d+"))
  d |>
    filter(esquema == "única" | (str_starts(esquema, "hasta") & kwh <= lim) | (str_starts(esquema, "más") & kwh > lim)) |>
    mutate(kwh_en_bloque = pmax(0, pmin(kwh, coalesce(kwh_hasta, Inf)) - (kwh_desde - 1))) |>
    group_by(anio, mes) |>
    # a month counts only when its blocks cover the kWh with no hole or overlap
    filter(sum(kwh_en_bloque) == max(kwh, 25)) |>
    summarise(factura = sum(kwh_en_bloque * precio), .groups = "drop")
}
factura_dac <- function(reg, temp, kwh) {
  d <- det |> filter(tarifa == "DAC", (region == reg & temporada == temp) | bloque == "cargo_fijo") |>
    # January 2005 holds both energy schemes (two blocks to the 21st, one price from the
    # 22nd): like every split month, take the part in force at the month's end
    group_by(anio, mes) |>
    filter(!(any(bloque == "energia") & bloque %in% c("energia_1_500", "energia_adicional"))) |> ungroup()
  d |>
    mutate(cant = case_when(bloque == "cargo_fijo" ~ 1, bloque == "energia" ~ kwh,
                            bloque == "energia_1_500" ~ min(kwh, 500), bloque == "energia_adicional" ~ max(kwh - 500, 0))) |>
    group_by(anio, mes) |>
    filter(any(bloque == "cargo_fijo"), any(bloque != "cargo_fijo")) |>
    summarise(factura = sum(cant * precio), .groups = "drop")
}
CASOS <- tribble(
  ~caso, ~etiqueta, ~tarifa, ~temporada, ~kwh, ~region,
  "t1_100", "Tariff 1, 100 kWh a month", "1", "todo el año", 100, NA,
  "t1_200", "Tariff 1, 200 kWh a month", "1", "todo el año", 200, NA,
  "t1_250", "Tariff 1, 250 kWh a month", "1", "todo el año", 250, NA,
  "1f_ver_800", "Tariff 1F in summer, 800 kWh a month", "1F", "verano", 800, NA,
  "dac_cen_250", "DAC, Central region, 250 kWh a month", "DAC", "todo el año", 250, "Central",
  "dac_cen_400", "DAC, Central region, 400 kWh a month", "DAC", "todo el año", 400, "Central")
fac <- pmap(CASOS, \(caso, etiqueta, tarifa, temporada, kwh, region) {
  f <- if (tarifa == "DAC") factura_dac(region, temporada, kwh) else factura_hogar(tarifa, temporada, kwh)
  mutate(f, caso = caso, etiqueta = etiqueta, kwh = kwh)
}) |> bind_rows() |>
  left_join(select(inpc, anio, mes, a_pesos_ult), by = c("anio", "mes")) |>
  mutate(factura_real = factura * a_pesos_ult, por_kwh = factura / kwh) |>
  select(-a_pesos_ult) |> arrange(caso, anio, mes)
# check against script 05: October 2026, 250 kWh, 621 on tariff 1 and 1,828 on DAC Central
chk <- fac |> filter(anio == 2026, mes == 10, caso %in% c("t1_250", "dac_cen_250"))
print(chk)
stopifnot(nrow(chk) == 2, round(chk$factura[chk$caso == "t1_250"]) == 621, round(chk$factura[chk$caso == "dac_cen_250"]) == 1828)
# tariff 1 has a bill every month from February 2002
stopifnot(all(count(filter(fac, caso == "t1_100", anio %in% 2003:2025), anio)$n == 12))
write_csv(fac, file.path(OUT, "facturas.csv"))

# tariff 1 block prices (single list from July 2012; before, the list of homes above 140 kWh)
t1 <- det |> filter(tarifa == "1", esquema %in% c("única", "más de 140 kWh", "hasta 140 kWh")) |>
  select(anio, mes, esquema, bloque, kwh_desde, kwh_hasta, precio) |>
  left_join(select(inpc, anio, mes, a_pesos_ult), by = c("anio", "mes")) |>
  mutate(precio_real = precio * a_pesos_ult) |> select(-a_pesos_ult)
write_csv(t1, file.path(OUT, "cuotas_t1.csv"))

# DAC energy price by region (single price from 22 January 2005; before, the first 500 kWh)
dacr <- det |> filter(tarifa == "DAC", bloque %in% c("energia", "energia_1_500", "cargo_fijo")) |>
  select(anio, mes, region, temporada, bloque, precio) |>
  left_join(select(inpc, anio, mes, a_pesos_ult), by = c("anio", "mes")) |>
  mutate(precio_real = precio * a_pesos_ult) |> select(-a_pesos_ult)
write_csv(dacr, file.path(OUT, "dac_regiones.csv"))

# cost-based tariff against tariff 1, 150 kWh a month (DB1 is the category up to 150) -------
cne <- read_csv("data/derived/cne_db1_db2_mensual.csv", show_col_types = FALSE) |>
  filter(cat == "DB1") |>
  select(anio, mes = mes_n, division, cargo, total) |>
  pivot_wider(names_from = cargo, values_from = total) |>
  rename(fijo = `Único`, var = Variable) |>
  mutate(anio = as.integer(anio), mes = as.integer(mes), costo_150 = fijo + 150 * var)
stopifnot(!anyNA(cne$costo_150), n_distinct(cne$division) == 17)
t1_150 <- factura_hogar("1", "todo el año", 150)
cvp <- cne |> inner_join(t1_150, by = c("anio", "mes")) |>
  mutate(pct_cubierto = 100 * factura / costo_150) |>
  select(anio, mes, division, costo_150, factura_t1_150 = factura, pct_cubierto)
stopifnot(nrow(cvp) == nrow(cne))
write_csv(cvp, file.path(OUT, "costo_vs_precio.csv"))

# customers and consumption by tariff, 2010-2017 -------------------------------------
cfe <- read_csv(file.path(path.expand(Sys.getenv("MLPV_IN")), "Electricity/cfe_users_consumption_mun/cfe_users_consumption_mun_2010_2017.csv"),
                col_types = cols(cvegeo = "c", tarifa = "c", cve_ent = "c", cve_mun = "c", entidad = "c", municipio = "c", .default = "d"))
cli <- cfe |> filter(tarifa %in% c("01", "1A", "1B", "1C", "1D", "1E", "1F", "DAC")) |>
  mutate(tarifa = if_else(tarifa == "01", "1", tarifa)) |>
  group_by(anio = as.integer(year), tarifa) |>
  summarise(clientes = sum(users, na.rm = TRUE), kwh_anual = sum(kwh, na.rm = TRUE), .groups = "drop") |>
  group_by(anio) |>
  mutate(pct_clientes = 100 * clientes / sum(clientes), pct_kwh = 100 * kwh_anual / sum(kwh_anual),
         kwh_mes_cliente = kwh_anual / clientes / 12) |> ungroup()
stopifnot(n_distinct(cli$anio) == 8, nrow(cli) == 64, all(cli$clientes > 0))
print(cli |> filter(tarifa == "DAC"))
write_csv(cli, file.path(OUT, "clientes_tarifa.csv"))

# DAC regions by municipality (Acuerdo 123/2017, numeral 6.3.1) ------------------------
norm <- function(x) x |> str_to_lower() |> stringi::stri_trans_general("Latin-ASCII") |> str_replace_all("[^a-z ]", " ") |> str_squish()
mun <- read_csv("data/derived/mun_tarifas_2017.csv", show_col_types = FALSE, col_types = cols(cvegeo = "c")) |>
  select(cvegeo, entidad, municipio) |> mutate(ent = norm(entidad), mn = norm(municipio))
g <- st_read("data/derived/mun_tarifas_2017.geojson", quiet = TRUE)
stopifnot("cvegeo" %in% names(g))
# every polygon, with state names from INEGI's state code
ENT <- c("01" = "Aguascalientes", "02" = "Baja California", "03" = "Baja California Sur", "04" = "Campeche",
         "05" = "Coahuila de Zaragoza", "06" = "Colima", "07" = "Chiapas", "08" = "Chihuahua", "09" = "Ciudad de México",
         "10" = "Durango", "11" = "Guanajuato", "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco", "15" = "México",
         "16" = "Michoacán de Ocampo", "17" = "Morelos", "18" = "Nayarit", "19" = "Nuevo León", "20" = "Oaxaca",
         "21" = "Puebla", "22" = "Querétaro", "23" = "Quintana Roo", "24" = "San Luis Potosí", "25" = "Sinaloa",
         "26" = "Sonora", "27" = "Tabasco", "28" = "Tamaulipas", "29" = "Tlaxcala", "30" = "Veracruz de Ignacio de la Llave",
         "31" = "Yucatán", "32" = "Zacatecas")
pol <- g |> st_drop_geometry() |> select(cvegeo, any_of(c("nombre", "municipio", "NOMGEO"))) |>
  mutate(cve_ent = substr(cvegeo, 1, 2))
nm_col <- intersect(c("nombre", "municipio", "NOMGEO"), names(pol))[1]
stopifnot(!is.na(nm_col))
pol <- pol |> mutate(mn = norm(.data[[nm_col]]))
# names as printed in the Acuerdo -> the polygon's name where they differ
ALIAS <- c("chalco de diaz covarrubias" = "chalco", "ciudad nezahualcoyotl" = "nezahualcoyotl",
           "ecatepec" = "ecatepec de morelos", "tlalnepantla" = "tlalnepantla de baz",
           "san vicente chicoloapan" = "chicoloapan", "los reyes la paz" = "la paz",
           "coacalco" = "coacalco de berriozabal", "cuautitlan" = "cuautitlan",
           "tamazopo" = "tamasopo", "tancahuitz" = "tancanhuitz", "tampacan" = "tampacan",
           "ebano" = "ebano", "tanquian de escobedo" = "tanquian de escobedo",
           "ozuluama de mazcarenas" = "ozuluama de mascarenas", "ocampo y sierra mojada" = "ocampo",
           "general francisco murguia" = "general francisco r murguia", "parras de la fuente" = "parras",
           "san pedro de las colonias" = "san pedro", "tampamolon" = "tampamolon corona")
LISTAS <- list(
  list(region = "Baja California", ent = "26", m = "San Luis Río Colorado"),
  list(region = "Norte", ent = "32", m = c("Chalchihuites", "Jiménez del Teúl", "Sombrerete", "Sain Alto", "Jerez", "Juan Aldama",
                                           "Río Grande", "General Francisco Murguía", "Mazapil", "Melchor Ocampo")),
  list(region = "Norte", ent = "05", m = c("Torreón", "San Pedro de las Colonias", "Matamoros", "Viesca", "Parras de la Fuente",
                                           "Francisco I. Madero", "Ocampo y Sierra Mojada", "Sierra Mojada")),
  list(region = "Noreste", ent = "32", m = c("Concepción del Oro", "El Salvador")),
  list(region = "Noreste", ent = "24", m = c("Vanegas", "Cedral", "Cerritos", "Guadalcázar", "Ciudad Fernández", "Rioverde",
                                             "San Ciro de Acosta", "Lagunillas", "Santa Catarina", "Rayón", "Cárdenas", "Alaquines",
                                             "Ciudad del Maíz", "Ciudad Valles", "Tamazopo", "Aquismón", "Axtla de Terrazas",
                                             "Tamazunchale", "Huehuetlán", "Tamuín", "Tancahuitz", "Tanlajas", "San Antonio",
                                             "Coxcatlán", "Tampamolón", "San Vicente Tancuayalab", "Ebano", "Xilitla", "Tampacán",
                                             "Tanquián de Escobedo")),
  list(region = "Noreste", ent = "30", m = c("Pánuco", "Tempoal", "Pueblo Viejo", "Tampico Alto", "Ozuluama de Mazcareñas",
                                             "El Higo", "Huayacocotla")),
  list(region = "Central", ent = "15", m = c("Tultepec", "Tultitlán", "Ixtapaluca", "Chalco de Díaz Covarrubias", "Huixquilucan",
                                             "San Mateo Atenco", "Toluca", "Tepotzotlán", "Cuautitlán", "Coacalco",
                                             "Cuautitlán Izcalli", "Atizapán de Zaragoza", "Tlalnepantla", "Naucalpan de Juárez",
                                             "Ecatepec", "Chimalhuacán", "San Vicente Chicoloapan", "Texcoco",
                                             "Ciudad Nezahualcóyotl", "Los Reyes La Paz")),
  list(region = "Central", ent = "17", m = "Cuernavaca"))
# The Acuerdo writes "Ocampo y Sierra Mojada" as two municipalities of Coahuila joined by
# "y" in a list; "Huixquilucan de Degollado" is INEGI's "Huixquilucan".
asig <- map(LISTAS, \(l) {
  k <- norm(l$m); k <- ifelse(k %in% names(ALIAS), ALIAS[k], k)
  k <- unique(k)
  hit <- pol |> filter(cve_ent == l$ent, mn %in% k)
  falta <- setdiff(k, hit$mn)
  if (length(falta)) stop("not matched in state ", l$ent, ": ", paste(falta, collapse = ", "))
  tibble(cvegeo = hit$cvegeo, region_dac = l$region)
}) |> bind_rows()
stopifnot(!anyDuplicated(asig$cvegeo))
ESTADO <- c("02" = "Baja California", "03" = "Baja California Sur", "26" = "Noroeste", "25" = "Noroeste",
            "08" = "Norte", "10" = "Norte", "19" = "Noreste", "28" = "Noreste", "05" = "Noreste", "09" = "Central",
            "18" = "Sur", "14" = "Sur", "06" = "Sur", "16" = "Sur", "01" = "Sur", "11" = "Sur", "22" = "Sur", "13" = "Sur",
            "12" = "Sur", "29" = "Sur", "21" = "Sur", "20" = "Sur", "07" = "Sur", "27" = "Sur", "32" = "Sur", "24" = "Sur",
            "30" = "Sur", "15" = "Sur", "17" = "Sur", "31" = "Peninsular", "04" = "Peninsular", "23" = "Peninsular")
stopifnot(length(ESTADO) == 32, setequal(names(ESTADO), names(ENT)))
mrd <- pol |> select(cvegeo, cve_ent, municipio = all_of(nm_col)) |>
  left_join(asig, by = "cvegeo") |>
  mutate(region_dac = coalesce(region_dac, ESTADO[cve_ent]),
         region_precio = recode(region_dac, Norte = "Norte y Noreste", Noreste = "Norte y Noreste",
                                Sur = "Sur y Peninsular", Peninsular = "Sur y Peninsular"))
stopifnot(!anyNA(mrd$region_dac))
print(count(mrd, region_dac))
write_csv(mrd, file.path(OUT, "mun_region_dac.csv"))

# the step into DAC for every tariff -------------------------------------------------------
# A home at its tariff's DAC limit (dac_limites.csv): its bill on the tariff and the bill
# for the same kWh on DAC, every month. Seasonal tariffs get a summer and a rest-of-year
# bill (which months are summer depends on the locality). DAC is priced in the price region
# that held most of that tariff's household customers in 2017; where that region has
# seasons (Baja California, Baja California Sur, Noroeste until 15 August 2005) each
# tariff season is set against the DAC season of the same name.
lim <- read_csv("data/derived/dac_limites.csv", show_col_types = FALSE)
reg_t <- cfe |> filter(year == 2017, tarifa %in% c("01", "1A", "1B", "1C", "1D", "1E", "1F")) |>
  mutate(tarifa = if_else(tarifa == "01", "1", tarifa)) |>
  inner_join(mrd |> select(cvegeo, region_precio), by = "cvegeo") |>
  group_by(tarifa, region_precio) |> summarise(clientes = sum(users), .groups = "drop_last") |>
  mutate(pct = 100 * clientes / sum(clientes)) |> slice_max(clientes, n = 1) |> ungroup()
print(reg_t)
stopifnot(nrow(reg_t) == 7)
dac_mes <- function(reg, temp, kwh) {
  own <- factura_dac(reg, temp, kwh)
  all <- factura_dac(reg, "todo el año", kwh)
  bind_rows(own, anti_join(all, own, by = c("anio", "mes")))
}
esc <- pmap(lim, \(tarifa, limite_dac_kwh_mes) {
  r <- reg_t$region_precio[reg_t$tarifa == tarifa]; k <- limite_dac_kwh_mes
  temps <- if (tarifa == "1") "todo el año" else c("verano", "fuera de verano")
  map(temps, \(s) bind_rows(
    factura_hogar(tarifa, s, k) |> mutate(serie = "propia"),
    dac_mes(r, if (s == "todo el año") "todo el año" else s, k) |> mutate(serie = "DAC")) |>
      mutate(temporada = s)) |> bind_rows() |>
    mutate(tarifa = tarifa, kwh = k, region_dac = r)
}) |> bind_rows() |>
  left_join(select(inpc, anio, mes, a_pesos_ult), by = c("anio", "mes")) |>
  mutate(factura_real = factura * a_pesos_ult) |> select(-a_pesos_ult) |>
  arrange(tarifa, temporada, serie, anio, mes)
stopifnot(!anyDuplicated(esc[c("tarifa", "temporada", "serie", "anio", "mes")]))
# tariff 1 at 250 kWh must equal the Central case above wherever the regions agree
chk1 <- esc |> filter(tarifa == "1", serie == "propia") |> inner_join(fac |> filter(caso == "t1_250"), by = c("anio", "mes"))
stopifnot(nrow(chk1) > 250, isTRUE(all.equal(chk1$factura.x, chk1$factura.y)))
write_csv(esc, file.path(OUT, "escalon_dac.csv"))
print(esc |> filter(anio == 2026, mes == 9) |> select(tarifa, temporada, serie, kwh, region_dac, factura) |>
        pivot_wider(names_from = serie, values_from = factura) |> mutate(veces = DAC / propia))
