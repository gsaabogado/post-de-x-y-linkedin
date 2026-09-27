# 01_build_data.R -------------------------------------------------------------
# Solar panels in Mexican homes, Encuesta Intercensal 2025, municipal level,
# pushed onto a 1 km grid for the map (figure F1).
#
# The survey reports a RATE per municipality, the share of inhabited private
# dwellings with a solar panel for electricity (PCN_VPH_PANEL). A rate cannot
# be smoothed directly, so it is split into two extensive quantities first:
#   den = inhabited private dwellings (VIVPARHAB)
#   num = den * rate, dwellings with a panel
# Each is then spread over the municipality's 1 km cells in proportion to
# WorldPop 2025 population (dasymetric), and 02 smooths both and divides.
# Inside a municipality this assumes one rate everywhere; the survey has no
# finer detail.
#
# Seven municipalities created after 2021 have no polygon in the Dec 2021
# Marco Geoestadistico. Each is folded back into the municipality it split
# from, adding numerator and denominator, which reproduces the parent's
# pre-split territory exactly.
#
# Seven municipalities carry "MI" (insufficient sample) instead of a rate.
# They are dropped from numerator AND denominator, so the smoothed surface
# fills them from their neighbours rather than reading them as zero.
#
# Run from the post root: Rscript scripts/01_build_data.R

library(dplyr)
library(readr)
library(sf)
library(terra)

MLPV <- path.expand(Sys.getenv("MLPV_IN"))  # folder with the MLPV inputs; set in .Renviron
if (!nzchar(MLPV)) stop("Set MLPV_IN (the MLPV project's in/ folder) in .Renviron; see README")
EIC_ZIP <- file.path(MLPV, "Census/intercensal2025/raw/conjunto_de_datos_eic2025_105_csv.zip")
MUN_SHP <- file.path(MLPV, "shp/agem/00mun.shp")
NAT_CSV <- file.path(MLPV, "Census/derived/panel_shares_state_national_2015_2020.csv")
POP_TIF <- "../02-mexico-population-latitude/data/mex_pop_2025_CN_1km_R2025A_UA_v1.tif"
OUT <- "data/derived"

# Post-2021 municipality -> the one it was carved out of.
# Guerrero: Congress decree of 2021-08-31, reported by Quadratin and El
# Financiero. San Luis Potosi and Sinaloa are NOT yet checked against a source:
# Villa de Pozos from the SLP capital, Eldorado from Culiacan, Juan Jose Rios
# from Guasave (it may also take part of Ahome).
PARENT <- c("12082" = "12053",   # Las Vigas <- San Marcos
            "12083" = "12012",   # Nuu Savi <- Ayutla de los Libres
            "12084" = "12041",   # Santa Cruz del Rincon <- Malinaltepec
            "12085" = "12023",   # San Nicolas <- Cuajinicuilapa
            "24059" = "24028",   # Villa de Pozos <- San Luis Potosi
            "25019" = "25006",   # Eldorado <- Culiacan
            "25020" = "25011")   # Juan Jose Rios <- Guasave

# --- survey ------------------------------------------------------------------
eic <- read_csv(unz(EIC_ZIP, "conjunto_de_datos/conjunto_datos_eic2025_105.csv"),
                col_types = cols(.default = "c"), locale = locale(encoding = "latin1"))

val <- eic |> filter(ESTIMADOR == "Valor")
se  <- eic |> filter(ESTIMADOR == "Error estándar")
stopifnot(nrow(val) == nrow(se), identical(val$CVEGEO, se$CVEGEO))

nat25 <- tibble(year = 2025L,
                pct = as.numeric(val$PCN_VPH_PANEL[val$CVEGEO == "000000000"]),
                se  = as.numeric(se$PCN_VPH_PANEL[se$CVEGEO == "000000000"]))

mun <- val |>
  filter(CVE_MUN != "000", CVE_LOC == "0000") |>
  transmute(cve = substr(CVEGEO, 1, 5), nom = NOM_MUN,
            den = as.numeric(VIVPARHAB),
            pct = suppressWarnings(as.numeric(PCN_VPH_PANEL)))
stopifnot(nrow(mun) == 2478, !anyNA(mun$den), sum(is.na(mun$pct)) == 7,
          all(val$PCN_VPH_PANEL[is.na(suppressWarnings(as.numeric(val$PCN_VPH_PANEL)))] == "MI"))

# The municipal counts must rebuild the published national rate.
rebuilt <- with(filter(mun, !is.na(pct)), 100 * sum(den * pct / 100) / sum(den))
message(sprintf("national rate rebuilt from municipalities %.3f%% vs published %.2f%%",
                rebuilt, nat25$pct))
stopifnot(abs(rebuilt - nat25$pct) < 0.01)

mun <- mun |>
  filter(!is.na(pct)) |>
  mutate(num = den * pct / 100,
         cve = ifelse(cve %in% names(PARENT), PARENT[cve], cve)) |>
  group_by(cve) |>
  summarise(den = sum(den), num = sum(num), .groups = "drop")

# --- municipal polygons ------------------------------------------------------
shp <- st_read(MUN_SHP, quiet = TRUE) |>
  st_transform(4326) |>
  select(cve = CVEGEO, ent = CVE_ENT)
stopifnot(nrow(shp) == 2471, all(mun$cve %in% shp$cve))
shp <- shp |> left_join(mun, by = "cve") |>
  mutate(den = coalesce(den, 0), num = coalesce(num, 0), id = row_number())

# --- dasymetric allocation onto the 1 km population grid ---------------------
pop <- rast(POP_TIF)
pop[is.na(pop)] <- 0
idr <- rasterize(vect(shp), pop, field = "id", touches = FALSE)

cells <- tibble(cell = seq_len(ncell(pop)), id = values(idr, mat = FALSE),
                pop = values(pop, mat = FALSE)) |>
  filter(!is.na(id))
w <- cells |> group_by(id) |> mutate(w = if (sum(pop) > 0) pop / sum(pop) else 1 / n()) |>
  ungroup()

# Municipalities too small to own a cell centre (dozens in Oaxaca) drop their
# whole mass into the cell under a point inside them.
orphan <- setdiff(shp$id, w$id)
if (length(orphan)) {
  pts <- st_point_on_surface(st_geometry(shp[orphan, ]))
  oc <- cellFromXY(pop, st_coordinates(pts))
  w <- bind_rows(w, tibble(cell = oc, id = orphan, pop = NA_real_, w = 1))
}
message(sprintf("%d municipalities own no cell centre and were placed as a point", length(orphan)))

alloc <- w |> left_join(st_drop_geometry(shp) |> select(id, den, num), by = "id") |>
  group_by(cell) |>
  summarise(den = sum(den * w), num = sum(num * w), .groups = "drop")

den_r <- num_r <- rast(pop)
den_r[] <- 0; num_r[] <- 0
den_r[alloc$cell] <- alloc$den
num_r[alloc$cell] <- alloc$num

# Mass preservation, both quantities, against the municipal table.
chk <- c(den = global(den_r, "sum")[1, 1] / sum(mun$den),
         num = global(num_r, "sum")[1, 1] / sum(mun$num))
message(sprintf("grid / table: dwellings %.5f, with panel %.5f", chk["den"], chk["num"]))
stopifnot(all(abs(chk - 1) < 1e-4))

writeRaster(c(den_r, num_r) |> setNames(c("den", "num")),
            file.path(OUT, "dwellings_panel_1km.tif"), overwrite = TRUE)

# --- outlines ------------------------------------------------------------------
# Planar geometry for the dissolve: s2 rejects a degenerate edge in one of
# INEGI's polygons, and these outlines are only drawn, never measured.
sf_use_s2(FALSE)
# Dissolve and simplify in INEGI's own metric projection (tolerance 800 m),
# then move to lon/lat.
states <- st_read(MUN_SHP, quiet = TRUE) |>
  group_by(ent = CVE_ENT) |> summarise(.groups = "drop") |>
  st_simplify(dTolerance = 800) |>
  st_transform(4326)
country <- st_union(states) |> st_make_valid()
state_lines <- st_boundary(states) |> st_cast("MULTILINESTRING") |> st_cast("LINESTRING")
state_lines <- do.call(rbind, lapply(seq_along(st_geometry(state_lines)), function(i) {
  xy <- st_coordinates(st_geometry(state_lines)[[i]])
  data.frame(x = xy[, 1], y = xy[, 2], grp = i)
}))
saveRDS(country, file.path(OUT, "mex_poly.rds"))
saveRDS(state_lines, file.path(OUT, "state_lines.rds"))

# --- national series for the inset --------------------------------------------
# 2015 and 2020 come from MLPV script 18 (microdata, same definition as INEGI's
# 2025 indicator). When that CSV is a Dropbox online-only placeholder, reading
# it hangs, so the values are then taken from the MLPV Census README, which
# records the same run (2026-09-23), and a warning says so.
if (as.integer(system2("stat", c("-f", "%b", shQuote(path.expand(NAT_CSV))), stdout = TRUE)) > 0) {
  nat_prior <- read_csv(NAT_CSV, show_col_types = FALSE) |>
    filter(cve_ent == "00") |>
    transmute(year = as.integer(year), pct = pct_panel, se = se_pct)
} else {
  warning("national CSV is online-only; using the values recorded in the MLPV README")
  nat_prior <- tibble(year = c(2015L, 2020L), pct = c(0.549, 0.806), se = c(0.007, 0.033))
}
nat <- bind_rows(nat_prior, nat25) |> arrange(year)
stopifnot(identical(nat$year, c(2015L, 2020L, 2025L)), !anyNA(nat))
print(nat)
saveRDS(nat, file.path(OUT, "national_series.rds"))

states25 <- val |>
  filter(CVE_ENT != "00", CVE_MUN == "000") |>
  transmute(nom = NOM_ENT, pct = as.numeric(PCN_VPH_PANEL))
stopifnot(nrow(states25) == 32)
saveRDS(states25, file.path(OUT, "states_2025.rds"))

# --- cities for labels, values read from the survey ---------------------------
loc <- val |>
  filter(CVE_LOC != "0000", !grepl("^99", CVE_LOC)) |>
  transmute(cvegeo = CVEGEO, nom = NOM_LOC, ent = NOM_ENT,
            pop = as.numeric(POBTOT), pct = as.numeric(PCN_VPH_PANEL))
saveRDS(loc, file.path(OUT, "localities_50k.rds"))

message("DONE!")
