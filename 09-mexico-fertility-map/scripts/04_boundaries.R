#### 04_boundaries.R
#### Fertility map post: municipal and state polygons from INEGI's Marco
#### Geoestadístico, Encuesta Intercensal 2025 (upc 794551196649, archived in
#### Data/inegi_marco_geo/raw_2026-11/), joined to the Intercensal TGF.
#### Output: data/derived/mun_2025.rds (sf, simplified for a national map),
####         data/derived/ent_2025.rds

library(tidyverse)
library(sf)
set.seed(20261116)

z <- Sys.getenv("MG_ZIP")
stopifnot(file.exists(z))
tmp <- file.path(tempdir(), "mg2025")
unzip(z, files = paste0("conjunto_de_datos/00", rep(c("mun", "ent"), each = 5),
                        c(".shp", ".shx", ".dbf", ".prj", ".cpg")),
      exdir = tmp, overwrite = TRUE)

mun <- st_read(file.path(tmp, "conjunto_de_datos/00mun.shp"), quiet = TRUE)
ent <- st_read(file.path(tmp, "conjunto_de_datos/00ent.shp"), quiet = TRUE)
cat("CRS:", st_crs(mun)$Name, "\n"); print(names(mun))
stopifnot(nrow(mun) == 2478, !anyDuplicated(mun$CVEGEO), nrow(ent) == 32)

eic <- readRDS("data/derived/eic_mun.rds")
cat("Polygons not in the Intercensal:", sum(!mun$CVEGEO %in% eic$cve_mun),
    " Intercensal rows without a polygon:", sum(!eic$cve_mun %in% mun$CVEGEO), "\n")
stopifnot(setequal(mun$CVEGEO, eic$cve_mun))

# Area from the polygons themselves (projected CRS, square km)
mun <- mun %>% mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)
cat("National land area, km2:", round(sum(mun$area_km2)), "\n")

# Simplify for a national map: tolerance in metres (LCC), topology kept per
# polygon; check that no municipality disappears.
mun_s <- st_simplify(mun, preserveTopology = TRUE, dTolerance = 200)
stopifnot(all(!st_is_empty(mun_s)), nrow(mun_s) == 2478)
ent_s <- st_simplify(ent, preserveTopology = TRUE, dTolerance = 200)

out <- mun_s %>%
  select(cve_mun = CVEGEO, area_km2) %>%
  left_join(eic %>% select(cve_mun, nom_ent, nom_mun, flag, censused, tgf,
                           tgf_se, tgf_cv, tgf_lo, tgf_hi, pob, f15_49),
            by = "cve_mun")
stopifnot(nrow(out) == 2478, sum(is.na(out$tgf)) == 7)
cat("New municipalities present: ",
    paste(out$nom_mun[out$cve_mun %in% c("24059", "25019", "25020")], collapse = ", "), "\n")

saveRDS(out, "data/derived/mun_2025.rds")
saveRDS(ent_s %>% select(cve_ent = CVE_ENT, nom_ent = NOMGEO), "data/derived/ent_2025.rds")
cat("Object size, MB:", round(object.size(out) / 1e6, 1), "\n")
