#### 03_diagnostic.R
#### Fertility map post: why do municipal fertility rates from SINAC birth
#### records and from the Intercensal 2025 disagree? Tests:
####   1 levels and correlation (reproduce the logged 1.23, 1.32, 0.62)
####   2 noise: gap in Intercensal standard errors, censused vs sampled
####   3 aggregation: municipalities -> metro zones (CONAPO 2010) and states
####   4 importers: gap against births delivered here / births to residents
#### SINAC rate = sum over 15-49 of 5 * births(age group) / women(age group),
#### women from the Intercensal (October 2025). Births by occurrence in 2024,
#### the year the Intercensal question refers to.
#### Output: data/derived/diagnostic.rds, printed tables

library(tidyverse)
library(sf)
set.seed(20261116)

eic <- readRDS("data/derived/eic_mun.rds")
sb  <- readRDS("data/derived/sinac_births.rds")
zm  <- st_read(file.path(Sys.getenv("SHP_DIR"), "04_cities/ZM_2010.shp"), quiet = TRUE) %>%
  st_drop_geometry() %>% transmute(cve_mun = CVE_MUN1, zm = CVE_SUN) %>% distinct()
stopifnot(!anyDuplicated(zm$cve_mun))

women <- eic %>% select(cve_mun, cve_ent, starts_with("w")) %>%
  pivot_longer(starts_with("w"), names_to = "agegrp", values_to = "women") %>%
  mutate(agegrp = str_remove(agegrp, "^w"))

# Keys: SINAC municipalities absent from the Intercensal frame and back
k24 <- sb$by_res %>% filter(yr == 2024) %>% group_by(cve_mun) %>% summarise(b = sum(births))
cat("SINAC residence codes not in the Intercensal:",
    sum(!k24$cve_mun %in% eic$cve_mun), "codes,",
    sum(k24$b[!k24$cve_mun %in% eic$cve_mun]), "births\n")
print(k24 %>% filter(!cve_mun %in% eic$cve_mun) %>% arrange(-b) %>% head(8))
cat("Intercensal municipalities with no SINAC birth:",
    sum(!eic$cve_mun %in% k24$cve_mun), "\n")

# Rate for any grouping of municipalities
tfr <- function(births, grp) {
  w <- women %>% left_join(grp, by = "cve_mun") %>% filter(!is.na(g)) %>%
    group_by(g, agegrp) %>% summarise(women = sum(women), .groups = "drop")
  bb <- births %>% left_join(grp, by = "cve_mun") %>% filter(!is.na(g)) %>%
    group_by(g, agegrp) %>% summarise(births = sum(births), .groups = "drop")
  w %>% left_join(bb, by = c("g", "agegrp")) %>%
    mutate(births = replace_na(births, 0)) %>%
    group_by(g) %>% summarise(tfr = sum(5 * births / women), births = sum(births),
                              women = sum(women), .groups = "drop")
}
eic_grp <- function(grp) {  # Intercensal rate of a group, weighted by women 15-49
  eic %>% left_join(grp, by = "cve_mun") %>% filter(!is.na(g), !is.na(tgf)) %>%
    group_by(g) %>% summarise(tgf = weighted.mean(tgf, f15_49), pob = sum(pob), .groups = "drop")
}

r24 <- sb$by_res %>% filter(yr == 2024)
d24 <- sb$by_del %>% filter(yr == 2024)
r25 <- sb$by_res %>% filter(yr == 2025)

g_mun <- eic %>% transmute(cve_mun, g = cve_mun)
g_nat <- eic %>% transmute(cve_mun, g = "MX")

cat("\n== 1 Levels\n")
cat("Intercensal national row 1.23; women-weighted mean of municipalities:",
    round(eic_grp(g_nat)$tgf, 3), "\n")
cat("SINAC by residence 2024:", round(tfr(r24, g_nat)$tfr, 3),
    " 2025:", round(tfr(r25, g_nat)$tfr, 3),
    " mean 2024-25:", round((tfr(r24, g_nat)$tfr + tfr(r25, g_nat)$tfr) / 2, 3), "\n")

m <- eic %>% select(cve_mun, nom_ent, nom_mun, tgf, tgf_se, censused, pob) %>%
  left_join(tfr(r24, g_mun) %>% select(cve_mun = g, s_res = tfr, b_res = births), by = "cve_mun") %>%
  left_join(tfr(d24, g_mun) %>% select(cve_mun = g, s_del = tfr, b_del = births), by = "cve_mun") %>%
  filter(!is.na(tgf)) %>%
  # censused municipalities print SE 0, and a few tiny ones print TGF 0
  mutate(ratio = if_else(tgf > 0, s_res / tgf, NA_real_),
         z = if_else(tgf_se > 0, (s_res - tgf) / tgf_se, NA_real_),
         importer = b_del / pmax(b_res, 1))

big <- m %>% filter(pob >= 50000)
cat("Correlation, all municipalities: residence", round(cor(m$tgf, m$s_res), 3),
    " delivery", round(cor(m$tgf, m$s_del), 3), "\n")
cat("Correlation, 50,000+ people: residence", round(cor(big$tgf, big$s_res), 3),
    " delivery", round(cor(big$tgf, big$s_del), 3), "\n")

cat("\n== 2 Noise\n")
cat("Share of municipalities with |gap| within 2 Intercensal SE:",
    round(mean(abs(m$z) <= 2, na.rm = TRUE), 3), "(sampled only; censused SE is 0)\n")
print(m %>% group_by(censused) %>%
        summarise(n = n(), cor = cor(tgf, s_res), within2se = mean(abs(z) <= 2, na.rm = TRUE),
                  median_ratio = median(ratio, na.rm = TRUE)))

cat("\n== 3 Aggregation\n")
g_zm <- eic %>% select(cve_mun) %>% inner_join(zm, by = "cve_mun") %>% rename(g = zm)
g_ent <- eic %>% transmute(cve_mun, g = cve_ent)
agg <- function(grp, lab) {
  x <- eic_grp(grp) %>% left_join(tfr(r24, grp) %>% select(g, s_res = tfr), by = "g")
  x <- x %>% mutate(ratio = if_else(tgf > 0, s_res / tgf, NA_real_))
  cat(lab, ": n", nrow(x), " cor", round(cor(x$tgf, x$s_res, use = "complete.obs"), 3),
      " median ratio", round(median(x$ratio, na.rm = TRUE), 3),
      " IQR of ratio", round(IQR(x$ratio, na.rm = TRUE), 3), "\n")
  x
}
x_mun <- agg(g_mun, "Municipalities")
x_zm <- agg(g_zm, "Metro zones 2010")
x_ent <- agg(g_ent, "States")
cat("Valle de México metro zone (13):\n"); print(x_zm %>% filter(g == "13"))
cat("CDMX boroughs:\n")
print(m %>% filter(str_sub(cve_mun, 1, 2) == "09") %>%
        transmute(nom_mun, tgf, s_res = round(s_res, 2), s_del = round(s_del, 2),
                  ratio = round(ratio, 2), importer = round(importer, 2)) %>% arrange(-ratio))

cat("\n== 4 Importers\n")
cat("Spearman of log ratio with log importer index, 50,000+:",
    round(cor(log(big$ratio), log(big$importer), method = "spearman"), 3), "\n")
print(big %>% mutate(imp_q = ntile(importer, 5)) %>% group_by(imp_q) %>%
        summarise(n = n(), importer = round(median(importer), 2),
                  median_ratio = round(median(ratio), 3)))
cat("Biggest disagreements among 50,000+ (by ratio):\n")
print(big %>% arrange(-abs(log(ratio))) %>% head(15) %>%
        transmute(nom_ent, nom_mun, pob, tgf, s_res = round(s_res, 2),
                  ratio = round(ratio, 2), z = round(z, 1), importer = round(importer, 2)))

saveRDS(list(mun = m, zm = x_zm, ent = x_ent), "data/derived/diagnostic.rds")
