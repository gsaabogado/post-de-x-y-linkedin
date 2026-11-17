#### 01_intercensal.R
#### Fertility map post: municipal TGF and women by five-year age from the
#### Encuesta Intercensal 2025, "Principales resultados por localidad de
#### 50 000 y más habitantes" (file 105, which also carries municipal rows).
#### Output: data/derived/eic_mun.rds (one row per municipality).
#### Run from the post folder: Rscript scripts/01_intercensal.R

library(tidyverse)
set.seed(20261116)

z <- Sys.getenv("EIC_ZIP")
stopifnot(file.exists(z))

d <- read_csv(unz(z, "conjunto_de_datos/conjunto_datos_eic2025_105.csv"),
              col_types = cols(.default = "c"),
              locale = locale(encoding = "latin1"))

mun <- d %>%
  filter(CVE_ENT != "00", !CVE_MUN %in% c("000", "997"), CVE_LOC == "0000")

# Estimator rows, by name
est_key <- c("Valor" = "val", "Error estándar" = "se",
             "Coeficiente de variación" = "cv",
             "Límite inferior de confianza" = "lo",
             "Límite superior de confianza" = "hi")
stopifnot(all(unique(mun$ESTIMADOR) %in% names(est_key)))
mun <- mun %>% mutate(est = unname(est_key[ESTIMADOR]))
stopifnot(all(count(mun, est)$n == 2478))

# Flags: a census-style municipality carries "*" in the value, a sample too
# small for an estimate carries "**" or "MI". Keep the raw string.
age_cols <- paste0("PCN_P_", c("15A19", "20A24", "25A29", "30A34",
                               "35A39", "40A44", "45A49"), "_F")
all_f <- paste0("PCN_P_", c("0A4", "5A9", "10A14", "15A19", "20A24", "25A29",
                            "30A34", "35A39", "40A44", "45A49", "50A54",
                            "55A59", "60A64", "65A69", "70A74", "75YMAS"), "_F")

wide <- mun %>%
  select(CVEGEO, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN, est,
         POBTOT, POBFEM, P_15A49_F, all_of(all_f), TGF) %>%
  pivot_wider(names_from = est,
              values_from = c(POBTOT, POBFEM, P_15A49_F, all_of(all_f), TGF))

num <- function(x) suppressWarnings(as.numeric(str_remove_all(x, "\\*")))

out <- wide %>%
  transmute(
    cve_mun = paste0(CVE_ENT, CVE_MUN), cve_ent = CVE_ENT,
    nom_ent = NOM_ENT,
    # INEGI marks the name: "*" = all homes interviewed, "**" = sample too
    # small for an estimate (value "MI")
    flag = str_extract(NOM_MUN, "\\*+$"),
    nom_mun = str_remove(NOM_MUN, "\\*+$"),
    tgf_raw = TGF_val,
    tgf = num(TGF_val), tgf_se = num(TGF_se), tgf_cv = num(TGF_cv),
    tgf_lo = num(TGF_lo), tgf_hi = num(TGF_hi),
    censused = !is.na(flag) & flag == "*",
    pob = num(POBTOT_val), pobfem = num(POBFEM_val),
    f15_49 = num(P_15A49_F_val),
    across(all_of(paste0(all_f, "_val")), num)
  )

# Women by age: PCN_P_*_F is women of that age per 100 PERSONS
fsum <- rowSums(out[paste0(all_f, "_val")])
cat("Sum of female age shares (should be near women's share of all people):\n")
print(summary(fsum / (100 * out$pobfem / out$pob)))
w <- out %>%
  transmute(cve_mun, across(all_of(paste0(age_cols, "_val")),
                            ~ .x / 100 * pob, .names = "w_{.col}")) %>%
  rename_with(~ str_replace(.x, "^w_PCN_P_(\\d+)A(\\d+)_F_val$", "w\\1_\\2"))
out <- out %>% select(-all_of(paste0(all_f, "_val"))) %>% left_join(w, by = "cve_mun")
rebuilt <- rowSums(select(out, starts_with("w")))
cat("Rebuilt women 15-49 against P_15A49_F (ratio):\n")
print(summary(rebuilt / out$f15_49))

stopifnot(nrow(out) == 2478, n_distinct(out$cve_mun) == 2478)
cat("TGF missing:", sum(is.na(out$tgf)), " raw values:",
    paste(unique(out$tgf_raw[is.na(out$tgf)]), collapse = " | "), "\n")
cat("Censused:", sum(out$censused), " double asterisk:", sum(out$flag == "**", na.rm = TRUE), "\n")
stopifnot(all(is.na(out$tgf) == (out$flag %in% "**")))
cat("CV above 30:", sum(out$tgf_cv > 30, na.rm = TRUE),
    " top decile CV:", quantile(out$tgf_cv, 0.9, na.rm = TRUE), "\n")

nat <- d %>% filter(CVE_ENT == "00", ESTIMADOR == "Valor") %>% pull(TGF)
cat("National TGF row:", nat, "\n")

saveRDS(out, "data/derived/eic_mun.rds")
