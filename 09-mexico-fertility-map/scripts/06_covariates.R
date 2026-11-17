#### 06_covariates.R
#### Fertility map post, figure 2: municipal observables to set against
#### children per woman.
####   - women's schooling: average grades passed, women 15+ (Intercensal 2025, GRAPROES_F)
####   - indigenous: % of people 3+ who speak an indigenous language (Intercensal 2025,
####     PCN_P3YM_HLI); self-identification (PCN_POB_IND) kept beside it
####   - poverty: % of people in poverty, CONEVAL municipal measurement 2020
####   - rurality: % of people in localities under 2,500, Censo 2020 locality file
#### The 2020 sources predate the newest municipalities: those get NA, counted below.
#### Output: data/derived/covariates.rds

library(tidyverse)
set.seed(20261116)

# Intercensal values (estimator "Valor"), by name
d <- read_csv(unz(Sys.getenv("EIC_ZIP"), "conjunto_de_datos/conjunto_datos_eic2025_105.csv"),
              col_types = cols(.default = "c"), locale = locale(encoding = "latin1"))
num <- function(x) suppressWarnings(as.numeric(str_remove_all(x, "\\*")))
eic_cov <- d %>%
  filter(CVE_ENT != "00", !CVE_MUN %in% c("000", "997"), CVE_LOC == "0000",
         ESTIMADOR == "Valor") %>%
  transmute(cve_mun = paste0(CVE_ENT, CVE_MUN),
            school_f = num(GRAPROES_F),
            hli = num(PCN_P3YM_HLI),
            ind_self = num(PCN_POB_IND))
stopifnot(nrow(eic_cov) == 2478, !anyDuplicated(eic_cov$cve_mun))

# CONEVAL 2020: share in poverty (0-1 in the file)
pov <- read_csv(file.path(Sys.getenv("CONEVAL_DIR"), "Mun_poverty/pobreza_calibrada_municipal_2020.csv"),
                col_types = cols(.default = "c")) %>%
  transmute(cve_mun = str_pad(cve_mun, 5, pad = "0"), poverty = 100 * as.numeric(pobreza))
stopifnot(!anyDuplicated(pov$cve_mun), all(between(pov$poverty, 0, 100), na.rm = TRUE))

# Censo 2020 localities: rural = localities under 2,500 people. Locality rows
# only (LOC not 0000/9998/9999, which are totals and small-locality groups).
loc <- read_csv(Sys.getenv("CENSO20_CSV"),
                col_select = c(ENTIDAD, MUN, LOC, POBTOT), col_types = cols(.default = "c")) %>%
  mutate(pob = as.numeric(POBTOT))
cat("LOC codes that are not single localities:\n")
print(loc %>% filter(LOC %in% c("0000", "9998", "9999")) %>% count(LOC))
rur <- loc %>%
  filter(ENTIDAD != "00", MUN != "000", !LOC %in% c("0000", "9998", "9999")) %>%
  group_by(cve_mun = paste0(ENTIDAD, MUN)) %>%
  summarise(pob20 = sum(pob, na.rm = TRUE),
            rural = 100 * sum(pob[pob < 2500], na.rm = TRUE) / pob20, .groups = "drop")
# check the locality sum against the municipal total rows
mt <- loc %>% filter(ENTIDAD != "00", MUN != "000", LOC == "0000") %>%
  transmute(cve_mun = paste0(ENTIDAD, MUN), pob_tot = pob)
chk <- inner_join(rur, mt, by = "cve_mun")
cat("Locality sum / municipal total:\n"); print(summary(chk$pob20 / chk$pob_tot))
cat("National rural share 2020:", round(100 * sum(rur$rural / 100 * rur$pob20) / sum(rur$pob20), 1), "\n")

cov <- eic_cov %>% left_join(pov, by = "cve_mun") %>%
  left_join(rur %>% select(cve_mun, rural), by = "cve_mun")
cat("Missing values:\n"); print(colSums(is.na(cov)))
cat("Municipalities without a 2020 value:\n")
print(cov %>% filter(is.na(poverty) | is.na(rural)) %>% pull(cve_mun))

saveRDS(cov, "data/derived/covariates.rds")
