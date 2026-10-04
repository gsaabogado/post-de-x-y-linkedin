# ============================================
# Fiscal Profiles by Age — Mexico (2024)
# Source: ENIGH 2024 (Nueva Serie) microdata
# Methodology: NTA-style fiscal incidence
# Builds the smoothed age profiles and saves them to
# data/derived/age_profiles_2024.rds, in 2024 pesos. The figure is drawn by
# 02_fig_profiles.R.
#
# Run from the post root: Rscript scripts/01_build_profiles.R
# The household files live in the mto project; their folder comes from
# ENIGH_HH in the post's .Renviron.
# ============================================

library(tidyverse)

set.seed(42)

source("scripts/_targets.R")

# --- Paths ---
data_dir  <- "data"
enigh_hh  <- path.expand(Sys.getenv("ENIGH_HH"))
stopifnot(nzchar(enigh_hh), dir.exists(enigh_hh))

# --- Load data ---
# Individual-level: age, sex, education, social security
pob <- read_csv(file.path(data_dir, "poblacion.csv"), show_col_types = FALSE) |>
  select(folioviv, foliohog, numren, edad, sexo, parentesco,
         asis_esc, nivel, tipoesc, segsoc, inscr_1,
         inst_1, inst_2, inst_3, inst_4, inst_5, inst_6, inst_7, inst_9,
         servmed_1, servmed_2, servmed_3, servmed_4, servmed_5,
         factor)

# Individual-level: income by source
ing <- read_csv(file.path(data_dir, "ingresos.csv"), show_col_types = FALSE) |>
  select(folioviv, foliohog, numren, clave, ing_tri, factor)

# Household-level: expenditure (for consumption tax imputation)
conc <- read_csv(file.path(enigh_hh, "concentradohogar.csv"), show_col_types = FALSE) |>
  select(folioviv, foliohog, factor, tot_integ, edad_jefe,
         gasto_mon, alimentos, salud, educacion, educa_espa,
         transporte, vivienda, alquiler, vesti_calz, limpieza, enseres,
         personales, comunicacion = comunica, energia,
         transf_gas, bene_gob, jubilacion)

# Household-level: detailed expenditure (for IEPS imputation)
gasto_hh <- read_csv(file.path(enigh_hh, "gastoshogar.csv"), show_col_types = FALSE) |>
  select(folioviv, foliohog, clave, gasto_tri, factor)

# ============================================
# INCOME CLASSIFICATION (P-codes)
# ============================================
# Labels from INEGI's data dictionary for ENIGH 2024, file ingresos, variable
# clave (inegi.org.mx/rnm/index.php/catalog/1116/variable/F25/V1904), read on
# 2026-10-02. Never edit these lists from memory; see notes.md.
#
# LABOUR INCOME (subject to ISR/payroll):
#   P001-P007 = Sueldos, destajo, comisiones, horas extras, incentivos, bonos,
#               primas vacacionales (trabajo principal subordinado)
#   P008, P009 = Reparto de utilidades y aguinaldo del trabajo principal
#   P011, P013 = Sueldos y otros ingresos del trabajo principal (cooperativas,
#                sociedades y empresas propias)
#   P014-P016, P018 = Trabajo secundario: monto, utilidades, aguinaldo, sueldos
#   P021, P022 = Ingresos de otros trabajos (mes pasado y cinco meses anteriores)
#
# SELF-EMPLOYMENT / BUSINESS INCOME:
#   P012, P019 = Ganancias/utilidades del trabajo principal y secundario
#   P068-P074 = Negocios propios del trabajo principal (industrial, comercial,
#               servicios, agrícola, cría de animales, recolección, pesca)
#   P075-P081 = The same seven activities, trabajo secundario
#
# PROPERTY/CAPITAL INCOME:
#   P023-P025 = Alquiler de tierras, terrenos, casas, edificios y locales
#   P026-P029 = Intereses y rendimientos de bonos o cédulas
#   P030, P031 = Marcas, patentes, derechos de autor; otros ingresos por renta
#
# TRANSFERS — PENSIONS:
#   P032 = Jubilaciones y/o pensiones originadas dentro del país
#
# TRANSFERS — GOVERNMENT PROGRAMMES:
#   P038 = Becas provenientes del gobierno
#   P043 = PROCAMPO/ProAgro Productivo/Producción para el Bienestar
#   P045 = Otros programas para adultos mayores
#   P048 = Otros programas sociales
#   P101-P103 = Becas Benito Juárez (básica, media superior), Jóvenes
#               Escribiendo el Futuro
#   P104 = Pensión para el Bienestar de las Personas Adultas Mayores
#   P105 = Pensión para el Bienestar de las Personas con Discapacidad
#   P106 = Niñas y Niños, Hijos de Madres Trabajadoras
#   P107 = Seguro de Vida para Jefas de Familia
#   P108 = Jóvenes Construyendo el Futuro
#
# TRANSFERS — OTHER (private or from abroad):
#   P033 = Jubilaciones y/o pensiones de otros países
#   P034-P036 = Indemnizaciones (seguros, accidentes de trabajo, despido)
#   P037 = Becas de instituciones privadas u ONG
#   P039 = Donativos de ONG
#   P040 = Donativos en dinero de otros hogares
#   P041 = Ingresos provenientes de otros países
#
# Everything else (P049-P067: financial and capital receipts, child work) is
# "other" and is neither taxed nor counted as a benefit.

labour_codes <- c(paste0("P00", 1:9), "P011", "P013", "P014", "P015", "P016",
                  "P018", "P021", "P022")
business_codes <- c("P012", "P019", paste0("P0", 68:81))
capital_codes  <- paste0("P0", 23:31)
pension_codes  <- "P032"
gov_transfer_codes <- c("P038", "P043", "P045", "P048", paste0("P10", 1:8))
other_transfer_codes <- c("P033", "P034", "P035", "P036", "P037", "P039",
                          "P040", "P041")

# ============================================
# 1. BUILD INDIVIDUAL INCOME BY AGE
# ============================================

# --- Formality flag from poblacion ---
# Formal = insured at IMSS, ISSSTE, ISSSTE estatal or PEMEX (inst_1..inst_4)
# as a benefit of the person's own job (inscr_1, "Afiliado por prestación en
# el trabajo"). These are the workers whose cuotas the payroll target counts.
# Not pres_2 (aguinaldo), not segsoc (ever contributed), not pres_8 (SAR/AFORE,
# 6 million short of this flag). See notes.md, decision 1.
pob <- pob |>
  mutate(is_formal = as.integer(!is.na(inscr_1) &
                                  (!is.na(inst_1) | !is.na(inst_2) |
                                     !is.na(inst_3) | !is.na(inst_4))))

# Classify each income record
ing_classified <- ing |>
  mutate(
    income_type = case_when(
      clave %in% labour_codes   ~ "labour",
      clave %in% business_codes ~ "business",
      clave %in% capital_codes  ~ "capital",
      clave %in% pension_codes  ~ "pensions",
      clave %in% gov_transfer_codes ~ "gov_transfers",
      clave %in% other_transfer_codes ~ "other_transfers",
      TRUE ~ "other"
    )
  )

# Aggregate income by person and type
ing_person <- ing_classified |>
  group_by(folioviv, foliohog, numren, income_type) |>
  summarise(ing_tri = sum(ing_tri, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = income_type, values_from = ing_tri, values_fill = 0)

# Merge with population to get age + formality
income_types <- c("labour", "business", "capital", "pensions",
                  "gov_transfers", "other_transfers", "other")
stopifnot(all(income_types %in% names(ing_person)))
ind <- pob |>
  left_join(ing_person, by = c("folioviv", "foliohog", "numren")) |>
  mutate(across(all_of(income_types), ~replace_na(.x, 0)))
stopifnot(nrow(ind) == nrow(pob), !anyNA(ind$is_formal))

cat("Formality rate:",
    round(weighted.mean(ind$is_formal[ind$labour > 0], ind$factor[ind$labour > 0]) * 100, 1),
    "% of people with labour income;",
    round(sum(ind$factor * ind$is_formal) / 1e6, 2), "million formal\n")
cat("ENIGH annual totals by income class, bn pesos:\n")
print(round(sapply(ind[income_types], function(x) sum(x * ind$factor) * 4 / 1e9), 1))

# Cap age at 85+
ind <- ind |>
  mutate(age = pmin(edad, 85))

# ============================================
# 2. IMPUTE TAXES (REVENUE SIDE)
# ============================================

# --- ISR (Income Tax) ---
# Mexico's 2024 ISR brackets (annual, approximate from monthly tables)
# We have quarterly income, so annualize: annual = ing_tri * 4
# Apply simplified progressive rates

impute_isr <- function(annual_income) {
  # 2024 ISR brackets (simplified)
  brackets <- c(0, 8952.49, 75984.55, 133536.07, 155229.80,
                 185852.57, 374837.88, 590795.99, 1127926.84,
                 1503902.46, 4511707.37, Inf)
  rates    <- c(0.0192, 0.0640, 0.1088, 0.16, 0.1792,
                 0.2136, 0.2352, 0.30, 0.32, 0.34, 0.35)
  fixed    <- c(0, 171.88, 4461.94, 10723.55, 14194.54,
                 19682.13, 60049.40, 110842.74, 271981.99,
                 392294.17, 1414947.85)

  tax <- 0
  for (i in seq_along(rates)) {
    if (annual_income > brackets[i]) {
      taxable_in_bracket <- min(annual_income, brackets[i + 1]) - brackets[i]
      if (i == 1) {
        tax <- taxable_in_bracket * rates[i]
      }
    }
  }

  # Simpler approach: use effective rate lookup
  for (i in length(rates):1) {
    if (annual_income > brackets[i]) {
      tax <- fixed[i] + (annual_income - brackets[i]) * rates[i]
      break
    }
  }
  return(tax)
}

ind <- ind |>
  mutate(
    annual_labour = labour * 4,
    annual_business = business * 4,
    # ISR on labour income — ONLY formal workers pay withholding
    isr_labour = if_else(is_formal == 1L,
                         map_dbl(annual_labour, impute_isr),
                         0),
    # ISR on business income — keep for all (businesses file taxes even if informal,
    # though compliance is low; the SHCP scaling will correct aggregate)
    isr_business = map_dbl(annual_business, impute_isr),
    # Total income tax (quarterly)
    income_tax = (isr_labour + isr_business) / 4,
    # Payroll / IMSS — ONLY formal workers
    payroll_tax = if_else(is_formal == 1L, labour * 0.12, 0)
  )

# --- Scale taxes to match SHCP 2024 aggregates ---
# ENIGH underreports income, so our imputed taxes are too low. Each tax is
# scaled to its official 2024 total in scripts/_targets.R (sources there).
# ISR is the part individuals pay; corporate ISR (personas morales) is out.

isr_imputed_annual <- sum(ind$income_tax * ind$factor, na.rm = TRUE) * 4
isr_target <- TARGETS$isr_individual  # individuals only, no corporate ISR
isr_scale <- isr_target / isr_imputed_annual
cat("ISR scale factor:", round(isr_scale, 2), "\n")

payroll_imputed_annual <- sum(ind$payroll_tax * ind$factor, na.rm = TRUE) * 4
payroll_target <- TARGETS$payroll  # IMSS + ISSSTE cuotas
payroll_scale <- payroll_target / payroll_imputed_annual
cat("Payroll scale factor:", round(payroll_scale, 2), "\n")

ind <- ind |>
  mutate(
    income_tax  = income_tax * isr_scale,
    payroll_tax = payroll_tax * payroll_scale
  )

# --- IVA (Consumption Tax) ---
# IVA is household-level. Allocate to individuals using OECD equivalence scales.
# IVA rate: 16% on most goods, 0% on food and medicine
# OECD modified scale: 1.0 first adult, 0.5 additional adults (14+), 0.3 children (<14)

# Compute equivalence weight for each individual
ind <- ind |>
  group_by(folioviv, foliohog) |>
  mutate(
    eq_weight = if_else(edad >= 14, 0.5, 0.3),
    # First adult (household head, parentesco == "101") gets 1.0
    eq_weight = if_else(parentesco == "101", 1.0, eq_weight),
    hh_eq_total = sum(eq_weight)
  ) |>
  ungroup()

conc_iva <- conc |>
  mutate(
    # Expenditure subject to IVA (exclude food, medicine, education, rent)
    iva_base = pmax(0, gasto_mon - alimentos - salud - educacion -
                      alquiler - energia),
    iva_paid = iva_base * 16 / 116
  ) |>
  select(folioviv, foliohog, iva_paid)

ind <- ind |>
  left_join(conc_iva, by = c("folioviv", "foliohog")) |>
  mutate(
    # Each person's share proportional to their equivalence weight
    consumption_tax = replace_na(iva_paid * eq_weight / hh_eq_total, 0)
  )

# Scale IVA to match SHCP
iva_imputed_annual <- sum(ind$consumption_tax * ind$factor, na.rm = TRUE) * 4
iva_target <- TARGETS$iva
iva_scale <- iva_target / iva_imputed_annual
cat("IVA scale factor:", round(iva_scale, 2), "\n")

ind <- ind |>
  mutate(consumption_tax = consumption_tax * iva_scale)

# --- IEPS (Excise Taxes) ---
# Fuel, alcohol, tobacco, sugary drinks — from detailed household expenditure
# IEPS is extracted from spending (included in price), like IVA
# Rates: fuel ~quota-based (~45% effective), alcohol 26.5-53%, tobacco ~160%+quota,
#         sugary drinks $1.17/liter (~10% effective)

ieps_hh <- gasto_hh |>
  mutate(
    ieps_category = case_when(
      substr(clave, 1, 4) == "0722" ~ "fuel",         # Gasolina, diesel
      substr(clave, 1, 3) == "021"  ~ "alcohol",       # Bebidas alcohólicas
      substr(clave, 1, 3) == "023"  ~ "tobacco",       # Tabaco
      # Refrescos de cola (012601), de otros sabores (012602), jugos con gas
      # (012604). Diet sodas (012603) carry no added sugar and stay out.
      # Catalogue F22/V1850; the earlier list (011221...) was beef and pork.
      clave %in% c("012601", "012602", "012604") ~ "sugary_drinks",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(ieps_category)) |>
  # Apply effective IEPS rate by category (extract from gross spending)
  mutate(
    ieps_paid = case_when(
      ieps_category == "fuel"          ~ gasto_tri * 0.35,  # ~35% effective rate
      ieps_category == "alcohol"       ~ gasto_tri * 0.30,  # ~30% avg rate
      ieps_category == "tobacco"       ~ gasto_tri * 0.60,  # ~60% effective (high)
      ieps_category == "sugary_drinks" ~ gasto_tri * 0.10,  # ~10% effective
    )
  ) |>
  group_by(folioviv, foliohog) |>
  summarise(ieps_paid = sum(ieps_paid, na.rm = TRUE), .groups = "drop")

ind <- ind |>
  left_join(ieps_hh, by = c("folioviv", "foliohog")) |>
  mutate(
    # Allocate within household using equivalence scales (same as IVA)
    ieps_tax = replace_na(ieps_paid * eq_weight / hh_eq_total, 0)
  )

# Scale IEPS to match SHCP
ieps_imputed_annual <- sum(ind$ieps_tax * ind$factor, na.rm = TRUE) * 4
ieps_target <- TARGETS$ieps  # full IEPS, spread over fuel, alcohol, tobacco, soda
ieps_scale <- ieps_target / ieps_imputed_annual
cat("IEPS scale factor:", round(ieps_scale, 2), "\n")

ind <- ind |>
  mutate(ieps_tax = ieps_tax * ieps_scale)

# ============================================
# 3. IMPUTE PUBLIC SPENDING (BENEFIT SIDE)
# ============================================

# --- Education spending ---
# Shape from SEP: federal spending per public-school student by tipo educativo
# in 2024 (EDU_REL in _targets.R; básica is not split by level). Level from
# total public spending on education (federal + state + municipal,
# TARGETS$education): each student's cost is scaled so the yearly total matches.
# Tipo educativo is assigned by age, as before.
ind <- ind |>
  mutate(
    # Public education benefit: only if attending PUBLIC school
    # tipoesc: 1=public, 2=private, 3=other (treat as public)
    in_public_school = (asis_esc == "1" & tipoesc %in% c("1", "3")),
    edu_benefit = case_when(
      !in_public_school ~ 0,
      edad >= 3  & edad <= 14 ~ EDU_REL[["basica"]] / 4,  # quarterly
      edad >= 15 & edad <= 17 ~ EDU_REL[["media_superior"]] / 4,
      edad >= 18 & edad <= 24 ~ EDU_REL[["superior"]] / 4,
      TRUE ~ 0
    )
  )
stopifnot(!anyNA(ind$edu_benefit))

edu_imputed_annual <- sum(ind$edu_benefit * ind$factor) * 4
edu_scale <- TARGETS$education / edu_imputed_annual
cat("Education scale factor:", round(edu_scale, 2),
    "| yearly cost per public student after scaling:",
    paste(names(EDU_REL), round(EDU_REL * edu_scale), collapse = ", "), "\n")
cat("Public students with a cost, millions:",
    round(sum(ind$factor[ind$edu_benefit > 0]) / 1e6, 2), "\n")

ind <- ind |>
  mutate(edu_benefit = edu_benefit * edu_scale)

# --- Healthcare spending ---
# Each institution's official 2024 health spending (HEALTH in _targets.R) is
# spread over the people the survey reports under it, in proportion to the
# age gradient below. No outside head count is needed.
# inst_1=IMSS, inst_2=ISSSTE, inst_3=ISSSTE estatal, inst_4=PEMEX, Defensa o
# Marina, inst_5=IMSS-Bienestar, inst_6=federal or state health institution,
# inst_7=private insurance, inst_9=no affiliation (dictionary F21).

# Derive data-driven age gradient from actual public health utilization (servmed)
# Then adjust for cost intensity: elderly visits cost more (hospitalizations, chronic care)
ind <- ind |>
  mutate(
    used_public_health = as.integer(!is.na(servmed_1) | !is.na(servmed_2) |
                                     !is.na(servmed_3) | !is.na(servmed_4) |
                                     !is.na(servmed_5))
  )

# Compute utilization rate by age, then smooth
util_by_age <- ind |>
  group_by(age) |>
  summarise(util_rate = weighted.mean(used_public_health, factor, na.rm = TRUE),
            .groups = "drop")
mean_util <- weighted.mean(ind$used_public_health, ind$factor, na.rm = TRUE)

util_by_age <- util_by_age |>
  mutate(
    # Data-driven frequency multiplier (normalized to mean = 1)
    freq_mult = util_rate / mean_util,
    # Cost intensity multiplier: elderly visits are more expensive
    # (hospitalization rates, chronic disease management, surgeries)
    cost_mult = case_when(
      age <= 4  ~ 1.5,   # Neonatal/infant — expensive per visit
      age <= 14 ~ 0.8,   # Children — mostly preventive, cheap
      age <= 44 ~ 1.0,   # Working age — baseline
      age <= 64 ~ 1.3,   # Chronic conditions begin
      age <= 74 ~ 1.6,   # Higher hospitalization
      TRUE      ~ 1.8    # 75+ — most expensive per visit
    ),
    # Combined age multiplier = frequency × cost intensity
    age_mult = freq_mult * cost_mult
  )

ind <- ind |>
  left_join(util_by_age |> select(age, age_mult), by = "age") |>
  mutate(
    # One regime per person, same priority as before. People without social
    # security (IMSS-Bienestar, health services, no affiliation, no answer)
    # share one pool: that money serves them wherever they report.
    health_regime = case_when(
      !is.na(inst_4) ~ "pemex_armed",
      !is.na(inst_2) ~ "issste",
      !is.na(inst_3) ~ "issste_estatal",
      !is.na(inst_1) ~ "imss",
      !is.na(inst_5) | !is.na(inst_6) | !is.na(inst_9) ~ "open",
      !is.na(inst_7) ~ "private",   # private insurance only: no public spending
      TRUE ~ "open"
    )
  )
stopifnot(!anyNA(ind$age_mult), !anyNA(ind$health_regime))

# Pesos per unit of age-weighted person, by regime. ISSSTE estatal has no
# spending figure of its own and takes the ISSSTE rate; private gets zero.
health_rate <- ind |>
  group_by(health_regime) |>
  summarise(people = sum(factor), weighted_people = sum(factor * age_mult),
            .groups = "drop") |>
  mutate(target = unname(HEALTH[health_regime]),
         rate = target / weighted_people)
stopifnot(all(names(HEALTH) %in% health_rate$health_regime))
health_rate$rate[health_rate$health_regime == "issste_estatal"] <-
  health_rate$rate[health_rate$health_regime == "issste"]
health_rate$rate[health_rate$health_regime == "private"] <- 0
stopifnot(!anyNA(health_rate$rate))
cat("Health: yearly pesos per person by regime (average over ages):\n")
print(health_rate |>
        transmute(health_regime, millions = round(people / 1e6, 2),
                  per_person = round(rate * weighted_people / people)))

ind <- ind |>
  left_join(health_rate |> select(health_regime, health_rate = rate),
            by = "health_regime") |>
  mutate(health_benefit = (health_rate * age_mult) / 4)  # quarterly
stopifnot(!anyNA(ind$health_benefit))
cat("Health total, bn:", round(sum(ind$health_benefit * ind$factor) * 4 / 1e9, 1), "\n")

# --- Cash transfers received ---
# Pensions (P032) and gov programmes (P038, P043, P045, P048, P101-P108) are
# each scaled to their official 2024 total in _targets.R. P032 also holds
# private pensions, which the survey cannot separate; they are scaled too.
gov_enigh_annual <- sum(ind$gov_transfers * ind$factor, na.rm = TRUE) * 4
gov_target <- TARGETS$programmes  # nine cash programmes, subsidios
gov_scale <- gov_target / gov_enigh_annual
cat("Gov transfers scale factor:", round(gov_scale, 2), "\n")

pension_enigh_annual <- sum(ind$pensions * ind$factor, na.rm = TRUE) * 4
pension_scale <- TARGETS$pensions / pension_enigh_annual
cat("Pensions scale factor:", round(pension_scale, 2), "\n")

ind <- ind |>
  mutate(gov_transfers = gov_transfers * gov_scale,
         pensions = pensions * pension_scale)

# ============================================
# 4. AGGREGATE BY AGE
# ============================================

age_profiles <- ind |>
  group_by(age) |>
  summarise(
    # Revenue (taxes paid) — weighted
    labour_tax      = weighted.mean(income_tax + payroll_tax, w = factor, na.rm = TRUE),
    capital_tax     = weighted.mean(map_dbl(capital * 4, impute_isr) / 4, w = factor, na.rm = TRUE),
    consumption_tax = weighted.mean(consumption_tax, w = factor, na.rm = TRUE),
    excise_tax      = weighted.mean(ieps_tax, w = factor, na.rm = TRUE),
    # Spending (benefits received) — weighted
    education       = weighted.mean(edu_benefit, w = factor, na.rm = TRUE),
    healthcare      = weighted.mean(health_benefit, w = factor, na.rm = TRUE),
    cash_transfers  = weighted.mean(pensions + gov_transfers, w = factor, na.rm = TRUE),
    # Population weight
    pop             = sum(factor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Annualize (from quarterly)
  mutate(across(c(labour_tax, capital_tax, consumption_tax, excise_tax,
                  education, healthcare, cash_transfers),
                ~ .x * 4))

# ============================================
# 5. SMOOTH PROFILES
# ============================================

# Apply local polynomial smoothing (like NTA methodology)
smooth_var <- function(x, y, span = 0.15) {
  fit <- loess(y ~ x, span = span)
  predict(fit, newdata = data.frame(x = x))
}

age_smooth <- age_profiles |>
  mutate(
    labour_tax      = smooth_var(age, labour_tax),
    capital_tax     = smooth_var(age, capital_tax),
    consumption_tax = smooth_var(age, consumption_tax),
    excise_tax      = smooth_var(age, excise_tax),
    education       = smooth_var(age, education),
    healthcare      = smooth_var(age, healthcare),
    cash_transfers  = smooth_var(age, cash_transfers)
  )

# ============================================
# 6. SAVE
# ============================================

stopifnot(nrow(age_smooth) == 86, all(age_smooth$age == 0:85), !anyNA(age_smooth))

saveRDS(list(raw = age_profiles, smooth = age_smooth),
        "data/derived/age_profiles_2024.rds")
message("DONE!")
