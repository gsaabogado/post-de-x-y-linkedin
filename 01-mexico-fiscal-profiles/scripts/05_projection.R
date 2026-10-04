# 05_projection.R -------------------------------------------------------------
# Projects Mexico's population from 2024 to 2075 under two fixed fertility
# rates and carries the 2024 fiscal profiles (F1) onto it.
#
# Ceteris paribus by design: fertility stays at one total fertility rate
# forever, deaths at CONAPO's 2025 rates by single age and sex, no migration,
# and taxes and benefits per person stay at their 2024 values in pesos of
# August 2026. Only the number of people at each age changes.
#
# Base population: the ENIGH 2024 people by single age and sex, the same head
# counts as F2 (scripts/_people.R, smoothed for age heaping), so the first frame
# continues F2. Ages 0-4 are the exception: they come from registered births,
# because the survey undercounts small children (see below). CONAPO's 2023 projection for 1 Jan 2025 has 2.5 million more
# people, with more young adults and fewer people over 60 than INEGI's 2024
# survey; it is used here only for death rates, the shape of fertility by age
# and a check of the one-year step.
#
# Inputs: data/poblacion.csv (ENIGH 2024), and in data/conapo/ (made by
# scripts/04_conapo_extract.js):
#   conapo_2025_national.csv                population on 1 Jan and 1 Jul 2025,
#                                           deaths in 2025, ages 0-109, by sex
#   conapo_fecundidad_national_2024_2025.csv age-specific fertility, 5-year groups
# Output: data/derived/projection_2025_2075.rds
#
# Run from the post root, after 01: Rscript scripts/05_projection.R

library(dplyr)
library(tidyr)

source("scripts/_money.R")
source("scripts/_targets.R")
source("scripts/_children.R")
source("scripts/_people.R")

TFR_SURVEY <- 1.2         # children per woman in 2024, INEGI Intercensal 2025
# The second scenario is the rate the registered births of 2024 imply; it is
# computed below (TFR_REGISTRY) once the base and the births are in.
YEARS <- 2024:2075        # 2024 = the ENIGH base; frames from 2025
MAX_AGE <- 109            # CONAPO's last age; treated as 109 and older
OPEN_AGE <- 85            # last row of the fiscal profiles: 85 and older

# --- CONAPO inputs -------------------------------------------------------------
cp <- read.csv("data/conapo/conapo_2025_national.csv")
stopifnot(nrow(cp) == 2 * (MAX_AGE + 1), !anyNA(cp),
          sum(cp$pob_inicio_2025[cp$sexo == "Hombres"]) == 64983857,
          sum(cp$pob_inicio_2025[cp$sexo == "Mujeres"]) == 67849944)
by_sex <- function(col, sex) {
  d <- cp[cp$sexo == sex, ]
  setNames(d[[col]][order(d$edad)], sort(d$edad))
}
pop0 <- list(m = by_sex("pob_inicio_2025", "Hombres"), f = by_sex("pob_inicio_2025", "Mujeres"))

# Central death rates of 2025 and the share surviving one year at each age.
mx <- list(m = by_sex("defunciones_2025", "Hombres") / by_sex("pob_mitad_2025", "Hombres"),
           f = by_sex("defunciones_2025", "Mujeres") / by_sex("pob_mitad_2025", "Mujeres"))
px <- lapply(mx, function(m) exp(-m))
stopifnot(all(sapply(px, function(p) all(p > 0 & p < 1))))

# Fertility shape: CONAPO's 2025 rates by 5-year group, one rate per single
# age 15-49, rescaled to each scenario's total fertility rate.
fert <- read.csv("data/conapo/conapo_fecundidad_national_2024_2025.csv") |>
  filter(anio == 2025)
stopifnot(nrow(fert) == 7)
lo <- as.integer(sub("-.*", "", fert$gpo_edad))
asfr_shape <- setNames(rep(fert$tasa_por_mil / 1000, each = 5), unlist(lapply(lo, function(a) a:(a + 4))))
TFR_CONAPO_2025 <- sum(asfr_shape)
stopifnot(abs(TFR_CONAPO_2025 - 5 * sum(fert$tasa_por_mil) / 1000) < 1e-9)

# Sex ratio at birth from CONAPO's age-0 population on 1 Jan 2025, corrected
# for the half year of infant deaths that differs by sex.
SRB <- (pop0$m[["0"]] / exp(-mx$m[["0"]] / 2)) / (pop0$f[["0"]] / exp(-mx$f[["0"]] / 2))
share_boys <- SRB / (1 + SRB)

# --- One year forward -----------------------------------------------------------
# Women exposed during the year: the mean of the start and the end of the
# year, by age, so the births of year t use women as they age through it.
step <- function(p, tfr) {
  age <- as.integer(names(p$m))
  older <- function(v, s) {
    out <- c(0, head(v * s, -1))                     # age a -> a + 1
    out[MAX_AGE + 1] <- out[MAX_AGE + 1] + v[MAX_AGE + 1] * s[MAX_AGE + 1]  # 109+ stays
    setNames(out, age)
  }
  m_next <- older(p$m, px$m)
  f_next <- older(p$f, px$f)
  fa <- names(asfr_shape)
  exposure <- (p$f[fa] + f_next[fa]) / 2
  births <- sum(asfr_shape * tfr / TFR_CONAPO_2025 * exposure)
  m_next[["0"]] <- births * share_boys * exp(-mx$m[["0"]] / 2)
  f_next[["0"]] <- births * (1 - share_boys) * exp(-mx$f[["0"]] / 2)
  stopifnot(all(m_next >= 0), all(f_next >= 0), births > 0)
  list(m = m_next, f = f_next, births = births)
}

# Check: one year at CONAPO's own 2025 fertility should land near CONAPO's
# 1 Jan 2026 population (133,901,039; age 0 1,993,001), which also has
# migration in it.
chk <- step(pop0, TFR_CONAPO_2025)
message(sprintf("check, CONAPO fertility %.3f: 2026 total %.0f (CONAPO 133,901,039), age 0 %.0f (CONAPO 1,993,001), births %.0f",
                TFR_CONAPO_2025, sum(chk$m) + sum(chk$f), chk$m[["0"]] + chk$f[["0"]], chk$births))

# --- ENIGH base, by single age and sex -------------------------------------------
# The smoothed head counts of F2 (scripts/_people.R), by sex, ages 0-109.
base <- people_2024(max_age = MAX_AGE, open_age = OPEN_AGE)
stopifnot(length(base$m) == MAX_AGE + 1, length(base$f) == MAX_AGE + 1)

# --- Ages 0-4 from registered births -----------------------------------------------
# The survey undercounts small children, which left a step in the projection
# where the surveyed babies meet the projected ones. Ages 0-4 of the base
# come from registered births (scripts/_children.R, shared with F2).
kids <- children_0_4_2024()
births_of <- kids$births_of
LATE_SHARE <- kids$late_share
before_0_4 <- sum(base$m[as.character(0:4)] + base$f[as.character(0:4)])
base$m[names(kids$m)] <- kids$m
base$f[names(kids$f)] <- kids$f
message(sprintf("late share %.3f; completed births 2020-2024: %s; ages 0-4 in the base: %.2f million (survey %.2f)",
                LATE_SHARE, paste(format(round(births_of[as.character(2020:2024)]), big.mark = ","), collapse = " | "),
                sum(base$m[as.character(0:4)] + base$f[as.character(0:4)]) / 1e6, before_0_4 / 1e6))
message("base, ages 0-7 (thousands): ", paste(round((base$m + base$f)[as.character(0:7)] / 1e3), collapse = " "))

# --- The two scenarios ---------------------------------------------------------------
# Registry rate: the completed births of 2024 over the births the women of the
# base would have per unit of total fertility, with CONAPO's age pattern.
births_per_unit <- sum(asfr_shape / TFR_CONAPO_2025 * base$f[names(asfr_shape)])
TFR_REGISTRY <- round(births_of[["2024"]] / births_per_unit, 2)
stopifnot(TFR_REGISTRY > TFR_SURVEY, TFR_REGISTRY < TFR_CONAPO_2025)
SCENARIOS <- c(low = TFR_SURVEY, high = TFR_REGISTRY)
message(sprintf("scenarios: survey %.2f, registry %.2f (births 2024 %s over %.0f per unit of fertility)",
                TFR_SURVEY, TFR_REGISTRY, format(round(births_of[["2024"]]), big.mark = ","), births_per_unit))

project <- function(tfr) {
  p <- base
  rows <- vector("list", length(YEARS))
  for (i in seq_along(YEARS)) {
    rows[[i]] <- tibble(year = YEARS[i], age = as.integer(names(p$m)),
                        pop = unname(p$m + p$f))
    if (i < length(YEARS)) p <- step(p, tfr)
  }
  bind_rows(rows) |> mutate(tfr = tfr)
}
pop <- bind_rows(lapply(unname(SCENARIOS), project))
stopifnot(!anyNA(pop), all(pop$pop >= 0))

# --- Fiscal profiles onto the projected people ------------------------------------
TAXES <- c("labour_tax", "capital_tax", "consumption_tax", "excise_tax")
BENEFITS <- c("education", "healthcare", "cash_transfers")
prof <- readRDS("data/derived/age_profiles_2024.rds")$smooth |>
  select(age, all_of(c(TAXES, BENEFITS))) |>
  mutate(across(all_of(c(TAXES, BENEFITS)), ~ pmax(.x, 0) * PESO_FACTOR))
stopifnot(nrow(prof) == OPEN_AGE + 1, !anyNA(prof))

# Everyone 85 and older takes the 85+ row, as in F1 and F2.
by_age <- pop |>
  mutate(age = pmin(age, OPEN_AGE)) |>
  group_by(tfr, year, age) |>
  summarise(pop = sum(pop), .groups = "drop") |>
  left_join(prof, by = "age") |>
  mutate(across(all_of(c(TAXES, BENEFITS)), ~ .x * pop))   # pesos per year
stopifnot(!anyNA(by_age), nrow(by_age) == length(SCENARIOS) * length(YEARS) * (OPEN_AGE + 1))

# Payer ages: where F1's balance per person is positive (23-59 in 2024),
# read from the profiles, not typed.
net_pp <- rowSums(prof[TAXES]) - rowSums(prof[BENEFITS])
PAYER_AGES <- prof$age[net_pp > 0]
stopifnot(length(PAYER_AGES) > 0, all(diff(PAYER_AGES) == 1))

# The same by single age up to 100 (100 and older together), for the video:
# every age from 85 on takes the 85+ amounts per person.
TOP_AGE <- 100
by_single_age <- pop |>
  mutate(age = pmin(age, TOP_AGE)) |>
  group_by(tfr, year, age) |>
  summarise(pop = sum(pop), .groups = "drop") |>
  mutate(profile_age = pmin(age, OPEN_AGE)) |>
  left_join(prof, by = c("profile_age" = "age")) |>
  transmute(tfr, year, age, pop,
            taxes = (labour_tax + capital_tax + consumption_tax + excise_tax) * pop,
            benefits = (education + healthcare + cash_transfers) * pop)
stopifnot(!anyNA(by_single_age))

totals <- by_age |>
  group_by(tfr, year) |>
  summarise(people = sum(pop),
            payers = sum(pop[age %in% PAYER_AGES]),
            taxes = sum(across(all_of(TAXES))),
            benefits = sum(across(all_of(BENEFITS))),
            .groups = "drop") |>
  rename(pop = people) |>
  mutate(net = taxes - benefits, net_per_person = net / pop,
         net_per_payer = net / payers) |>
  # GDP, everything else equal: each person of payer age produces what one
  # did in 2024, so GDP moves with their number. 2024 GDP in August 2026 pesos.
  group_by(tfr) |>
  mutate(gdp = GDP_2024 * PESO_FACTOR * payers / payers[year == 2024],
         net_pct_gdp = 100 * net / gdp) |>
  ungroup()
stopifnot(!anyNA(totals), all(totals$gdp > 0))

pop_by_year <- pop |>
  group_by(tfr, year) |>
  summarise(share_65 = sum(pop[age >= 65]) / sum(pop),
            share_u15 = sum(pop[age < 15]) / sum(pop),
            births_proxy = sum(pop[age == 0]),
            people = sum(pop), .groups = "drop")

stopifnot(abs(sum(by_single_age$taxes) / sum(totals$taxes) - 1) < 1e-9,
          abs(sum(by_single_age$benefits) / sum(totals$benefits) - 1) < 1e-9)

saveRDS(list(by_age = by_age, by_single_age = by_single_age, totals = totals,
             pop_by_year = pop_by_year,
             assumptions = list(scenarios = SCENARIOS, tfr_conapo_2025 = TFR_CONAPO_2025,
                                payer_ages = range(PAYER_AGES),
                                srb = SRB, peso_factor = PESO_FACTOR)),
        "data/derived/projection_2025_2075.rds")

print(totals |> filter(year %% 10 == 5 | year == 2075) |>
        mutate(pop = round(pop / 1e6, 1), net_bn = round(net / 1e9),
               per_person = round(net_per_person), per_payer = round(net_per_payer),
               gdp_bn = round(gdp / 1e9), pct_gdp = round(net_pct_gdp, 1)) |>
        select(tfr, year, pop, net_bn, per_person, per_payer, gdp_bn, pct_gdp), n = 30)
print(pop_by_year |> filter(year %in% c(2024, 2025, 2050, 2075)) |>
        mutate(across(c(share_65, share_u15), ~ round(100 * .x, 1)),
               people = round(people / 1e6, 1), births_proxy = round(births_proxy / 1e6, 2)))
message(sprintf("SRB %.3f; CONAPO 2025 TFR %.3f", SRB, TFR_CONAPO_2025))
message("DONE!")
