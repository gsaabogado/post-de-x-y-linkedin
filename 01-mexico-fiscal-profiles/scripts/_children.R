# _children.R ------------------------------------------------------------------
# Children aged 0-4 in 2024 from registered births, by sex. Sourced by
# 03_fig_totals.R (F2) and 05_projection.R (base of the projection), so both
# start from the same children.
#
# Why: the survey undercounts small children (ENIGH 2024 has 1.19 million
# aged 0, when 1.4 to 1.5 million births a year are registered).
#
# Births by year of occurrence come from INEGI, Estadística de Nacimientos
# Registrados 2025 (Reporte de resultados 38/26, Tabla 1), transcribed in
# data/inegi/enr2025_births_by_occurrence.csv. Age a in 2024 = the births of
# 2024 - a, survivors at CONAPO's 2025 death rates (data/conapo/).
#
# Recent years are not fully registered yet. Births registered two or more
# years late were `late_share` of those registered in the year of birth or the
# next one for 2016-2018 (the cohorts observed longest, before the pandemic
# shifted registration). Recent cohorts get that share, never less than what
# is already registered.

children_0_4_2024 <- function() {
  enr <- read.csv("data/inegi/enr2025_births_by_occurrence.csv")
  stopifnot(!anyNA(enr), all(enr$reg_year >= enr$occ_year))
  occ <- sort(unique(enr$occ_year))
  prompt <- sapply(occ, function(y) sum(enr$births[enr$occ_year == y & enr$reg_year <= y + 1]))
  so_far <- sapply(occ, function(y) sum(enr$births[enr$occ_year == y]))
  ref <- occ %in% 2016:2018
  late_share <- mean((so_far[ref] - prompt[ref]) / prompt[ref])
  births_of <- setNames(pmax(so_far, prompt * (1 + late_share)), occ)
  stopifnot(all(as.character(2020:2024) %in% names(births_of)))

  cp <- read.csv("data/conapo/conapo_2025_national.csv")
  stopifnot(!anyNA(cp))
  col <- function(name, sex) {
    d <- cp[cp$sexo == sex, ]
    setNames(d[[name]][order(d$edad)], sort(d$edad))
  }
  mx <- list(m = col("defunciones_2025", "Hombres") / col("pob_mitad_2025", "Hombres"),
             f = col("defunciones_2025", "Mujeres") / col("pob_mitad_2025", "Mujeres"))
  # Sex ratio at birth from CONAPO's age-0 population on 1 Jan 2025, corrected
  # for the half year of infant deaths that differs by sex.
  srb <- (col("pob_inicio_2025", "Hombres")[["0"]] / exp(-mx$m[["0"]] / 2)) /
    (col("pob_inicio_2025", "Mujeres")[["0"]] / exp(-mx$f[["0"]] / 2))
  share_boys <- srb / (1 + srb)

  survive_to <- function(m, a) {   # share of a birth cohort alive at age a (mid-year)
    if (a == 0) return(exp(-m[["0"]] / 2))
    exp(-sum(m[as.character(0:(a - 1))]) - m[[as.character(a)]] / 2)
  }
  ages <- 0:4
  b <- births_of[as.character(2024 - ages)]
  list(m = setNames(b * share_boys * sapply(ages, function(a) survive_to(mx$m, a)), ages),
       f = setNames(b * (1 - share_boys) * sapply(ages, function(a) survive_to(mx$f, a)), ages),
       births_of = births_of, late_share = late_share)
}
