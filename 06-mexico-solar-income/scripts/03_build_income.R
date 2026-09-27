# 03_build_income.R -----------------------------------------------------------
# Solar panels by household income, Censo 2020 extended questionnaire
# (dwelling microdata, about 4 million sampled homes). Feeds F2 to F4.
#
# Universe and share follow MLPV script 18 as its README documents them, so
# the national number here must reproduce that run (0.806%, SE 0.033):
#   universe  CLAVIVP 1-6 and 99 (inhabited private dwellings with
#             characteristics captured)
#   share     PANEL_SOLAR == 7 over the whole universe; "not specified" (9)
#             stays in the denominator, as in INEGI's "por cada cien viviendas"
#   SE        Taylor linearisation of the ratio, ultimate clusters
#             (ESTRATO x UPM) within strata ESTRATO; strata where every home has
#             FACTOR == 1 are fully enumerated and add no variance
#
# Income is INGTRHOG, the household's monthly LABOUR income. It excludes
# pensions, remittances and transfers. Homes are ranked by income adjusted for
# size with the square-root scale, inc = INGTRHOG / sqrt(NUMPERS) (2026-09-24;
# per person, / NUMPERS, before that, results kept in data/derived/pc_2026-09-24).
# Amounts are in pesos of August 2026: the census asks for last month's income
# and was taken in March 2020, so inc is scaled by INPC Aug 2026 / Mar 2020
# (145.462 / 106.838, base 2Q Jul 2018 = 100; INEGI). The scale changes only
# the labels of the cut points, never the ranking.
# Two groups are kept OUT of the deciles and reported on their own:
#   sin ingreso laboral   INGTRHOG == 0 (nobody in the home works for pay)
#   no especificado       INGTRHOG blank or 999999
# Deciles are national, weighted by FACTOR, over homes with positive income,
# so urban and rural bars compare the same income bands.
#
# Run from the post root: Rscript scripts/03_build_income.R

library(dplyr)
library(readr)

source("scripts/_survey.R")

MLPV <- path.expand(Sys.getenv("MLPV_IN"))  # folder with the MLPV inputs; set in .Renviron
if (!nzchar(MLPV)) stop("Set MLPV_IN (the MLPV project's in/ folder) in .Renviron; see README")
ZIP <- file.path(MLPV, "Census/censo2020_ca/raw/Censo2020_CA_eum_csv.zip")
OUT <- "data/derived"
INPC_2026_08 <- 145.462
INPC_2020_03 <- 106.838

v <- read_csv(pipe(paste("unzip -p", shQuote(path.expand(ZIP)), "Viviendas00.CSV")),
              col_select = c(ESTRATO, UPM, FACTOR, CLAVIVP, PANEL_SOLAR, AIRE_ACON,
                             NUMPERS, INGTRHOG, TAMLOC),
              col_types = cols(.default = "c"))
stopifnot(nrow(v) == 4016627)

v <- v |>
  filter(CLAVIVP %in% c("01", "02", "03", "04", "05", "06", "99")) |>
  mutate(w = as.numeric(FACTOR),
         panel = as.integer(PANEL_SOLAR == "7"),
         aire = as.integer(AIRE_ACON == "5"),
         numpers = as.numeric(NUMPERS),
         ing = suppressWarnings(as.numeric(INGTRHOG)),
         ing = ifelse(ing == 999999, NA, ing),
         inc = ing / sqrt(numpers) * INPC_2026_08 / INPC_2020_03,
         rural = TAMLOC == "1",
         psu = paste(ESTRATO, UPM))
stopifnot(!anyNA(v$w), !anyNA(v$panel), !anyNA(v$aire), all(v$numpers >= 1))

# Weighted decile cut points over homes with positive labour income.
wq <- function(x, w, p) {
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  sapply(p, function(q) x[which(cw >= q)[1]])
}
pos <- v |> filter(!is.na(inc), inc > 0)
cuts <- wq(pos$inc, pos$w, seq(0.1, 0.9, 0.1))
v <- v |> mutate(grp = case_when(
  is.na(inc) ~ "no especificado",
  inc == 0   ~ "sin ingreso laboral",
  TRUE      ~ sprintf("D%02d", findInterval(inc, cuts, left.open = TRUE) + 1L)))

# Every positive-income decile should hold ~10% of those homes.
dshare <- v |> filter(grepl("^D", grp)) |> count(grp, wt = w) |> mutate(s = n / sum(n))
print(dshare)
stopifnot(all(abs(dshare$s - 0.1) < 0.01))

# --- design-based SEs (scripts/_survey.R) ---------------------------------------
nh <- survey_setup(v)

nat <- ratio_se(v, nh, v$panel, rep("nacional", nrow(v)))
print(nat)
message(sprintf("national %.3f%% (SE %.3f) vs MLPV script 18: 0.806%% (SE 0.033)", nat$pct, nat$se))
stopifnot(abs(nat$pct - 0.806) < 0.0005, abs(nat$se - 0.033) < 0.0015)

area <- ifelse(v$rural, "rural", "urbano")
by_grp <- bind_rows(
  ratio_se(v, nh, v$panel, paste(area, v$grp)) |> mutate(var = "panel"),
  ratio_se(v, nh, v$panel, v$grp) |> mutate(var = "panel", dom = paste("total", dom)),
  ratio_se(v, nh, v$aire, paste(area, v$grp)) |> mutate(var = "aire"),
  ratio_se(v, nh, v$aire, v$grp) |> mutate(var = "aire", dom = paste("total", dom))
) |>
  mutate(area = sub(" .*", "", dom), grp = sub("^[a-z]+ ", "", dom)) |>
  select(var, area, grp, n, homes, pct, se) |>
  arrange(var, area, grp)
print(by_grp, n = 100)

# Shares of homes and of panels held by each group (for F4 later).
saveRDS(list(by_grp = by_grp, cuts = cuts, nat = nat), file.path(OUT, "income_2020.rds"))
saveRDS(v |> select(w, panel, aire, inc, grp, rural, ESTRATO, psu),
        file.path(OUT, "viv2020_slim.rds"))
message("DONE!")
