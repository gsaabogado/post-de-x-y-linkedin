# 08_dollars.R -----------------------------------------------------------------
# The peso amounts quoted in the LinkedIn text and on the website page, with
# their value in US dollars. Series rule: money in pesos of the latest INPC
# month (August 2026) and, in English texts, dollars of the same month in
# parentheses.
#
# The rate is the August 2026 average of FRED's EXMXUS, saved by post 04
# (../04-mexico-gdp-marginals/scripts/10_fx.R). The amounts come from the
# figures' own scripts: 02 (F1), 03 (F2) and 05 (projection).
#
# Run from the post root, after 02, 03 and 05: Rscript scripts/08_dollars.R

fx <- readRDS("../04-mexico-gdp-marginals/data/derived/fx_2026_08.rds")
stopifnot(fx$month == "2026-08", is.numeric(fx$mxn_per_usd), fx$mxn_per_usd > 10, fx$mxn_per_usd < 30)
rate <- fx$mxn_per_usd

f1 <- readRDS("data/derived/f1_headline.rds")
f2 <- readRDS("data/derived/f2_headline.rds")
tot <- readRDS("data/derived/projection_2025_2075.rds")$totals
sc <- sort(unique(tot$tfr))
stopifnot(length(sc) == 2)
at_2075 <- function(t) -tot$net_per_person[tot$tfr == t & tot$year == 2075]
stopifnot(length(at_2075(sc[1])) == 1, length(at_2075(sc[2])) == 1)

num <- function(v, digits = 0) format(round(v, digits), big.mark = ",", nsmall = max(digits, 0), trim = TRUE)
line <- function(what, pesos, unit_pesos, usd, unit_usd)
  cat(sprintf("%-46s %s %s = US$%s %s\n", what, pesos, unit_pesos, usd, unit_usd))

cat(sprintf("Rate: %.4f pesos per dollar (%s, %s)\n", rate, fx$series, fx$month))
line(sprintf("F1 peak, age %d", f1$peak_age), num(f1$peak), "pesos", num(f1$peak / rate, -1), "")
line(sprintf("F1 trough, age %d", f1$trough_age), num(abs(f1$trough)), "pesos", num(abs(f1$trough) / rate, -1), "")
line(sprintf("F2 net paid in, ages %d to %d", f1$age_in, f1$age_out),
     num(f2$net_payers / 1000, 1), "trillion pesos", num(f2$net_payers / rate), "billion")
line("F2 net received, the young and the old",
     num(abs(f2$net_young + f2$net_old) / 1000, 1), "trillion pesos",
     num(abs(f2$net_young + f2$net_old) / rate), "billion")
line(sprintf("Deficit per person 2075, %s children", sc[1]), num(at_2075(sc[1])), "pesos", num(at_2075(sc[1]) / rate, -1), "")
line(sprintf("Deficit per person 2075, %s children", sc[2]), num(at_2075(sc[2])), "pesos", num(at_2075(sc[2]) / rate, -1), "")
