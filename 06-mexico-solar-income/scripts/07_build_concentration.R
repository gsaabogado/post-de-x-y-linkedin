# 07_build_concentration.R ----------------------------------------------------
# Concentration curves of solar panels by labour income, Censo 2020. Feeds F4.
#
# Homes are ranked by monthly labour income adjusted for household size
# (INGTRHOG / sqrt(NUMPERS), see 03), poorest first, and the
# curve plots the cumulative share of homes (x) against the cumulative share of
# homes WITH A PANEL (y). The diagonal is equal adoption at every income; the
# further the curve sags below it, the more the panels sit at the top.
#
# Same sample as the deciles of F2: homes with positive labour income. Homes
# with none (8%) are left out rather than ranked at the bottom, because the
# group includes pensioner households whose true income is not zero. Each area
# is ranked within its own income distribution.
#
# The share of panels held by the top 10% and 20% is a ratio estimated over the
# domain "homes with a panel", so it gets a design-based SE from _survey.R.
#
# Run from the post root, after 03: Rscript scripts/07_build_concentration.R

library(dplyr)

source("scripts/_survey.R")

v <- readRDS("data/derived/viv2020_slim.rds")
nh <- survey_setup(v)
v <- v |> mutate(area = ifelse(rural, "rural", "urbano"))

pos <- v |> filter(!is.na(inc), inc > 0)

# Weighted rank within area, 0-1, poorest first. Ties (common: incomes are
# reported in round numbers) share their group's midpoint so the curve does
# not depend on row order.
rank_area <- function(d) {
  d |>
    group_by(area, inc) |>
    # new names, never `w`: reusing it would make the next two lines multiply
    # by the group's summed weight instead of each home's
    summarise(hw = sum(w), hp = sum(w * panel), ha = sum(w * aire), .groups = "drop") |>
    group_by(area) |>
    arrange(inc, .by_group = TRUE) |>
    mutate(cum_h = cumsum(hw) / sum(hw),
           cum_p = cumsum(hp) / sum(hp),
           cum_a = cumsum(ha) / sum(ha),
           mid = cum_h - hw / sum(hw) / 2) |>
    ungroup()
}
curve <- bind_rows(
  rank_area(pos),
  rank_area(mutate(pos, area = "nacional"))
)

# Concentration index = 1 - 2 * area under the curve (0 = equal, 1 = all at the top).
conc <- curve |> group_by(area) |>
  summarise(ci_panel = 1 - sum((cum_p + lag(cum_p, default = 0)) * (cum_h - lag(cum_h, default = 0))),
            ci_aire = 1 - sum((cum_a + lag(cum_a, default = 0)) * (cum_h - lag(cum_h, default = 0))),
            .groups = "drop")
print(conc)

# Top-share estimates with SEs. top10/top20 flag homes above the area's
# 90th/80th weighted percentile of size-adjusted labour income.
lookup <- curve |> filter(area != "nacional") |> select(area, inc, mid)
pos <- pos |> left_join(lookup, by = c("area", "inc")) |>
  mutate(top10 = as.integer(mid > 0.9), top20 = as.integer(mid > 0.8))
stopifnot(!anyNA(pos$mid))

# Map back onto the full frame so the design (all clusters) is intact.
v <- v |> left_join(pos |> select(ESTRATO, psu, w, inc, area, panel, top10, top20) |>
                      distinct(), by = c("ESTRATO", "psu", "w", "inc", "area", "panel"),
                    relationship = "many-to-many")
stopifnot(nrow(v) == nrow(readRDS("data/derived/viv2020_slim.rds")))

dom_panel <- ifelse(v$panel == 1 & !is.na(v$top10), v$area, NA)
top <- bind_rows(
  ratio_se(v, nh, coalesce(v$top10, 0L), dom_panel) |> mutate(stat = "top10"),
  ratio_se(v, nh, coalesce(v$top20, 0L), dom_panel) |> mutate(stat = "top20")
)
homes_top <- pos |> group_by(area) |>
  summarise(h10 = sum(w * top10) / sum(w), h20 = sum(w * top20) / sum(w), .groups = "drop")
print(top)
print(homes_top)

# The curve and the design-based estimate must agree on the top-10% share when
# both cut at the same place (the last tied income group with midpoint <= 0.9).
chk <- curve |> filter(area != "nacional") |> group_by(area) |>
  summarise(curve_top10 = 100 * (1 - max(cum_p[mid <= 0.9])), .groups = "drop") |>
  left_join(top |> filter(stat == "top10") |> select(area = dom, est = pct), by = "area")
print(chk)
stopifnot(all(abs(chk$curve_top10 - chk$est) < 0.01))

saveRDS(list(curve = curve, conc = conc, top = top, homes_top = homes_top),
        "data/derived/concentration_2020.rds")
message("DONE!")
