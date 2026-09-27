# 05_build_ac.R ---------------------------------------------------------------
# Solar panels by air conditioning within income decile, Censo 2020. Feeds F3.
#
# Plotting AC and panel shares on one axis does not work (AC runs 10-33% by
# decile, panels 0.3-2.1%), so F3 splits each decile by AC ownership instead.
# Same decile cut points, universe and SEs as 03.
#
# AC here stands in for heavy electricity use, but it also marks hot places,
# which are sunnier and sit on the more subsidised summer tariffs. The figure
# shows an association within income bands, not what AC does to adoption.
#
# Run from the post root, after 03: Rscript scripts/05_build_ac.R

library(dplyr)
library(tidyr)

source("scripts/_survey.R")

v <- readRDS("data/derived/viv2020_slim.rds")
nh <- survey_setup(v)

area <- ifelse(v$rural, "rural", "urbano")
ac <- ifelse(v$aire == 1, "con", "sin")

by_dec <- ratio_se(v, nh, v$panel, paste(area, ac, v$grp)) |>
  separate(dom, c("area", "ac", "grp"), sep = " ", extra = "merge")
by_area <- ratio_se(v, nh, v$panel, paste(area, ac)) |>
  separate(dom, c("area", "ac"), sep = " ")
stopifnot(nrow(by_dec) == 2 * 2 * 12, !anyNA(by_dec$pct))

# How much of the panel stock sits in homes with AC, against their share of homes.
share <- tibble(
  homes_ac = sum(v$w * v$aire) / sum(v$w),
  panels_ac = sum(v$w * v$panel * v$aire) / sum(v$w * v$panel)
)
print(by_area)
print(share)

urb <- by_dec |> filter(area == "urbano", grepl("^D", grp)) |>
  select(ac, grp, pct) |> pivot_wider(names_from = ac, values_from = pct) |>
  mutate(ratio = con / sin)
print(urb)
message(sprintf("urban deciles: homes with AC %.1f-%.1f times likelier to have a panel",
                min(urb$ratio), max(urb$ratio)))

saveRDS(list(by_dec = by_dec, by_area = by_area, share = share, urb_ratio = urb),
        "data/derived/ac_2020.rds")
message("DONE!")
