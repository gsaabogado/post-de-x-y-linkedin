# _people.R --------------------------------------------------------------------
# People by single age and sex in 2024 from the survey's person file, smoothed.
# Sourced by 03_fig_totals.R (F2) and 05_projection.R (base of the
# projection), so the projection starts from the people F2 shows.
#
# Head counts carry age heaping (people report 30, 40, 50, 60, 80, 90), which
# makes every tenth age jump, so they are smoothed by sex with a loess and
# rescaled to keep their sum:
#   - ages 0 to keep_age: a fit over ages 0-84 (span 0.15), as since
#     2026-10-02, so nothing moved for them when F2 was extended to age 100;
#   - older ages up to top_age - 1: a fit over ages 0 to top_age - 1 in one
#     piece (the same 13-age window), which leaves no step at 85 (Luis,
#     2026-10-04), rescaled so the people aged 0 to top_age - 1 add up to the
#     survey's;
#   - top_age and older: as surveyed, ages above max_age folded into max_age.
# Ages 0-4 are replaced afterwards by registered births (scripts/_children.R).
#
# Returns list(m, f): named numeric vectors, names = ages 0..max_age.

people_2024 <- function(max_age = 109, open_age = 85, keep_age = 75, top_age = 100) {
  enigh <- read.csv("data/poblacion.csv", colClasses = "character")[, c("sexo", "edad", "factor")]
  enigh$edad <- pmin(as.integer(enigh$edad), max_age)
  enigh$factor <- as.numeric(enigh$factor)
  stopifnot(!anyNA(enigh), all(enigh$sexo %in% c("1", "2")), keep_age < open_age, open_age < top_age)
  one_sex <- function(code) {
    raw <- tapply(enigh$factor[enigh$sexo == code], enigh$edad[enigh$sexo == code], sum)
    v <- data.frame(edad = 0:max_age, n = 0)
    v$n[match(as.integer(names(raw)), v$edad)] <- raw
    closed <- v$edad < open_age
    fit_low <- predict(loess(n ~ edad, data = v[closed, ], span = 0.15))
    fit_low <- fit_low * sum(v$n[closed]) / sum(fit_low)
    single <- v$edad < top_age
    fit_all <- predict(loess(n ~ edad, data = v[single, ], span = 0.15 * open_age / top_age))
    low <- v$edad <= keep_age
    upper <- v$edad > keep_age & single
    stopifnot(all(fit_low[which(low)] > 0), all(fit_all[which(upper)] > 0))
    total_single <- sum(v$n[single])
    v$n[low] <- fit_low[which(low)]
    v$n[upper] <- fit_all[which(upper)] * (total_single - sum(v$n[low])) / sum(fit_all[which(upper)])
    stopifnot(abs(sum(v$n[single]) - total_single) < 1, all(v$n >= 0),
              abs(v$n[keep_age + 2] / fit_all[keep_age + 2] - 1) < 0.03)   # the seam is smooth
    setNames(v$n, v$edad)
  }
  out <- list(m = one_sex("1"), f = one_sex("2"))
  stopifnot(abs(sum(out$m) + sum(out$f) - sum(enigh$factor)) < 1)
  out
}
