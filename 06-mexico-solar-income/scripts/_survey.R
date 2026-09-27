# _survey.R -------------------------------------------------------------------
# Design-based SE for a ratio over any set of domains, Censo 2020 extended
# questionnaire. Taylor linearisation, ultimate clusters (ESTRATO x UPM) within
# strata ESTRATO; fully enumerated strata (every FACTOR == 1) add no variance,
# single-PSU strata are left out and counted. Expects a data frame `v` with
# ESTRATO, psu, w.

survey_setup <- function(v) {
  cert <- v |> dplyr::group_by(ESTRATO) |> dplyr::summarise(cert = all(w == 1), .groups = "drop")
  nh <- v |> dplyr::distinct(ESTRATO, psu) |> dplyr::count(ESTRATO, name = "n_h") |>
    dplyr::left_join(cert, by = "ESTRATO")
  message(sprintf("%d strata, %d fully enumerated, %d with a single PSU",
                  nrow(nh), sum(nh$cert), sum(nh$n_h == 1 & !nh$cert)))
  nh
}

# y: outcome (0/1), dom: domain label per home (NA = outside every domain).
ratio_se <- function(v, nh, y, dom) {
  d <- dplyr::tibble(ESTRATO = v$ESTRATO, psu = v$psu, w = v$w, y = y, dom = dom) |>
    dplyr::filter(!is.na(dom))
  est <- d |> dplyr::group_by(dom) |>
    dplyr::summarise(X = sum(w), Y = sum(w * y), n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(R = Y / X)
  z <- d |> dplyr::left_join(est |> dplyr::select(dom, X, R), by = "dom") |>
    dplyr::mutate(z = w * (y - R) / X) |>
    dplyr::group_by(dom, ESTRATO, psu) |> dplyr::summarise(z = sum(z), .groups = "drop") |>
    dplyr::group_by(dom, ESTRATO) |>
    dplyr::summarise(s1 = sum(z), s2 = sum(z^2), .groups = "drop") |>
    dplyr::left_join(nh, by = "ESTRATO") |>
    dplyr::filter(!cert, n_h > 1) |>
    dplyr::mutate(v = n_h / (n_h - 1) * (s2 - s1^2 / n_h)) |>
    dplyr::group_by(dom) |> dplyr::summarise(var = sum(v), .groups = "drop")
  est |> dplyr::left_join(z, by = "dom") |>
    dplyr::transmute(dom, n, homes = X, pct = 100 * R, se = 100 * sqrt(dplyr::coalesce(var, 0)))
}
