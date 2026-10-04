# _money.R --------------------------------------------------------------------
# Carries the profiles from 2024 pesos to pesos of August 2026, the series
# convention (latest INPC month, deflated from the period the value refers to).
#
# Every tax is scaled to what SHCP collected over the whole of 2024, and the
# unit costs of education and health are 2024 annual figures, so the period is
# the calendar year and the base is the 2024 average of the monthly INPC. ENIGH
# pensions and programmes were collected Aug-Nov 2024 (INPC 136.0-137.4, 0.6%
# above the year average); they move with the same single factor.
#   - INPC, base 2Q Jul 2018 = 100: monthly 2024 values and August 2026 from
#     elcontribuyente.mx/inpc (INEGI table, read 2026-10-01). August 2026
#     matches the value posts 04 and 06 took from INEGI.
#     December 2024 (137.949) checked against INEGI's release of 2025-01-09.

INPC_2024 <- c(133.555, 133.681, 134.065, 134.336, 134.087, 134.594,
               136.003, 136.013, 136.080, 136.828, 137.424, 137.949)
INPC_2026_08 <- 145.462
stopifnot(length(INPC_2024) == 12)

# Pesos of August 2026 per peso of 2024.
PESO_FACTOR <- INPC_2026_08 / mean(INPC_2024)
