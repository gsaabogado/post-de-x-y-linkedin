# _money.R --------------------------------------------------------------------
# Converts the grid's PPP dollars of 2021 into pesos of August 2026, the series
# convention (latest INPC month, deflated from the period the value refers to).
#
# Within Mexico the grid's nominal and PPP columns differ by ONE constant factor
# (nominal / PPP = 0.4904 in every cell, checked 2026-09-25), so shares, strips
# and the Gini are unit-free and unchanged. Only levels move.
#
# Level: the grid's national total is rescaled to official GDP in current pesos,
# then carried from the 2021 average price level to August 2026.
#   - GDP 2021, current pesos: 26,690,033,392,000 (World Bank NY.GDP.MKTP.CN,
#     from INEGI; API read 2026-09-25). Positive check: the grid's nominal total,
#     1,316.569 bn USD, times the 2021 average rate 20.2724 MXN/USD (World Bank
#     PA.NUS.FCRF) gives 26,690.3 bn pesos, the same to 0.001%.
#   - INPC, base 2Q Jul 2018 = 100: monthly 2021 values from entornofiscal.com's
#     INEGI table; December 2021 (117.308) checked against INEGI's press release
#     (fortnights 117.301 and 117.314), and March 2020 / August 2026 match the
#     values post 06 took from INEGI.

GDP_MXN_2021 <- 26690033392000
INPC_2021 <- c(110.210, 110.907, 111.824, 112.190, 112.419, 113.018,
               113.682, 113.899, 114.601, 115.561, 116.884, 117.308)
INPC_2026_08 <- 145.462

# Pesos of August 2026 per PPP dollar of the grid, given the grid's PPP total.
mxn_factor <- function(total_ppp) {
  stopifnot(length(INPC_2021) == 12)
  GDP_MXN_2021 / total_ppp * INPC_2026_08 / mean(INPC_2021)
}
