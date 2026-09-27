# 10_fx.R ---------------------------------------------------------------------
# Pesos per US dollar in August 2026, to put dollars in parentheses next to the
# pesos of August 2026 (Luis, 2026-09-26). Same month as the INPC we deflate
# to, so the dollar is a dollar of today, like the peso.
#
# Source: FRED EXMXUS, Mexican pesos to one US dollar, monthly average of the
# Federal Reserve H.10 noon buying rates. Banxico's FIX would be the local
# choice, but its API needs a token this machine does not have.
#
# Run from the post root: Rscript scripts/10_fx.R

library(readr)

url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=EXMXUS&cosd=2025-01-01"
dest <- "data/derived/exmxus.csv"
download.file(url, dest, quiet = TRUE, mode = "wb")
# fredgraph serves this CSV chunked, with no Content-Length; check it is whole instead
fx <- read_csv(dest, show_col_types = FALSE)
names(fx) <- c("date", "mxn_per_usd")
stopifnot(nrow(fx) >= 12, !anyNA(fx$date))

fx_aug <- fx$mxn_per_usd[fx$date == as.Date("2026-08-01")]
stopifnot(length(fx_aug) == 1, !is.na(fx_aug), fx_aug > 10, fx_aug < 30)
message(sprintf("August 2026: %.4f pesos per dollar", fx_aug))
saveRDS(list(mxn_per_usd = fx_aug, month = "2026-08", series = "FRED EXMXUS",
             read = Sys.Date()), "data/derived/fx_2026_08.rds")
