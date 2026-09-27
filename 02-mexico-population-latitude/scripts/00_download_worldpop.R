# 00_download_worldpop.R ------------------------------------------------------
# Fetch the WorldPop R2025A constrained population grid for Mexico (2025).
# Downloads are verified byte-for-byte against the server's Content-Length.
# Run from the project root: Rscript scripts/00_download_worldpop.R

library(tools)

BASE <- "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/2025/MEX/v1"

FILES <- list(
  grid_1km = file.path(BASE, "1km_ua/constrained/mex_pop_2025_CN_1km_R2025A_UA_v1.tif"),
  grid_100m = file.path(BASE, "100m/constrained/mex_pop_2025_CN_100m_R2025A_v1.tif")
)

# Only the 1 km grid is needed for the social-media render. Set to TRUE for a
# poster-resolution rebuild.
WANT_100M <- FALSE

data_dir <- "data"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

remote_size <- function(url) {
  h <- system2("curl", c("-sIL", "--max-time", "60", shQuote(url)), stdout = TRUE)
  cl <- grep("^[Cc]ontent-[Ll]ength:", h, value = TRUE)
  if (!length(cl)) stop("no Content-Length returned for ", url)
  as.numeric(sub("^[^:]+:\\s*", "", tail(cl, 1)))
}

fetch_verified <- function(url, dest) {
  want <- remote_size(url)
  if (file.exists(dest) && file.size(dest) == want) {
    message(sprintf("OK (cached)  %s  [%s bytes]", basename(dest), format(want, big.mark = ",")))
    return(invisible(dest))
  }
  message(sprintf("downloading  %s  [%s bytes]", basename(dest), format(want, big.mark = ",")))
  # -C - resumes a truncated file rather than silently keeping the short one
  rc <- system2("curl", c("-fL", "-C", "-", "--max-time", "1800",
                          "-o", shQuote(dest), shQuote(url)))
  if (rc != 0) stop("curl exited ", rc, " for ", url)
  got <- file.size(dest)
  if (got != want) {
    stop(sprintf("SHORT DOWNLOAD: %s is %s bytes, server reports %s",
                 basename(dest), format(got, big.mark = ","), format(want, big.mark = ",")))
  }
  message(sprintf("verified     %s  md5=%s", basename(dest), md5sum(dest)))
  invisible(dest)
}

fetch_verified(FILES$grid_1km, file.path(data_dir, basename(FILES$grid_1km)))
if (WANT_100M) fetch_verified(FILES$grid_100m, file.path(data_dir, basename(FILES$grid_100m)))

message("DONE!")
