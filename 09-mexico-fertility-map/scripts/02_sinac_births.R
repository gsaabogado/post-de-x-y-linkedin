#### 02_sinac_births.R
#### Fertility map post: live births that OCCURRED in 2024 (and 2025), from the
#### SINAC files of 2024 and 2025 (Secretaría de Salud, DGIS datos abiertos),
#### counted by mother's municipality of residence, by municipality of the
#### delivery, and by mother's five-year age group.
#### The 2025 file is a zip inside a zip: unzip it once into SINAC25_CSV
#### (scratch location set in .Renviron), never into the archive.
#### Output: data/derived/sinac_births.rds

library(tidyverse)
set.seed(20261116)

dir <- Sys.getenv("SINAC_DIR")
f24 <- file.path(dir, "sinac_2024.zip")
f25 <- Sys.getenv("SINAC25_CSV")
stopifnot(file.exists(f24), file.exists(f25))

cols <- c("EDAD", "RESIDEEXTRANJERO", "ENTIDADRESIDENCIA", "MUNICIPIORESIDENCIA",
          "FECHANACIMIENTO", "ENTIDADFEDERATIVAPARTO", "MUNICIPIOPARTO")

rd <- function(con, file_year) {
  read_csv(con, col_select = all_of(cols), col_types = cols(.default = "c"),
           progress = FALSE) %>%
    mutate(file_year = file_year)
}
b <- bind_rows(rd(pipe(paste("unzip -p", shQuote(f24))), 2024),
               rd(f25, 2025))
cat("Records read:", nrow(b), "\n")

b <- b %>%
  mutate(age = as.integer(EDAD),
         yr = as.integer(str_sub(FECHANACIMIENTO, 7, 10)),
         res = paste0(ENTIDADRESIDENCIA, MUNICIPIORESIDENCIA),
         del = paste0(ENTIDADFEDERATIVAPARTO, MUNICIPIOPARTO),
         agegrp = cut(age, seq(15, 50, 5), right = FALSE,
                      labels = c("15_19", "20_24", "25_29", "30_34",
                                 "35_39", "40_44", "45_49")))

cat("Births by year of occurrence and file:\n")
print(count(b, file_year, yr) %>% filter(yr >= 2022) %>% pivot_wider(names_from = file_year, values_from = n))
cat("Resident abroad (RESIDEEXTRANJERO):\n"); print(count(b, RESIDEEXTRANJERO))
cat("Residence municipality unknown (999 or state 99):",
    sum(b$MUNICIPIORESIDENCIA == "999" | b$ENTIDADRESIDENCIA %in% c("88", "99")), "\n")
cat("Mother age outside 15-49 or missing:", sum(is.na(b$agegrp)), "\n")

occ <- b %>% filter(yr %in% c(2024, 2025))
by_res <- occ %>% filter(!is.na(agegrp)) %>% count(yr, cve_mun = res, agegrp, name = "births")
by_del <- occ %>% filter(!is.na(agegrp)) %>% count(yr, cve_mun = del, agegrp, name = "births")
res_del <- occ %>% count(yr, res, del, name = "births")
tot <- occ %>% count(yr, name = "all_births")

saveRDS(list(by_res = by_res, by_del = by_del, res_del = res_del, tot = tot),
        "data/derived/sinac_births.rds")
rm(b, occ); gc()
