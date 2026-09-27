# Tweet 5: municipal DAC share (CFE customers by tariff, 2017) vs panel rate (EIC 2025).
# Place-level association only. Prints the table; writes nothing. ~20 s.
library(dplyr)
library(readr)
library(tidyr)

MLPV <- path.expand(Sys.getenv("MLPV_IN"))  # folder with the MLPV inputs; set in .Renviron
if (!nzchar(MLPV)) stop("Set MLPV_IN (the MLPV project's in/ folder) in .Renviron; see README")
EIC_ZIP <- file.path(MLPV, "Census/intercensal2025/raw/conjunto_de_datos_eic2025_105_csv.zip")
CFE <- file.path(MLPV, "Electricity/cfe_users_consumption_mun/cfe_users_consumption_mun_2010_2017.csv")
PARENT <- c("12082" = "12053", "12083" = "12012", "12084" = "12041", "12085" = "12023",
            "24059" = "24028", "25019" = "25006", "25020" = "25011")

eic <- read_csv(unz(EIC_ZIP, "conjunto_de_datos/conjunto_datos_eic2025_105.csv"),
                col_types = cols(.default = "c"), locale = locale(encoding = "latin1"))
mun <- eic |> filter(ESTIMADOR == "Valor", CVE_MUN != "000", CVE_LOC == "0000") |>
  transmute(cve = substr(CVEGEO, 1, 5), den = as.numeric(VIVPARHAB),
            pct = suppressWarnings(as.numeric(PCN_VPH_PANEL))) |>
  filter(!is.na(pct)) |>
  mutate(num = den * pct / 100, cve = ifelse(cve %in% names(PARENT), PARENT[cve], cve)) |>
  group_by(cve) |> summarise(den = sum(den), num = sum(num), .groups = "drop")

cfe <- read_csv(CFE, col_types = cols(cvegeo = "c", tarifa = "c", .default = "d",
                                      entidad = "c", municipio = "c", cve_ent = "c", cve_mun = "c")) |>
  filter(year == 2017, tarifa %in% c("01", "1A", "1B", "1C", "1D", "1E", "1F", "DAC")) |>
  group_by(cve = cvegeo) |>
  summarise(res = sum(users), dac = sum(users[tarifa == "DAC"]), .groups = "drop")

d <- inner_join(mun, cfe, by = "cve") |> filter(res > 0)
message(sprintf("matched %d municipalities, %.1f%% of 2025 dwellings",
                nrow(d), 100 * sum(d$den) / sum(mun$den)))
d <- d |> mutate(dac_sh = 100 * dac / res)

# Dwelling-weighted groups of DAC share.
d <- d |> mutate(g = cut(dac_sh, c(-Inf, 0.25, 0.5, 1, 2, 4, Inf),
                         labels = c("<0.25", "0.25-0.5", "0.5-1", "1-2", "2-4", ">4")))
print(d |> group_by(g) |>
        summarise(n_mun = n(), dwell_M = sum(den) / 1e6,
                  panel_pct = 100 * sum(num) / sum(den), dac_pct = 100 * sum(dac) / sum(res)))
w <- d$den
message(sprintf("weighted corr(dac share, panel rate) = %.2f",
                cov.wt(cbind(d$dac_sh, 100 * d$num / d$den), wt = w / sum(w), cor = TRUE)$cor[1, 2]))
# Share of 2025 panel homes in the municipalities that held half of DAC customers.
d <- d |> arrange(desc(dac_sh))
print(d |> slice_head(n = 15) |> mutate(panel = 100 * num / den) |> select(cve, res, dac_sh, panel))
