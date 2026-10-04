# ============================================
# Official 2024 totals the build scales to, in pesos. One source line each.
# Approved by Luis on 2026-10-02 (decisions 2 to 5); full record in notes.md.
# Sourced by 01_build_profiles.R.
# ============================================
MDP <- 1e6  # the sources print millions of pesos

TARGETS <- list(
  # ISR paid by individuals: retenciones por salarios 1,254,807.4 + personas
  # físicas 76,664.7. SAT, Informe Tributario y de Gestión 4T 2024.
  isr_individual = 1331472.1 * MDP,
  # Cuotas obrero-patronales: IMSS 565,753.9 + ISSSTE 42,384.1, government
  # contributions excluded. SHCP, Informe 4T 2024, anexo de finanzas públicas.
  payroll = 608138.0 * MDP,
  # IVA total. SHCP, Informe 4T 2024.
  iva = 1407982.5 * MDP,
  # IEPS total, including the lines the script has no spending codes for
  # (alimentos no básicos, combustibles fósiles, telecom, apuestas,
  # plaguicidas, about 62 bn). SHCP, Informe 4T 2024.
  ieps = 628364.1 * MDP,
  # Nine cash programmes, subsidios (the part that reaches beneficiaries).
  # Cuenta Pública 2024, Tomo III, Gasto por Categoría Programática.
  programmes = 614265.4 * MDP,
  # Contributory pensions (pensiones y jubilaciones): IMSS 849,769.6, ISSSTE
  # 343,002.8, Pemex 83,824.6, CFE 61,531.4, Ramo 19 direct 115,534.9. The
  # Bienestar old-age pension is NOT in this line (it sits in programmes).
  # Cuenta Pública 2024, Tomo I, Gastos Presupuestarios p. 18. Decision 3.
  pensions = 1453663.2 * MDP,
  # Public spending on education 2024: federal 1,131,528.6 (final figure, SEP
  # Principales Cifras 2024-2025 and 2025-2026, bolsillo, p. 52) + state
  # 260,593.6 + municipal 212.4 (estimates, edition 2023-2024, table "Gasto en
  # educación", the last one that prints them). Private spending (460,252.1)
  # is out. That edition's own public total, 1,379,648.9, used the approved
  # federal budget (1,118,842.8). Decision 4.
  education = (1131528.6 + 260593.6 + 212.4) * MDP
)

# Federal spending per public-school student by tipo educativo, 2024, pesos.
# Used only as RELATIVE costs between levels; the level comes from
# TARGETS$education. SEP, Principales Cifras 2024-2025 and 2025-2026
# (bolsillo), "Gasto federal educativo", p. 52. Superior includes posgrado
# and science and technology spending.
EDU_REL <- c(basica = 32800, media_superior = 31300, superior = 64000)
stopifnot(!anyNA(unlist(TARGETS)), all(unlist(TARGETS) > 0))

# Public health spending 2024 by institution, pesos, devengado. Each total is
# spread over the people the survey reports under that institution. Decision 5.
# Cuenta Pública 2024, cuentapublica.hacienda.gob.mx/work/models/CP/2024/tomo/,
# "Estado analítico del ejercicio del presupuesto de egresos en clasificación
# funcional", row Salud (re-read in the PDFs on 2026-10-02):
#   IMSS            472,506,751,069  VII/Print.50GYR.03.F_FUNC.pdf
#   ISSSTE           88,350,626,046  VII/Print.51GYN.03.F_FUNC.pdf
#   Defensa           8,783,561,910  III/Print.7R07.03.FUNC_GF.pdf
#   Marina            6,161,427,254  III/Print.13R13.03.FUNC_GF.pdf
#   IMSS-Bienestar  169,080,524,639  VII/Print.47AYO.03.F_FUNC.pdf
#   Ramo 12 Salud    60,682,553,565  III/Print.12R12.03.FUNC_GF.pdf
#   FASSA (Ramo 33)  97,128,401,223  III/Print.33R33.03.FUNC_GF.pdf
# Pemex prints no Salud row; its figure is programme E013 "Servicios médicos
# al personal de PEMEX", 17,641,612,509, VIII/Print.52TYY.03.F_EAEPCFPE.pdf.
# One survey answer (inst_4) covers Pemex, Defensa and Marina, so they pool.
# "open" serves people without social security. Ramo 12 and FASSA are net of
# what was moved to IMSS-Bienestar during the year (budget reductions), so
# the three are read as not double counted.
# Not included: ISSSTE estatal (no figure; takes the ISSSTE rate per person)
# and state governments' own health spending.
HEALTH <- c(imss        = 472506751069,
            issste      = 88350626046,
            pemex_armed = 17641612509 + 8783561910 + 6161427254,
            open        = 169080524639 + 60682553565 + 97128401223)
stopifnot(!anyNA(HEALTH), all(HEALTH > 0))

# Nominal GDP 2024 at market prices, pesos. Used only to express the
# projected totals as a share of GDP (05_projection.R).
# INEGI, Cuentas de Bienes y Servicios 2024, Comunicado de prensa 133/25
# (2 Oct 2025): "El PIB de 2024 a precios de mercado cerró en 33 506 847 mdp
# corrientes". Same figure in PIB por Entidad Federativa 2024, Comunicado
# 156/25 (5 Dec 2025). Both read in the PDFs on 2026-10-03.
# The sum of the quarterly releases (33,913,829, used for a few hours on
# 2026-10-03) was a preliminary figure, 1.2% higher.
GDP_2024 <- 33506847 * MDP
