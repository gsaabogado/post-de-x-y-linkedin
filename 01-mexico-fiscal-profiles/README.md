# 01 · Contributions paid and transfers received by age in Mexico

What the average Mexican pays in contributions and receives in transfers from the government at each age, in 2024. The balance is positive only between ages 23 and 59, and it reaches its lowest point at 70, when pensions and health care peak. A simple illustrative exercise then carries these profiles to 2075 under two fertility rates, to show why births matter for public finances. It is not a forecast.

The figure follows the method of National Transfer Accounts. The household survey says who pays and who receives, and each amount is then scaled so that it adds up to its official 2024 total. Every total and its source is in `scripts/_targets.R`.

## Inputs

The scripts read the ENIGH 2024 microdata (Nueva serie) from INEGI, inegi.org.mx/rnm/index.php/catalog/1116.

- `poblacion.csv`, `ingresos.csv` and `trabajos.csv` go in `01-mexico-fiscal-profiles/data/`.
- `concentradohogar.csv` and `gastoshogar.csv` go in a folder named by `ENIGH_HH` in `01-mexico-fiscal-profiles/.Renviron`.
- The projection also uses CONAPO's Proyecciones de la población de México 2020-2070 (population, deaths and fertility by age, 2025). The host, repodatos.atdt.gob.mx, refuses scripted downloads, so `scripts/04_conapo_extract.js` is a snippet to paste into a browser console on that site. It saves the two national extracts that go in `data/conapo/`.
- Registered births by year of occurrence (INEGI, Estadística de Nacimientos Registrados 2025, Reporte de resultados 38/26, Tabla 1), transcribed in `data/inegi/enr2025_births_by_occurrence.csv`.

The repository holds no survey microdata. The two small extracts in `data/conapo/` and `data/inegi/` are included, because one cannot be downloaded by a script and the other is transcribed from a report.

## Run order

From the post folder, `Rscript scripts/01_build_profiles.R` builds the age profiles (in 2024 pesos), then `Rscript scripts/02_fig_profiles.R` draws the figure in pesos of August 2026 (`scripts/_money.R`). `03_fig_totals.R` draws the totals by age, `05_projection.R` projects the population to 2075, `06_anim_projection.R` makes the video with the two fertility scenarios, and `07_fig_crossing.R` draws the deficit per person over time. `08_dollars.R` prints the peso amounts quoted in the texts with their value in dollars (it reads the August 2026 rate saved by `../04-mexico-gdp-marginals/scripts/10_fx.R`), `09_carousel_pdf.R` joins the English figures into the LinkedIn carousel, and `10_web_figures.sh` writes the web copies of the figures. `FIG_LANG=en` gives the English version of every figure. The canvas (12 x 8 inches) and the size of the notes are set once for all four pieces in `scripts/_layout.R`.

## Decisions

Each of these choices changes the figure, so each is stated here with its source.

- **Survey codes.** Income sources follow the INEGI catalogue for `ingresos.clave`. Social programmes are P038, P043, P045, P048 and P101 to P108. Income from own businesses (P012, P019, P068 to P081) pays income tax. Soft drinks are spending codes 012601, 012602 and 012604. Diet sodas are left out because the tax applies to added sugar.
- **Formal workers.** A person is formal when they are insured at IMSS, ISSSTE, state ISSSTE or Pemex as a benefit of their own job (`inscr_1` with `inst_1` to `inst_4` in `poblacion`). Only formal workers pay income tax on wages and social security contributions.
- **Contributions.** Income tax paid by individuals, 1,331,472 million pesos (salary withholding plus personas físicas, SAT). IMSS and ISSSTE contributions, 608,138 million (SHCP). VAT, 1,407,983 million (SHCP). The whole excise tax (IEPS), 628,364 million (SHCP), spread over spending on fuel, alcohol, tobacco and soft drinks. Taxes paid by companies are left out.
- **Pensions.** The survey's pensions (P032) are scaled to the official total for contributory pensions, 1,453,663 million (Cuenta Pública 2024). The survey cannot separate private pensions, so they are scaled too.
- **Social programmes.** Scaled to the nine cash programmes as paid to beneficiaries, 614,265 million (Cuenta Pública 2024).
- **Education.** Students in public schools receive a cost by level. The ratios between levels come from SEP's federal spending per student (basic 32,800, upper secondary 31,300, higher 64,000 pesos), and the total is public spending on education, 1,392,335 million. That total is the final federal figure plus SEP's estimates of state and municipal spending.
- **Health.** Each institution's 2024 health spending (Cuenta Pública 2024) is spread over the people who report it in the survey: IMSS, ISSSTE, and Pemex together with the armed forces. People without social security share one pool made of IMSS-Bienestar, the health ministry and the state health fund (FASSA). State ISSSTE has no figure of its own and takes the ISSSTE rate. Within each group, older and very young people receive more, following how often each age uses public health services.
- **Projection to 2075.** Everything else is held equal. Each woman always has 1.2 children (INEGI, 2025 Intercensal Survey, for 2024) or 1.46 (the rate that the births registered for 2024 imply, once late registrations are added; computed in `05_projection.R` from INEGI's registered births by year of occurrence). Deaths stay at CONAPO's 2025 rates by age and sex, there is no migration, and what each person pays and receives at each age stays as in 2024. The starting population is the ENIGH 2024 one, the same people as the totals figure, except ages 0 to 4, which come from registered births because the survey undercounts small children. GDP moves with the number of people aged 23 to 59. The age pattern of births comes from CONAPO.

## Limits

The age multipliers for health costs are assumptions, not sourced figures. State governments' own health spending is not included. Public spending that reaches no particular person, such as security or roads, is left out. Curves are smoothed, and the amounts per person stop at an open group of everyone aged 85 or older. The figure of totals by age shows single ages up to 90: every age from 85 on takes that group's amounts per person and only the number of people changes. People older than 90 are not drawn but count in every total. The head counts by age, smoothed for age heaping, come from `scripts/_people.R`, shared with the projection.
