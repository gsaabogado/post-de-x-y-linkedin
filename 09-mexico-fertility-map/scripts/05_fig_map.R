#### 05_fig_map.R
#### Fertility map post, figure 1: children per woman by municipality,
#### Encuesta Intercensal 2025 (births of 2024), on INEGI's Intercensal 2025
#### boundary frame. Classes centred on the replacement level 2.1; red below
#### (falling short of replacement), blue above (Luis, 2026-11-16).
#### Run from the post folder: Rscript scripts/05_fig_map.R  (FIG_LANG=en for English)

library(tidyverse)
library(sf)
set.seed(20261116)
source("../_style/style.R")

mun <- readRDS("data/derived/mun_2025.rds")
ent <- readRDS("data/derived/ent_2025.rds")
eic <- readRDS("data/derived/eic_mun.rds")

NAT <- 1.23                                   # national row of the Intercensal file
stopifnot(abs(weighted.mean(eic$tgf, eic$f15_49, na.rm = TRUE) - NAT) < 0.01)

# Numbers for text, from the data
pop_tot   <- sum(eic$pob)
share_21  <- sum(eic$pob[eic$tgf >= 2.1], na.rm = TRUE) / pop_tot
n_21      <- sum(eic$tgf >= 2.1, na.rm = TRUE)
share_nat <- sum(eic$pob[eic$tgf < NAT], na.rm = TRUE) / pop_tot
n_mi      <- sum(is.na(eic$tgf))
n_cens    <- sum(eic$censused)
cat(sprintf("At or above 2.1: %d municipalities, %.1f%% of people\n", n_21, 100 * share_21))
cat(sprintf("Below the national value: %.1f%% of people\n", 100 * share_nat))

# Colour classes (Luis, 2026-11-16): centred on the replacement level 2.1,
# few classes as in the Europe map, so regions read without smoothing. Six
# red classes below 2.1, three blue above (colours flipped by Luis, 2026-11-16:
# below replacement is the concern); the national 1.23 is in the notes.
# First draft (continuous, centred on 1.23) kept as images/f1_map_v1_centre_national_light.png;
# blue-below version as images/f1_map_v2_blue_below_light.png.
BRK <- c(0.8, 1.0, 1.2, 1.4, 1.7, REPL <- 2.1, 2.5, 3.0)
cls <- cut(eic$tgf, c(-Inf, BRK, Inf), right = FALSE)
cat("Municipalities per class:\n"); print(table(cls, useNA = "ifany"))
# The 7 municipalities without an estimate ("MI") take the plain average of
# the municipalities that border them (Luis, 2026-11-16), so no hole reads as
# missing data. Neighbours = within 500 m: the simplified polygons no longer
# share borders exactly. Text numbers above stay on observed values only.
nb <- st_is_within_distance(mun, mun, dist = 500)
mun$tgf_map <- mun$tgf
for (i in which(is.na(mun$tgf))) {
  j <- setdiff(nb[[i]], i)
  v <- mun$tgf[j]
  stopifnot(sum(!is.na(v)) >= 2)
  mun$tgf_map[i] <- mean(v, na.rm = TRUE)
  cat(sprintf("Filled %s %s: %.2f from %d neighbours\n", mun$cve_mun[i], mun$nom_mun[i],
              mun$tgf_map[i], sum(!is.na(v))))
}
stopifnot(!anyNA(mun$tgf_map))
mun <- mun %>% mutate(cls = cut(tgf_map, c(-Inf, BRK, Inf), right = FALSE))
CLS_LAB <- c(es_en("menos\nde 0.8", "under\n0.8"),
             sprintf(es_en("%s a\n%s", "%s to\n%s"), head(c("0.8", "1.0", "1.2", "1.4", "1.7", "2.1", "2.5"), 7),
                     c("1.0", "1.2", "1.4", "1.7", "2.1", "2.5", "3.0")),
             es_en("3.0\no más", "3.0\nor more"))
# The reds end on a pale pink, not on the ramp's grey neutral, which read
# like "no data" (first classed render, 2026-11-16). div runs slate -> grey -> brick.
PALE_PINK <- "#ECCBC1"   # darkened from #F3DFD9, which read as white (Luis, 2026-11-16)
class_cols <- function(p) c(colorRampPalette(c(rev(p$div[5:7]), PALE_PINK))(6),
                            colorRampPalette(rev(p$div[1:4]))(4)[2:4])

# Widen the frame so the panel fills the 16:9 canvas: with a fixed-shape map
# the title, byline and notes otherwise shrink to the map's width. The extra
# room goes west, over the Pacific, where the legend sits.
bb <- st_bbox(mun); W <- bb$xmax - bb$xmin; H <- bb$ymax - bb$ymin
XLIM <- c(bb$xmin - 0.55 * W, bb$xmax + 0.08 * W)
YLIM <- c(bb$ymin - 0.02 * H, bb$ymax + 0.02 * H)

make <- function(mode) {
  p <- pal(mode)
  ggplot() +
    geom_sf(data = mun, aes(fill = cls), colour = NA) +
    geom_sf(data = ent, fill = NA, colour = p$bg, linewidth = 0.35) +
    scale_fill_manual(values = setNames(class_cols(p), levels(mun$cls)),
                      labels = CLS_LAB, drop = FALSE, na.translate = FALSE,
                      na.value = p$bg,
                      name = es_en("Hijos por mujer", "Children per woman")) +
    coord_sf(crs = st_crs(mun), datum = NA, expand = FALSE,
             xlim = XLIM, ylim = YLIM) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom",
                               keywidth = unit(1.45, "cm"), keyheight = unit(0.4, "cm"))) +
    labs(title = es_en("Hijos por mujer en cada municipio de México, 2024",
                       "Children per woman in each Mexican municipality, 2024"),
         subtitle = es_en("Children per woman in each Mexican municipality, 2024",
                          "Intercensal Survey 2025"),
         caption = caption_ls(
           notes = c(
             es_en("Hijos por mujer: los hijos que tendría una mujer a lo largo de su vida si en cada edad los tuviera al ritmo de las mujeres de esa edad en 2024.",
                   "Children per woman: the children a woman would have over her life if at each age she had them at the rate of women that age in 2024."),
             sprintf(es_en("Rojo, menos de 2.1 hijos por mujer, el nivel que mantiene la población a largo plazo. Azul, 2.1 o más. El país en conjunto tiene %.2f.",
                           "Red, fewer than 2.1 children per woman, the level that keeps a population stable in the long run. Blue, 2.1 or more. The country as a whole has %.2f."), NAT),
             sprintf(es_en("%d municipios, con %.1f%% de la población, tienen 2.1 hijos por mujer o más.",
                           "%d municipalities, with %.1f%% of the population, have 2.1 children per woman or more."),
                     n_21, 100 * share_21),
             sprintf(es_en("La encuesta visitó todas las viviendas en %d municipios y una muestra en los demás. En %d municipios la muestra no alcanza y el mapa usa el promedio de los municipios vecinos.",
                           "The survey visited every home in %d municipalities and a sample in the rest. In %d municipalities the sample is too small and the map shows the average of the neighbouring municipalities."),
                     n_cens, n_mi)),
           source = es_en("Instituto Nacional de Estadística y Geografía (INEGI), Encuesta Intercensal 2025, principales resultados por localidad de 50 000 y más habitantes (22 de septiembre de 2026), y Marco Geoestadístico, Encuesta Intercensal 2025.",
                          "Instituto Nacional de Estadística y Geografía (INEGI), Encuesta Intercensal 2025, main results by locality of 50,000 or more inhabitants (22 September 2026), and Marco Geoestadístico, Encuesta Intercensal 2025."),
           width = 185)) +
    theme_ls(mode) +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), panel.grid.major.y = element_blank(),
          legend.position = "inside", legend.position.inside = c(0.0, 0.08),
          legend.justification = c(0, 0),
          legend.title = element_text(colour = p$ink, size = rel(0.85), face = "bold"),
          legend.text = element_text(colour = p$sub, size = rel(0.68), lineheight = 1),
          legend.key.spacing.x = unit(0, "pt"),
          legend.background = element_blank())
}

save_ls(make, "images/f1_map")
message("DONE!")
