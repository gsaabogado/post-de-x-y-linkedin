# 06_anim_projection.R --------------------------------------------------------
# Video F3: taxes paid and benefits received in total at each age, year by
# year from 2025 to 2075, under two fertility rates side by side. Series style.
#
# Design (Luis, 2026-10-03: the bar version "se ve muy tosco", kept as
# 06_anim_projection.R.bak_bars): single ages 0-100 as two filled shapes
# (taxes up, benefits down), the 2025 shapes as a dotted outline, the year as
# a large pale number, and three plain numbers per panel: the average deficit
# per person, the total deficit and its share of GDP. Motion is continuous:
# every year, with STEPS_PER_YEAR blended frames between two years.
#
# The video opens on 2025, runs to 2075 and holds there 5 s (series rule: a
# video ends and holds on its still). mp4 + gif, and the last frame as a png.
#
# Run from the post root, after 05: Rscript scripts/06_anim_projection.R
# English: FIG_LANG=en Rscript scripts/06_anim_projection.R
# A few frames only: PREVIEW_FRAMES=1,150,301 Rscript scripts/06_anim_projection.R

library(dplyr)
library(tidyr)
library(ggplot2)
library(parallel)

source("../_style/style.R")
source("scripts/_layout.R")

FIG_LANG <- Sys.getenv("FIG_LANG", "es")
stopifnot(FIG_LANG %in% c("es", "en"))
es_en <- function(es, en) if (FIG_LANG == "en") en else es

BN <- 1e9
Y0 <- 2025; Y1 <- 2075
FPS <- 24
STEPS_PER_YEAR <- 6            # blended frames per year: 50 years in 12.5 s
HOLD_FIRST <- 1.5; HOLD_LAST <- 5   # seconds
W_IN <- FIG_W; H_IN <- FIG_H; DPI <- 240   # 2880 x 1920, downscaled to VIDEO_PX (1800 x 1200)
CORES <- 4                     # the workstation is shared: keep the render narrow
FRAMES <- "images/frames_f3"

proj <- readRDS("data/derived/projection_2025_2075.rds")
ages <- proj$by_single_age |>
  filter(year >= Y0, year <= Y1) |>
  transmute(tfr, year, age, taxes = taxes / BN, benefits = -benefits / BN) |>
  arrange(tfr, year, age)
tot <- proj$totals |>
  filter(year >= Y0, year <= Y1) |>
  transmute(tfr, year, deficit = -net_per_person, total_bn = -net / BN, pct = -net_pct_gdp) |>
  arrange(tfr, year)
stopifnot(!anyNA(ages), !anyNA(tot), all(tot$deficit > 0), n_distinct(ages$tfr) == 2)

pesos <- function(v) paste0("$", format(round(v), big.mark = ",", trim = TRUE))
# The two fertility rates come from 05 (survey and registry), never typed here.
LO <- proj$assumptions$scenarios[["low"]]
HI <- proj$assumptions$scenarios[["high"]]
num <- function(t) vapply(t, function(v) format(v, nsmall = 1), character(1))   # 1.2, 1.46
scen <- function(t) sprintf(es_en("%s hijos por mujer", "%s children per woman"), num(t))

# The fixed line of the video, from the data.
cmp <- tot |> select(tfr, year, deficit) |>
  pivot_wider(names_from = tfr, values_from = deficit, names_prefix = "t")
worse <- cmp[[paste0("t", LO)]] > cmp[[paste0("t", HI)]]
CROSS <- min(cmp$year[sapply(seq_along(worse), function(i) all(worse[i:length(worse)]))])
stopifnot(CROSS > 2030, CROSS < Y1)
at <- function(t, y) tot$deficit[tot$tfr == t & tot$year == y]
TITLE <- sprintf(es_en(
  "Con %s hijos por mujer el déficit por persona sería menor\nque con %s hasta %d, pero en %d llegaría a %s contra %s",
  "With %s children per woman the deficit per person would stay\nbelow that with %s until %d, but by %d it would reach %s against %s"),
  num(LO), num(HI), CROSS - 1, Y1, pesos(at(LO, Y1)), pesos(at(HI, Y1)))

# One frame = a point in time t (2025, 2025 + 1/6, ...): values blended
# linearly between the two neighbouring years, by scenario and age.
TIMES <- c(seq(Y0, Y1 - 1 / STEPS_PER_YEAR, by = 1 / STEPS_PER_YEAR), Y1)
N_FRAMES <- length(TIMES)
blend <- function(df, t, cols) {
  y <- floor(t + 1e-9); w <- t - y
  a <- df[df$year == y, ]
  if (w < 1e-9) return(a)
  b <- df[df$year == y + 1, ]
  stopifnot(nrow(a) == nrow(b), all(a$tfr == b$tfr))
  for (cc in cols) a[[cc]] <- (1 - w) * a[[cc]] + w * b[[cc]]
  a
}

ghost <- ages |> filter(year == Y0) |> mutate(panel = scen(tfr))
BAR_TOP <- max(ages$taxes)
Y_TOP <- BAR_TOP * 2.05          # the upper half of the panel holds the numbers
Y_BOT <- min(ages$benefits) * 1.05

make_frame <- function(i, mode = "light") {
  p <- pal(mode)
  t <- TIMES[i]
  a <- blend(ages, t, c("taxes", "benefits")) |> mutate(panel = scen(tfr), net = taxes + benefits)
  b <- blend(tot, t, c("deficit", "total_bn", "pct")) |> mutate(panel = scen(tfr))
  first <- b |> filter(tfr == min(tfr))
  second <- b |> filter(tfr == max(tfr))
  txt <- function(data, x, y, label, ...) {
    data$.x <- x; data$.y <- y; data$.label <- label
    geom_text(data = data, aes(x = .x, y = .y, label = .label), family = STYLE_FONT, ...)
  }
  ggplot() +
    geom_area(data = a, aes(age, taxes), fill = p$primary, alpha = 0.92) +
    geom_area(data = a, aes(age, benefits), fill = p$accent, alpha = 0.85) +
    # 2025 stays as a dotted outline, so the change reads at a glance
    geom_line(data = ghost, aes(age, taxes), colour = p$sub, linewidth = 0.45, linetype = "22") +
    geom_line(data = ghost, aes(age, benefits), colour = p$sub, linewidth = 0.45, linetype = "22") +
    geom_hline(yintercept = 0, colour = p$bg, linewidth = 0.8) +
    geom_line(data = a, aes(age, net), colour = p$bg, linewidth = 2) +
    geom_line(data = a, aes(age, net), colour = p$ink, linewidth = 0.8) +
    # the year, large and quiet
    txt(b, 1, Y_BOT * 0.80, as.character(floor(t + 1e-9)), hjust = 0, colour = p$grid,
        fontface = "bold", size = 17) +
    # the three numbers
    txt(b, 100, Y_TOP * 0.93, es_en("Déficit promedio por persona", "Average deficit per person"),
        hjust = 1, colour = p$sub, size = 3.3) +
    txt(b, 100, Y_TOP * 0.805, pesos(b$deficit), hjust = 1, colour = p$ink, fontface = "bold", size = 8) +
    txt(b, 100, Y_TOP * 0.605,
        sprintf(es_en("Déficit total: %s mil millones\n%.1f%% del PIB", "Total deficit: %s billion\n%.1f%% of GDP"),
                pesos(b$total_bn), b$pct),
        hjust = 1, colour = p$sub, size = 3.4, lineheight = 1.05) +
    # direct labels instead of a legend
    txt(first, 2, Y_TOP * 0.93, es_en("Contribuciones que pagan", "Contributions paid"),
        hjust = 0, colour = p$primary, fontface = "bold", size = 3.6) +
    txt(first, 28, Y_BOT * 0.30, es_en("Transferencias\nque reciben", "Transfers\nreceived"),
        hjust = 0, colour = p$accent, fontface = "bold", size = 3.6, lineheight = 0.95) +
    txt(second, 2, Y_TOP * 0.93, sprintf(es_en("Línea punteada: %d", "Dotted line: %d"), Y0),
        hjust = 0, colour = p$sub, size = 3.2) +
    facet_wrap(~ panel, ncol = 2) +
    scale_x_continuous(breaks = seq(0, 100, 20), expand = expansion(add = c(0.5, 0.5))) +
    scale_y_continuous(breaks = seq(-200, 100, 100),
                       labels = function(v) paste0(es_en("$", ""), abs(v)),
                       limits = c(Y_BOT, Y_TOP), expand = expansion(0)) +
    labs(x = es_en("Edad", "Age"),
         y = es_en("Miles de millones de pesos al año\npor cada edad (pesos de agosto de 2026)",
                   "Billions of pesos per year\nat each age (August 2026 pesos)"),
         title = TITLE,
         caption = caption_ls(
           notes = c(
             es_en("Este ejercicio es ilustrativo. La tasa de fecundidad permanece constante. La mortalidad sigue los valores de 2025. No hay migración y cada persona paga y recibe lo mismo que en 2024.",
                   "This exercise is illustrative. The fertility rate stays constant. Mortality follows the 2025 values. There is no migration and each person pays and receives what they did in 2024."),
             es_en("El PIB cambia con el número de personas de 23 a 59 años.",
                   "GDP changes with the number of people aged 23 to 59."),
             es_en("No es el déficit del gobierno, porque faltan los impuestos de las empresas y el gasto que no llega a una persona en particular.",
                   "It is not the government deficit, because taxes paid by companies and spending that reaches no particular person are left out."),
             es_en("Cada decisión y su fuente están en el código, github.com/gsaabogado/post-de-x-y-linkedin.",
                   "Every decision and its source are in the code, github.com/gsaabogado/post-de-x-y-linkedin.")),
           source = es_en("INEGI (ENIGH 2024, Encuesta Intercensal 2025, nacimientos registrados 2025 y PIB de 2024), CONAPO (mortalidad y edades de la fecundidad), SHCP, SAT, Cuenta Pública y SEP.",
                          "INEGI (ENIGH 2024, 2025 Intercensal Survey, registered births 2025 and 2024 GDP), CONAPO (mortality and the age pattern of fertility), SHCP, SAT, Cuenta Pública and SEP."),
           width = CAP_CHARS, width_list = CAP_CHARS)) +
    theme_ls(mode) +
    theme(strip.text = element_text(size = rel(1.1)), panel.spacing.x = unit(1.6, "lines"),
          plot.caption = element_text(lineheight = 1.1, size = rel(CAP_REL))) +   # numbered notes: tighter leading
    byline_ls(mode)
}

render_one <- function(i) {
  f <- file.path(FRAMES, sprintf("f%04d.png", i))
  ggsave(f, make_frame(i), width = W_IN, height = H_IN, dpi = DPI,
         bg = pal("light")$bg, device = ragg::agg_png)
  f
}

dir.create(FRAMES, recursive = TRUE, showWarnings = FALSE)
preview <- Sys.getenv("PREVIEW_FRAMES")
if (nzchar(preview)) {
  for (i in as.integer(strsplit(preview, ",")[[1]])) message(render_one(i), "  t=", round(TIMES[i], 2))
  message("PREVIEW DONE!")
  quit(save = "no")
}
unlink(list.files(FRAMES, full.names = TRUE))

# PSOCK, not fork: a forked child that touches the macOS font stack crashes.
t0 <- Sys.time()
message(sprintf("rendering %d frames on %d workers...", N_FRAMES, CORES))
wd <- getwd()
cl <- makeCluster(CORES)
clusterExport(cl, ls(globalenv()), envir = globalenv())
invisible(clusterEvalQ(cl, {
  setwd(wd)
  suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2); library(ragg)})
}))
res <- parLapply(cl, seq_len(N_FRAMES), function(i) try(render_one(i), silent = TRUE))
stopCluster(cl)
bad <- vapply(res, function(r) inherits(r, "try-error") || is.null(r), logical(1))
if (any(bad)) { message(res[[which(bad)[1]]]); stop(sum(bad), " frames failed") }
n_written <- length(list.files(FRAMES, pattern = "^f[0-9]{4}\\.png$"))
if (n_written != N_FRAMES) stop(sprintf("expected %d frames, found %d", N_FRAMES, n_written))
message(sprintf("frames done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

suffix <- if (FIG_LANG == "en") "_en" else ""
still <- sprintf("images/f3_projection_2075_light%s.png", suffix)
file.copy(file.path(FRAMES, sprintf("f%04d.png", N_FRAMES)), still, overwrite = TRUE)

mp4 <- sprintf("images/video_f3_projection_light%s.mp4", suffix)
gif <- sprintf("images/video_f3_projection_light%s.gif", suffix)
hold <- sprintf("tpad=start_mode=clone:start_duration=%s:stop_mode=clone:stop_duration=%s", HOLD_FIRST, HOLD_LAST)
system2("ffmpeg", c("-y", "-loglevel", "error", "-framerate", FPS,
                    "-i", file.path(FRAMES, "f%04d.png"),
                    "-vf", shQuote(paste0(hold, sprintf(",scale=%d:%d:flags=lanczos", VIDEO_PX[1], VIDEO_PX[2]))),
                    "-c:v", "libx264", "-preset", "slow", "-pix_fmt", "yuv420p",
                    "-crf", "16", "-movflags", "+faststart", mp4))
system2("ffmpeg", c("-y", "-loglevel", "error", "-i", mp4,
                    "-vf", shQuote("fps=12,scale=1100:-2:flags=lanczos,split[a][b];[a]palettegen=stats_mode=diff[p];[b][p]paletteuse=dither=bayer:bayer_scale=3"),
                    gif))
if (!file.exists(mp4) || file.size(mp4) < 1e5) stop("mp4 encoding failed")
if (!file.exists(gif) || file.size(gif) < 1e4) stop("gif encoding failed")
unlink(FRAMES, recursive = TRUE)

message(sprintf("wrote %s (%.1f MB), %s (%.1f MB) and %s", mp4, file.size(mp4) / 1e6, gif, file.size(gif) / 1e6, still))
message("DONE!")
