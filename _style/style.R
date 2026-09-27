# _style/style.R ----------------------------------------------------------------
# One visual identity for every post in the series, in a light and a dark
# version. Source it from a post's scripts: source("../_style/style.R").
#
# Taken from the figures in Luis's papers (AEA P&P remittances, Temperature and
# Remittances, EJ Mexico, Madrid Central), not invented for social media:
#   - Helvetica Neue, bold panel titles aligned left
#   - axis lines on the left and bottom only, faint horizontal grid, no
#     vertical grid, no panel box
#   - one dark slate blue for the main estimate, one brick red as the accent,
#     greys for context (support histograms, reference groups)
#   - point estimates with capped intervals, a dotted line at zero
#   - notes and source under the figure, as in a journal
# What it deliberately avoids, because it reads as generated: giant hero
# numbers, neon gradients, glow, emoji, centred text, more than two accent
# colours in one chart.

library(ggplot2)

STYLE_FONT <- "Helvetica Neue"

# Every colour a figure may use. Figures reference roles, never hex codes, so
# one switch flips a whole chart between light and dark.
STYLE <- list(
  light = list(
    bg = "#FFFFFF", ink = "#1B1F24", sub = "#5A6270", faint = "#8A919C",
    grid = "#E9E9E9", axis = "#4A4A4A",
    primary = "#2C3E50",       # slate blue: the main estimate
    accent = "#C0392B",        # brick red: the comparison or the highlight
    primary_soft = "#AEB8C4", accent_soft = "#F2D4CF",
    support = "#9A9A9A",       # histograms, reference groups
    signature = "#9C7409",     # ochre, the byline ONLY; never encodes data
    # sequential ramp for maps, low -> high, ends on the accent
    ramp = c("#F4F1EC", "#E9D3C8", "#DDA99A", "#CD7766", "#B8473A", "#8E2A20", "#5C1810"),
    # diverging ramp for maps of a rate against a reference (below -> above),
    # slate blue to a neutral to brick red; the neutral must differ from bg
    div = c("#1F3347", "#4F6B8A", "#9DAFC2", "#CFCAC2", "#DDA193", "#C0392B", "#7A1F16")
  ),
  dark = list(
    bg = "#0F1216", ink = "#E9ECEF", sub = "#A3ACB8", faint = "#6F7885",
    grid = "#242A32", axis = "#8A919C",
    primary = "#8FB3D9",       # the slate blue, lifted to read on dark
    accent = "#E4604E",
    primary_soft = "#3A4B60", accent_soft = "#5A2A24",
    support = "#6B7280",
    signature = "#D4A437",
    ramp = c("#171B21", "#2E2A33", "#5A3136", "#8E3A33", "#C4513F", "#E88A63", "#F6CDA8"),
    div = c("#C7DCF0", "#8FB3D9", "#58789C", "#6E6A66", "#B0584A", "#E4604E", "#F6B59E")
  )
)

pal <- function(mode = c("light", "dark")) STYLE[[match.arg(mode)]]

# The series theme. `mode` picks the palette; everything else is shared.
theme_ls <- function(mode = "light", base_size = 13) {
  p <- pal(mode)
  theme_minimal(base_family = STYLE_FONT, base_size = base_size) +
    theme(
      plot.background = element_rect(fill = p$bg, colour = NA),
      panel.background = element_rect(fill = p$bg, colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = p$grid, linewidth = 0.35),
      axis.line = element_line(colour = p$axis, linewidth = 0.45),
      axis.ticks = element_line(colour = p$axis, linewidth = 0.4),
      axis.ticks.length = unit(3, "pt"),
      axis.text = element_text(colour = p$sub, size = rel(0.82)),
      axis.title = element_text(colour = p$ink, size = rel(0.9)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.title.x = element_text(margin = margin(t = 8)),
      plot.title = element_text(colour = p$ink, face = "bold", size = rel(1.35),
                                margin = margin(b = 3)),
      plot.subtitle = element_text(colour = p$sub, size = rel(0.95), margin = margin(b = 14)),
      plot.caption = element_text(colour = p$sub, size = rel(0.62), hjust = 0,
                                  lineheight = 1.35, margin = margin(t = 14)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      strip.text = element_text(colour = p$ink, face = "bold", hjust = 0, size = rel(0.95)),
      legend.position = "none",
      plot.margin = margin(22, 26, 16, 22)
    )
}

# Figure language (Luis, 2026-09-27): X in Spanish, LinkedIn carousel in English.
# FIG_LANG=en Rscript scripts/xx.R writes *_light_en.png; the default (es) writes
# the Spanish files unchanged. Not LANG: that is the shell locale.
FIG_LANG <- Sys.getenv("FIG_LANG", "es")
stopifnot(FIG_LANG %in% c("es", "en"))
es_en <- function(es, en) if (FIG_LANG == "en") en else es

# Journal-style footer: notes, then source, in grey at the left.
# Spanish only here; the English gloss lives in the subtitle and axis titles.
caption_ls <- function(notes = NULL, source, width = 150) {
  parts <- c(if (!is.null(notes)) strwrap(paste(es_en("Notas:", "Notes:"), notes), width),
             strwrap(paste(es_en("Fuente:", "Source:"), source), width))
  paste(parts, collapse = "\n")
}

# The series signature (Luis, 2026-09-24): the byline alone, bottom right of
# the image, in the ochre `signature` colour that no data ever uses. No series
# label, logo or watermark. save_ls() adds it to every figure.
BYLINE <- "Luis Sarmiento \u00b7 ETH Z\u00fcrich \u00b7 @LSarmientoEcon"
byline_ls <- function(mode = "light") {
  list(labs(tag = BYLINE),
       theme(plot.tag.location = "plot", plot.tag.position = "bottomright",
             plot.tag = element_text(family = STYLE_FONT, colour = pal(mode)$signature,
                                     size = 9, face = "plain", hjust = 1, vjust = 0)))
}

# Title in Spanish, English gloss as the subtitle's first line.
title_ls <- function(es, en) list(title = es, subtitle = en)

# Save at the size X and LinkedIn crop best (16:9 at 3840 px).
# Light only by default (Luis, 2026-09-24: the series publishes the light
# version). The dark palette stays in pal(); pass modes = c("light", "dark")
# to render both.
save_ls <- function(make_plot, stem, width = 12, height = 6.75, dpi = 320,
                    modes = "light") {
  for (m in modes) {
    f <- sprintf("%s_%s%s.png", stem, m, if (FIG_LANG == "en") "_en" else "")
    ggsave(f, make_plot(m) + byline_ls(m), width = width, height = height, dpi = dpi,
           bg = pal(m)$bg, device = ragg::agg_png)
    message("wrote ", f)
  }
}
