# _layout.R -------------------------------------------------------------------
# Canvas and caption shared by the four pieces of this post (Luis, 2026-10-04:
# the 16:9 panel looked squat under eleven lines of notes, and note 4 of F1
# should fit on one line).
#
# The canvas is 3:2 instead of the series 16:9, and the caption is smaller so
# a note of about 275 characters runs the full width without wrapping. Longer
# notes still wrap (F1 note 4 in English, F4 note 3).
# CAP_CHARS is the number of characters that fit one caption line at CAP_REL
# on a FIG_W canvas: F1 note 4 in Spanish (271 characters) fills 97% of it
# and F3 note 2 (275) ends level with the signature.
# Raise CAP_CHARS only together with a look at the render, a caption line
# that is too long clips silently.

FIG_W <- 12; FIG_H <- 8   # inches, 3:2
CAP_REL <- 0.50           # caption size relative to the 13 pt base (6.5 pt)
CAP_CHARS <- 276          # strwrap width for notes and source

# Video frame in pixels, 3:2. Kept at 1200 px high because pages that list X's
# video limits give 1920 x 1200 as the largest frame (X's own help page could
# not be read on 2026-10-04; its API guide says 1280 x 1024 and aspect ratios
# from 1:1 to 3:1). Test the upload in X's composer before the publishing day.
VIDEO_PX <- c(1800, 1200)
