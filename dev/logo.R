## Code to reproduce the chromConverter hex sticker (`dev/figures/logo.png`).
##
## Run from the package root with `source("dev/logo.R")`. ggplot2, hexSticker,
## showtext, png, ggfx and chromatographR are only needed here, so none of
## them are package dependencies.
##
## The committed logo was finished by hand in an image editor: the binary
## backdrop is drawn here rather than pasted in, and its digits are random, so
## this reproduces the design but not the exact pixels.

library(ggplot2)
library(hexSticker)
library(chromatographR)

outfile <- "dev/figures/logo_auto.png"
glyph_size <- 2.6
## blur sd in px at the sticker's 518 x 599, per layer; 0 leaves a layer alone.
## The digits are off because ggfx and `geom_subview()` do not get along on
## that layer -- see the note in the glow section.
glow_sigma <- c(trace = 4, digits = 0)
glow_strength <- .5  # halo alpha, as a fraction of its layer's

## `sticker()` loads hexSticker's bundled url font, which switches text
## rendering to showtext as a side effect. showtext assumes 96 dpi while the
## sticker is written at 300, so text comes out ~3x smaller than its nominal
## size. The sticker is assembled by hand below (it needs two subviews, and
## `sticker()` takes only one), so ask for the same treatment explicitly --
## otherwise `size = 15` renders as a title three times too big.
showtext::showtext_auto()
showtext::showtext_opts(dpi = 96)

## --- fonts ------------------------------------------------------------------

## `family = "mono"` leaves the digit font up to showtext's own mono face,
## which draws a dotted zero and a footed one. The digits in the committed
## logo are a plain grotesque -- oval zero, footless one -- so name the family
## explicitly instead, which also pins it across machines. To use a different
## font, put it first in `digit_fonts`: any family listed by
## `sysfonts::font_files()` works, and `sysfonts::font_add("myfont",
## "/path/to/myfont.ttf")` registers one that is not. Candidates worth trying:
## "Helvetica" or "Arial" (closest to the original), "Menlo", "Monaco" or
## "Andale Mono" (dotted or slashed zero, denser), "Courier New" (thin,
## serifed digits), "Krungthep" or "Silom" (heavy and much brighter),
## "Optima" (light and airy). `inst/logo_fonts.R` renders a magnified patch
## of the backdrop in each of a list of fonts, which is an easier way to
## choose than re-rendering the whole sticker.
digit_fonts <- c("Helvetica", "Arial", "DejaVu Sans", "Andale Mono")

register_font <- function(families) {
  available <- sysfonts::font_files()
  for (family in families) {
    file <- available$file[available$family == family &
                             available$face == "Regular"]
    if (length(file)) {
      sysfonts::font_add(family, file[1])
      return(family)
    }
  }
  ## nothing matched; fall back to the device default so the script still runs
  "mono"
}

digit_font <- register_font(digit_fonts)

## --- glow -------------------------------------------------------------------

## A halo behind the green content. The committed logo does not actually have
## one -- a slice across its trace goes from grey to full strength in a single
## antialiased pixel, and nothing in it is brighter than #66FF66 -- so this is
## an addition rather than a reproduction.
##
## ggfx blurs a layer as it is drawn, which beats blurring the finished png in
## two ways: each layer gets its own radius, and the halo goes *behind* its
## layer, so the strokes keep their exact colour. Screening a halo over a core
## that is already at full green raises only its red and blue, which
## desaturates it towards white.
##
## `with_outer_glow()` ignores any alpha in `colour`, so the way to dial a halo
## down is to derive it from a fainter copy of the layer: the halo tracks the
## donor's alpha linearly. The donor's own dim core is then covered by the
## crisp layer drawn on top of it.
##
## This works on the trace but not on the digits, which is why
## `glow_sigma[["digits"]]` is 0. Glowing them inside a `geom_subview()` turns
## the backdrop into a flat green wash with the digits sitting on top of it,
## and the wash is the same at sigma .2 as at 1.5 -- so it is not the halos of
## neighbouring glyphs merging, which was the obvious guess given that they
## are 4 px glyphs on a 5 px pitch. The same layer at the same density glows
## correctly when its plot is rendered on its own rather than as a subview
## (mean alpha .090 -> .128, where the subview gives a near-solid .47), and a
## sparse grid of glyphs glows correctly even as a subview. Blurring the
## finished png with magick does not have the problem, if the digits ever need
## a halo: see the version of this script in git history before this comment
## was written.
halo <- function(donor, sigma) {
  if (sigma <= 0) return(NULL)   # `+ NULL` is a no-op, so this drops the layer
  ggfx::with_outer_glow(donor, colour = "#66FF66", sigma = sigma, expand = 1)
}

## --- chromatogram ----------------------------------------------------------

## the 200 nm trace of the first goldenrod chromatogram, over the same
## 13-15 minute window as the chromatographR sticker. The 1.5x makes the peaks
## fill the frame under the `ylim()` the original was drawn with.
data(Sa_pr)

chrom <- reshape_chroms(Sa_pr, idx = 1, lambdas = 200)
chrom <- chrom[chrom$rt >= 12.99 & chrom$rt <= 15.09, ]

trace <- ggplot(chrom, aes(x = rt, y = absorbance * 1.5)) +
  halo(geom_line(color = "#66FF66", linewidth = .6, alpha = glow_strength),
       glow_sigma[["trace"]]) +
  geom_line(color = "#66FF66", linewidth = .6) +
  ylim(0, 600) +
  theme_dark() + theme_transparent() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank())

## --- binary backdrop -------------------------------------------------------

## a grid of randomly lit 0s and 1s, cropped to the hexagon so it does not
## spill past the border. Coordinates run 0-1 in both directions and the
## subview is placed over the whole hexagon, so the hexagon is the unit
## hexagon: |x| <= min(1, 2 * (1 - |y|)) about the center.
set.seed(1)
ncols <- 100
nrows <- 50

digits <- expand.grid(col = seq_len(ncols), row = seq_len(nrows))
digits$x <- (digits$col - .5)/ncols
digits$y <- (digits$row - .5)/nrows
inside <- abs(digits$x - .5)/.5 <= pmin(1, 2*(1 - abs(digits$y - .5)/.5))
digits <- digits[inside, ]
digits$char <- sample(c("0", "1"), nrow(digits), replace = TRUE)

## The backdrop in the original is not uniform: it fades out along a diagonal,
## running at full strength across the bottom-left and gone by the top-right
## corner, which is what keeps the title on plain grey. Measured off the
## committed logo, brightness is flat until .67x + .70y reaches ~.55 and hits
## zero at ~.85, so scale each glyph's alpha by that ramp.
fade <- pmin(1, pmax(0, (.85 - (.67*digits$x + .70*digits$y))/.3))
## Keep the per-glyph jitter narrow. A wide range (.1-.9) reads as blotchy
## rather than textured: measured over a patch where `fade` is flat, it gives
## a block-level CV of .34 against .24 in the committed logo. Rendering itself
## is even -- a grid of identical glyphs at constant alpha measures CV .04 --
## so the mottling is this line, not the rasteriser.
##
## The range sits high because the committed logo's digits are brighter than a
## .45-.85 ramp gives. Over a band across the lower left, where `fade` is
## flat, its lit pixels average .37 in green-minus-grey; that ramp gave .23
## spread over a third more pixels, i.e. the same amount of light smeared
## thinner, which reads as grey rather than green. Peak alpha is what closes
## the gap.
digits$alpha <- runif(nrow(digits), .8, 1) * fade
digits <- digits[digits$alpha > .02, ]

backdrop <- ggplot(digits, aes(x = x, y = y, label = char, alpha = alpha)) +
  ## the donor keeps the diagonal fade, scaled down, so the halo fades out
  ## with the digits instead of hazing the top right corner evenly
  halo(geom_text(aes(alpha = alpha * glow_strength), color = "#66FF66",
                 family = digit_font, size = glyph_size),
       glow_sigma[["digits"]]) +
  geom_text(color = "#66FF66", family = digit_font, size = glyph_size) +
  scale_alpha_identity() +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void() + theme_transparent()

## --- assembly --------------------------------------------------------------

## this is `hexSticker::sticker()` unrolled, with the backdrop slipped in
## underneath the chromatogram. The hexagon spans sqrt(3) x 2 in these
## coordinates, so those are the backdrop's width and height.
s <- ggplot() +
  geom_hexagon(size = 1.2, fill = "gray26", color = NA) +
  geom_subview(subview = backdrop, x = 1, y = 1, width = sqrt(3), height = 2) +
  geom_subview(subview = trace, x = 1, y = .9, width = 2, height = 1) +
  geom_hexagon(size = 1.2, fill = NA, color = "black") +
  geom_pkgname("chromConverter", x = 1, y = 1.45, color = "#66FF66",
               family = "mono", size = 15) +
  theme_sticker(size = 1.2)

save_sticker(outfile, s)

## The chromatogram panel is wider than the hexagon (`width = 2` against the
## hexagon's sqrt(3)), so its lowest grid line pokes out past the bottom two
## corners. Clip it away using the hexagon's own alpha channel, which keeps the
## border intact, and trims the glow where it spills past the border too.
mask_file <- tempfile(fileext = ".png")
save_sticker(mask_file,
             ggplot() + geom_hexagon(size = 1.2, fill = "black", color = "black") +
               theme_sticker(size = 1.2))

img <- png::readPNG(outfile)
img[, , 4] <- img[, , 4] * png::readPNG(mask_file)[, , 4]
png::writePNG(img, outfile)
