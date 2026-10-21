## Contact sheet for picking the digit font in `logo.R`.
##
## Run from the package root with `source("dev/logo_fonts.R")`. Writes a
## labelled comparison to `outfile`: one patch of the binary backdrop per
## font, drawn at the same pixel scale as the sticker and then magnified, so
## the glyph shapes are readable at a size the sticker itself is too small to
## show. Pick a font and put it first in `digit_fonts` in `logo.R`. The sheet
## is a scratch artifact, so it is written to `dev/figures`, which git and the
## package build ignore, rather than `man/figures`, which ships with the package.
##
## Needs magick on top of what `logo.R` uses, and nothing here is a package
## dependency.

library(ggplot2)

outfile <- "dev/figures/logo_fonts.png"

## The fonts to compare -- edit this. "mono" is showtext's built-in mono face
## and needs no registering; everything else has to be a family that
## `sysfonts::font_files()` lists, which on this machine is most of
## /System/Library/Fonts. `unique(sysfonts::font_files()$family)` is the full
## list; it runs to a couple of hundred entries, so it is worth grepping:
##
##   ff <- sysfonts::font_files()
##   sort(unique(ff$family[grepl("Mono|Courier|Menlo|Monaco", ff$family)]))
##
## To try a font that is not installed, register it by hand first with
## `sysfonts::font_add("myfont", "/path/to/myfont.ttf")` and then name it here.
fonts <- c("mono", "Helvetica", "Arial", "Arial Narrow", "Verdana", "Tahoma",
           "Geneva", "Optima", "PT Sans", "Andale Mono", "Menlo", "Monaco",
           "Courier New", "Courier", "American Typewriter", "Skia",
           "Krungthep", "Silom")

ncol_sheet <- 3   # patches per row of the sheet
zoom <- 5         # magnification of each patch
size <- 2.9       # must match `geom_text(size = )` in logo.R

showtext::showtext_auto()
showtext::showtext_opts(dpi = 96)

## --- geometry ---------------------------------------------------------------

## The sticker is 518 x 599 px with a 100 x 50 grid of digits across it, so a
## cell is 5.18 x 11.98 px. Reproducing that pitch is what makes the patches
## true to the final render: with `expand = c(0, 0)` and no margins the panel
## fills the device, so asking for `cols * 5.18` by `rows * 11.98` px at the
## dpi showtext is using gives glyphs the same pixel size as in the sticker.
cell_w <- 518/100
cell_h <- 599/50
cols <- 26
rows <- 7

set.seed(1)
patch <- expand.grid(col = seq_len(cols), row = seq_len(rows))
patch$x <- (patch$col - .5)/cols
patch$y <- (patch$row - .5)/rows
patch$char <- sample(c("0", "1"), nrow(patch), replace = TRUE)
patch$alpha <- runif(nrow(patch), .8, 1)

## --- render -----------------------------------------------------------------

available <- sysfonts::font_files()

render_patch <- function(family) {
  if (family != "mono") {
    file <- available$file[available$family == family &
                             available$face == "Regular"]
    if (!length(file)) {
      message("skipping '", family, "': not in sysfonts::font_files()")
      return(NULL)
    }
    sysfonts::font_add(family, file[1])
  }
  p <- ggplot(patch, aes(x = x, y = y, label = char, alpha = alpha)) +
    geom_text(color = "#66FF66", family = family, size = size) +
    scale_alpha_identity() +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "gray26", color = NA),
          plot.margin = margin(0, 0, 0, 0))
  file <- tempfile(fileext = ".png")
  ggsave(file, p, width = cols*cell_w/96, height = rows*cell_h/96, dpi = 96)
  img <- magick::image_read(file)
  img <- magick::image_scale(img, magick::geometry_size_percent(zoom*100))
  ## label above the patch rather than over it, so no digits are hidden
  magick::image_append(c(
    magick::image_annotate(
      magick::image_blank(magick::image_info(img)$width, 26, "gray16"),
      family, color = "white", size = 17, location = "+6+4"),
    img), stack = TRUE)
}

patches <- lapply(fonts, render_patch)
patches <- patches[!vapply(patches, is.null, logical(1))]

## --- assemble ---------------------------------------------------------------

rows_of <- split(patches, (seq_along(patches) - 1) %/% ncol_sheet)
sheet <- magick::image_append(
  do.call(c, lapply(rows_of, function(r) {
    magick::image_append(do.call(c, r))
  })), stack = TRUE)

dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
magick::image_write(sheet, outfile)
message("wrote ", outfile)
