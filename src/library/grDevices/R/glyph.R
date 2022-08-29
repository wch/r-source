
## Generate object containing information about typeset glyphs

glyphInfo <- function(glyph, font, index, x, y) {
    glyph <- as.character(glyph)
    if (!all(sapply(font, inherits, "RFont"))) stop("Invalid font(s)")
    index <- as.integer(index)
    x <- as.numeric(x)
    y <- as.numeric(y)
    info <- data.frame(glyph, I(font), index, x, y)
    if (nrow(info) < 1)
        stop("Invalid glyph info")
    class(info) <- c("RGlyphInfo", "data.frame")
    info
}


