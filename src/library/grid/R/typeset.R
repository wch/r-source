
################################################################################
## Typeset a span

validDetails.typesetgrob <- function(x) {
    if (!inherits(x$span, "RTextSpan"))
        stop("Invalid span for typesetting")
    if (!is.unit(x$x) || !is.unit(x$y))
        stop("'x' and 'y' must be units")
    ## Make sure that x and y are of length > 0
    if (length(x$x) < 1 || length(x$y) < 1)
        stop("'x' and 'y' must have length > 0")
    if (length(x$width) != 1)
        stop("'width' must have length 1")
    x
}

drawDetails.typesetgrob <- function(x, recording=TRUE) {
    ## TODO:  fill in any NAs in span with 'gp' values
    grid.Call.graphics(C_typeset, x$span, x$x, x$y, x$width)
}

## NOTE that 'gp' settings only use the FIRST value
## as defaults for the span
typesetGrob <- function(span,
                        x=.5, y=.5, width=NA, default.units="npc",
                        gp=gpar(), vp=NULL, name=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (!is.unit(width))
        width <- unit(width, default.units)
    grob(span=span, x=x, y=y, width=width, gp=gp, vp=vp, name=name,
         cl="typesetgrob")
}

grid.typeset <- function(...) {
    grid.draw(typesetGrob(...))
}

################################################################################
## Render typeset glyphs

validDetails.glyphgrob <- function(x) {
    if (!inherits(x$glyph, "RGlyphInfo"))
        stop("Invalid glyph info")
    if (!is.unit(x$x) || !is.unit(x$y))
        stop("'x' and 'y' must be units")
    ## Make sure that x and y are of length > 0
    if (length(x$x) < 1 || length(x$y) < 1)
        stop("'x' and 'y' must have length > 0")
    x
}

drawDetails.glyphgrob <- function(x, recording=TRUE) {
    ## Calculate runs of glyphs
    fontdf <- do.call(rbind, lapply(x$glyph$font,
                                    function(x) do.call(data.frame, x)))
    fontstring <- unlist(do.call(paste,
                                 c(fontdf,
                                   list(sep=":"))))
    runs <- rle(fontstring)
    x$glyph$x <- unit(x$glyph$x, "bigpts")
    x$glyph$y <- unit(x$glyph$y, "bigpts")
    grid.Call.graphics(C_glyph,
                       as.integer(runs$lengths),
                       x$glyph, x$x, x$y)
}

glyphGrob <- function(glyph,
                      x=.5, y=.5, default.units="npc",
                      gp=gpar(), vp=NULL, name=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    grob(glyph=glyph, x=x, y=y, gp=gp, vp=vp, name=name,
         cl="glyphgrob")    
}

grid.glyph <- function(...) {
    grid.draw(glyphGrob(...))
}
