
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
    if (length(x$hjust) != 1 || length(x$vjust) != 1)
        stop("'hjust' and 'vjust' muat have length 1")
    x
}

glyphHOffset <- function(glyph, hjust) {
    gx <- convertWidth(unit(glyph$x, "bigpts"), "in", valueOnly=TRUE)
    width <- convertWidth(unit(attr(glyph, "width"), "bigpts"), "in",
                          valueOnly=TRUE)
    hAnchor <- attr(glyph, "hAnchor")
    if (is.numeric(hjust)) {
        justName <- names(hjust)
        if (is.null(justName)) {
            gx - hjust*width[1]
        } else {
            if (!justName %in% names(width)) {
                warning("Unknown width; using first width")
                gx - hjust*width[1]
            } else {
                gx - hjust*width[justName]
            }
        }
    } else {
        if (!hjust %in% names(hAnchor)) {
            warning("Unknown anchor; using left justification")
            gx
        } else {
            gx - convertWidth(unit(hAnchor[hjust], "bigpts"), "in",
                              valueOnly=TRUE)
        }
    }
}

glyphHJust <- function(x, glyph, hjust) {
    x <- convertX(x, "in", valueOnly=TRUE)
    x + glyphHOffset(glyph, hjust)
}

glyphVOffset <- function(glyph, vjust) {
    gy <- convertHeight(unit(glyph$y, "bigpts"), "in", valueOnly=TRUE)
    height <- convertHeight(unit(attr(glyph, "height"), "bigpts"), "in",
                            valueOnly=TRUE)
    vAnchor <- attr(glyph, "vAnchor")
    if (is.numeric(vjust)) {
        justName <- names(vjust)
        if (is.null(justName)) {
            gy - vjust*height[1]
        } else {
            if (!justName %in% names(height)) {
                warning("Unknown height; using first height")
                gy - vjust*height[1]
            } else {
                gy - vjust*height[justName]
            }
        }
    } else {
        if (!vjust %in% names(vAnchor)) {
            warning("Unknown anchor; using bottom justification")
            gy
        } else {
            gy - convertHeight(unit(vAnchor[vjust], "bigpts"), "in",
                               valueOnly=TRUE)
        }
    }
}

glyphVJust <- function(y, glyph, vjust) {
    y <- convertY(y, "in", valueOnly=TRUE)
    y + glyphVOffset(glyph, vjust)
}

drawDetails.glyphgrob <- function(x, recording=TRUE) {
    ## Calculate runs of glyphs
    fontstring <- unlist(do.call(paste,
                                 c(x$glyph[c("family", "weight", "style",
                                             "file", "index", "size")],
                                   list(sep=":"))))
    runs <- rle(fontstring)
    ## Calculate final glyph positions
    gx <- glyphHJust(x$x, x$glyph, x$hjust)
    gy <- glyphVJust(x$y, x$glyph, x$vjust)
    ## Call dev->glyph() for each run of glyphs
    grid.Call.graphics(C_glyph,
                       as.integer(runs$lengths),
                       x$glyph, gx, gy)
}

just <- function(x, ..., class) {
    UseMethod("just")
}
just.GridJust <- function(x, ...) {
    x
}
just.character <- function(x, ...) {
    just <- x
    class(just) <- "GridJust"
    just
}
just.numeric <- function(x, which=NULL, ...) {
    just <- x
    if (is.null(which)) {
        which <- names(x)
    }
    names(just) <- which
    class(just) <- "GridJust"
    just
}

glyphGrob <- function(glyph,
                      x=.5, y=.5, default.units="npc",
                      hjust="left", vjust="bottom",
                      gp=gpar(), vp=NULL, name=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    grob(glyph=glyph, x=x, y=y, hjust=just(hjust), vjust=just(vjust),
         gp=gp, vp=vp, name=name,
         cl="glyphgrob")    
}

grid.glyph <- function(...) {
    grid.draw(glyphGrob(...))
}
