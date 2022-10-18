
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
    if (!inherits(x$hjust, "GridJust") || !inherits(x$vjust, "GridJust"))
        stop("'hjust' and 'vjust' must be just() values")
    if (length(x$hjust) != 1 || length(x$vjust) != 1)
        stop("'hjust' and 'vjust' must have length 1")
    x
}

glyphHOffset <- function(glyph, hjust) {
    gx <- convertWidth(unit(glyph$x, "bigpts"), "in", valueOnly=TRUE)
    glyphWidth <- attr(glyph, "width")
    width <- convertWidth(unit(glyphWidth, "bigpts"), "in",
                          valueOnly=TRUE)
    names(width) <- names(glyphWidth)
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
                anchor <- glyphWidthLeft(glyphWidth, justName)
                gx - convertWidth(unit(hAnchor[anchor], "bigpts"), "in",
                              valueOnly=TRUE) -
                    hjust*width[justName]
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
    glyphHeight <- attr(glyph, "height")
    height <- convertHeight(unit(glyphHeight, "bigpts"), "in",
                            valueOnly=TRUE)
    names(height) <- names(glyphHeight)
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
                anchor <- glyphHeightBottom(glyphHeight, justName)
                gx - converHeight(unit(vAnchor[anchor], "bigpts"), "in",
                              valueOnly=TRUE) -
                    vjust*height[justName]
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
                                             "file", "index", "size",
                                             "colour")],
                                   list(sep=":"))))
    runs <- rle(fontstring)
    ## Calculate final glyph positions
    gx <- glyphHJust(x$x, x$glyph, x$hjust)
    gy <- glyphVJust(x$y, x$glyph, x$vjust)
    ## Replace NA colours with current gp$col
    naCol <- is.na(x$glyph$colour)
    if (any(naCol))
        x$glyph$colour[naCol] <- get.gpar("col")$col[1]
    ## Call dev->glyph() for each run of glyphs
    grid.Call.graphics(C_glyph,
                       as.integer(runs$lengths),
                       x$glyph, gx, gy)
}

grobPoints.glyphgrob <- function(x, closed=TRUE, ...) {
    if (closed) {
        gx <- glyphHJust(x$x, x$glyph, x$hjust)
        gy <- glyphVJust(x$y, x$glyph, x$vjust)
        w <- convertWidth(unit(attr(x$glyph, "width")[1], "bigpts"),
                          "in", valueOnly=TRUE)
        h <- convertHeight(unit(attr(x$glyph, "height")[1], "bigpts"),
                           "in", valueOnly=TRUE)
        left <- min(gx)
        bottom <- min(gy)
        right <- left + w
        top <- bottom + h
        gridGrobCoords(list("1"=gridCoords(x=c(left, left, right, right),
                                           y=c(bottom, top, top, bottom))),
                       x$name)
    } else {
        emptyGrobCoords(x$name)
    }    
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
                      hjust="centre", vjust="centre",
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
