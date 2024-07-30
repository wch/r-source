
################################################################################
## Render typeset glyphs

validDetails.glyphgrob <- function(x) {
    if (!inherits(x$glyphInfo, "RGlyphInfo"))
        stop("Invalid glyph info")
    if (!is.unit(x$x) || !is.unit(x$y))
        stop("'x' and 'y' must be units")
    ## Make sure that x and y are of length > 0
    if (length(x$x) < 1 || length(x$y) < 1)
        stop("'x' and 'y' must have length > 0")
    if (!inherits(x$hjust, "GlyphJust") || !inherits(x$vjust, "GlyphJust"))
        stop("'hjust' and 'vjust' must be glyphJust() values")
    if (length(x$hjust) != 1 || length(x$vjust) != 1)
        stop("'hjust' and 'vjust' must have length 1")
    x
}

glyphHOffset <- function(glyphInfo, hjust) {
    gx <- convertWidth(unit(glyphInfo$glyphs$x, "bigpts"), "in", valueOnly=TRUE)
    glyphWidth <- glyphInfo$width
    width <- convertWidth(unit(glyphWidth, "bigpts"), "in",
                          valueOnly=TRUE)
    names(width) <- names(glyphWidth)
    hAnchor <- glyphInfo$hAnchor
    if (is.numeric(hjust)) {
        justName <- names(hjust)
        if (is.null(justName)) {
            gx - convertWidth(unit(hAnchor["left"], "bigpts"), "in",
                              valueOnly=TRUE) -
                hjust*width[1]
        } else {
            if (!justName %in% names(width)) {
                warning("Unknown width; using first width")
                gx - convertWidth(unit(hAnchor["left"], "bigpts"), "in",
                                  valueOnly=TRUE) -
                    hjust*width[1]
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
            gx - convertWidth(unit(hAnchor["left"], "bigpts"), "in",
                              valueOnly=TRUE)
        } else {
            gx - convertWidth(unit(hAnchor[hjust], "bigpts"), "in",
                              valueOnly=TRUE)
        }
    }
}

glyphHJust <- function(x, glyphInfo, hjust) {
    x <- convertX(x, "in", valueOnly=TRUE)
    x + glyphHOffset(glyphInfo, hjust)
}

glyphVOffset <- function(glyphInfo, vjust) {
    gy <- convertHeight(unit(glyphInfo$glyphs$y, "bigpts"), "in",
                        valueOnly=TRUE)
    glyphHeight <- glyphInfo$height
    height <- convertHeight(unit(glyphHeight, "bigpts"), "in",
                            valueOnly=TRUE)
    names(height) <- names(glyphHeight)
    vAnchor <- glyphInfo$vAnchor
    if (is.numeric(vjust)) {
        justName <- names(vjust)
        if (is.null(justName)) {
            gy - convertHeight(unit(vAnchor["bottom"], "bigpts"), "in",
                               valueOnly=TRUE) -
                vjust*height[1]
        } else {
            if (!justName %in% names(height)) {
                warning("Unknown height; using first height")
                gy - convertHeight(unit(vAnchor["bottom"], "bigpts"), "in",
                                   valueOnly=TRUE) -
                    vjust*height[1]
            } else {
                anchor <- glyphHeightBottom(glyphHeight, justName)
                gy - convertHeight(unit(vAnchor[anchor], "bigpts"), "in",
                                   valueOnly=TRUE) -
                    vjust*height[justName]
            }
        }
    } else {
        if (!vjust %in% names(vAnchor)) {
            warning("Unknown anchor; using bottom justification")
            gy - convertHeight(unit(vAnchor["bottom"], "bigpts"), "in",
                               valueOnly=TRUE)
        } else {
            gy - convertHeight(unit(vAnchor[vjust], "bigpts"), "in",
                               valueOnly=TRUE)
        }
    }
}

glyphVJust <- function(y, glyphInfo, vjust) {
    y <- convertY(y, "in", valueOnly=TRUE)
    y + glyphVOffset(glyphInfo, vjust)
}

drawDetails.glyphgrob <- function(x, recording=TRUE) {
    ## Calculate runs of glyphs
    glyph_run_cols <- intersect(
      c("font", "size", "rot", "colour"),
      names(x$glyphInfo$glyphs)
    )
    fontstring <- unlist(do.call(paste,
                                 c(x$glyphInfo$glyphs[glyph_run_cols],
                                   list(sep=":"))))
    runs <- rle(fontstring)
    ## Calculate final glyph positions
    gx <- unit(glyphHJust(x$x, x$glyphInfo, x$hjust), "in")
    gy <- unit(glyphVJust(x$y, x$glyphInfo, x$vjust), "in")
    ## Replace NA colours with current gp$col
    naCol <- is.na(x$glyphInfo$glyphs$colour)
    if (any(naCol))
        x$glyphInfo$glyphs$colour[naCol] <- get.gpar("col")$col[1]
    ## Call dev->glyph() for each run of glyphs
    grid.Call.graphics(C_glyph,
                       as.integer(runs$lengths),
                       x$glyphInfo, gx, gy)
}

glyphRect <- function(x) {
    gx <- glyphHJust(x$x, x$glyphInfo, x$hjust)
    gy <- glyphVJust(x$y, x$glyphInfo, x$vjust)
    w <- convertWidth(unit(x$glyphInfo$width[1], "bigpts"),
                      "in", valueOnly=TRUE)
    h <- convertHeight(unit(x$glyphInfo$height[1], "bigpts"),
                       "in", valueOnly=TRUE)
    left <- min(gx)
    bottom <- min(gy)
    rectGrob(left, bottom, w, h, default.units="in", just=c("left", "bottom"))
}

xDetails.glyphgrob <- function(x, theta) {
    xDetails(glyphRect(x), theta)
}

yDetails.glyphgrob <- function(x, theta) {
    yDetails(glyphRect(x), theta)
}

widthDetails.glyphgrob <- function(x) {
    widthDetails(glyphRect(x))
}

heightDetails.glyphgrob <- function(x) {
    heightDetails(glyphRect(x))
}

xDetails.glyphgrob <- function(x, theta) {
    gx <- glyphHJust(x$x, x$glyphInfo, x$hjust)
    gy <- glyphVJust(x$y, x$glyphInfo, x$vjust)
    w <- convertWidth(unit(x$glyphInfo$width[1], "bigpts"),
                      "in", valueOnly=TRUE)
    h <- convertHeight(unit(x$glyphInfo$height[1], "bigpts"),
                       "in", valueOnly=TRUE)
    left <- min(gx)
    bottom <- min(gy)
    xDetails(rectGrob(left, bottom, w, h, default.units="in",
                      just=c("left", "bottom")),
             theta)
}

grobPoints.glyphgrob <- function(x, closed=TRUE, ...) {
    if (closed) {
        gx <- glyphHJust(x$x, x$glyphInfo, x$hjust)
        gy <- glyphVJust(x$y, x$glyphInfo, x$vjust)
        w <- convertWidth(unit(x$glyphInfo$width[1], "bigpts"),
                          "in", valueOnly=TRUE)
        h <- convertHeight(unit(x$glyphInfo$height[1], "bigpts"),
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

glyphGrob <- function(glyphInfo,
                      x=.5, y=.5, default.units="npc",
                      hjust="centre", vjust="centre",
                      gp=gpar(), vp=NULL, name=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    grob(glyphInfo=glyphInfo, x=x, y=y,
         hjust=glyphJust(hjust), vjust=glyphJust(vjust),
         gp=gp, vp=vp, name=name,
         cl="glyphgrob")    
}

grid.glyph <- function(...) {
    grid.draw(glyphGrob(...))
}
