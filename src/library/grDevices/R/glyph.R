
## Generate object containing information about typeset glyphs
## All x/y and width/height and anchors should be in "big" pts (1/72 inch)

mapCharWeight <- function(x) {
    if (is.na(x))
        x
    else 
        switch(as.character(x),
               normal=400,
               bold=700,
               as.numeric(x))
}

mapWeight <- function(x) {
    if (is.numeric(x)) {
        if (min(x, na.rm=TRUE) < 0 || max(x, na.rm=TRUE) > 1000)
            stop("Invalid span weight")
        x
    } else {
        sapply(x, mapCharWeight, USE.NAMES=FALSE)
    }   
}

mapStyle <- function(x) {
    ## NA passes through
    match(x, c("normal", "italic", "oblique"))
}

glyphWidth <- function(w, label="width", left="left") {
    if (!length(w) ||
        length(w) != length(label) ||
        length(label) != length(left))
        stop("length of arguments must match (and be greater than 0)")
    w <- as.numeric(w)
    if (any(!is.finite(w))) stop("Invalid glyph width(s)")
    names(w) <- as.character(label)
    attr(w, "anchor") <- as.character(left)
    class(w) <- "GlyphWidth"
    w
}

glyphWidthLeft <- function(w, label) {
    if (!inherits(w, "GlyphWidth")) stop("Invalid glyph width")
    if (!label %in% names(w)) {
        warning("Unknown width; using left anchor")
        return("left")
    }
    which <- match(label, names(w))
    attr(w, "anchor")[which]
}

glyphHeight <- function(h, label="height", bottom="bottom") {
    if (!length(h) ||
        length(h) != length(label) ||
        length(label) != length(bottom))
        stop("length of arguments must match (and be greater than 0)")
    h <- as.numeric(h)
    if (any(!is.finite(h))) stop("Invalid glyph height(s)")
    names(h) <- as.character(label)
    attr(h, "anchor") <- as.character(bottom)
    class(h) <- "GlyphHeight"
    h
}

glyphHeightBottom <- function(h, label) {
    if (!inherits(h, "GlyphHeight")) stop("Invalid glyph height")
    if (!label %in% names(h)) {
        warning("Unknown height; using bottom anchor")
        return("bottom")
    }
    which <- match(label, names(h))
    attr(h, "anchor")[which]
}

glyphAnchor <- function(value, label) {
    if (!length(value) ||
        length(value) != length(label))
        stop("length of arguments must match (and be greater than 0)")
    value <- as.numeric(value)
    if (any(!is.finite(value))) stop("Invalid glyph anchor")
    names(value) <- as.character(label)
    class(value) <- "GlyphAnchor"
    value
}

glyphInfo <- function(id, x, y,
                      family, weight, style, size, file, index,
                      width, height, hAnchor, vAnchor,
                      colour=NA) {
    id <- as.integer(id)
    x <- as.numeric(x)
    y <- as.numeric(y)
    ## Check font
    family <- as.character(family)
    nafamily <- is.na(family)
    if (any(nchar(family[!nafamily], "bytes") > 200))
        warning("Font family longer than 200 will be truncated")
    weight <- mapWeight(weight)
    style <- mapStyle(style)
    file <- as.character(file)
    nafile <- is.na(file)
    if (any(nchar(file[!nafile], "bytes") > 500))
        warning("Font file longer than 500 will be truncated")
    index <- as.integer(index)
    size <- as.numeric(size)
    ## Check colour (allow any R colour spec)
    nacol <- is.na(colour)
    if (any(!nacol)) {
        rgb <- col2rgb(colour[!nacol], alpha=TRUE)
        colour[!nacol] <- rgb(rgb[1,], rgb[2,], rgb[3,], rgb[4,],
                              maxColorValue=255)
    }
    ## Check width/height
    if (!inherits(width, "GlyphWidth"))
        width <- glyphWidth(width)
    if (!inherits(height, "GlyphHeight"))
        height <- glyphHeight(height)
    ## Check anchors
    if (missing(hAnchor))
        hAnchor <- glyphAnchor(c(min(x), min(x) + width[1],
                                 min(x) + width[1]/2),
                               label=c("left", "right", "centre"))
    if (missing(vAnchor))
        vAnchor <- glyphAnchor(c(min(y), min(y) + height[1],
                                 min(y) + height[1]/2),
                               label=c("bottom", "top", "centre"))
    if (!inherits(hAnchor, "GlyphAnchor"))
        hAnchor <- glyphAnchor(hAnchor, names(hAnchor))
    if (!inherits(vAnchor, "GlyphAnchor"))
        vAnchor <- glyphAnchor(vAnchor, names(vAnchor))
    hNames <- names(hAnchor)
    vNames <- names(vAnchor)
    if (!("left" %in% hNames && "bottom" %in% vNames))
        stop('There must be anchors named "left" and "bottom"')
    if (!"right" %in% hNames)
        hAnchor <- c(hAnchor,
                     right=unname(hAnchor["left"]) + unname(width[1]))
    if (!"top" %in% vNames)
        vAnchor <- c(vAnchor,
                     top=unname(vAnchor["bottom"]) + unname(height[1]))
    if (!"centre" %in% hNames)
        hAnchor <- c(hAnchor,
                     centre=unname(hAnchor["left"]) + unname(width[1]/2))
    if (!"centre" %in% vNames)
        vAnchor <- c(vAnchor,
                     centre=unname(vAnchor["bottom"]) + unname(height[1]/2))
    if (!"center" %in% hNames)
        hAnchor <- c(hAnchor, center=unname(hAnchor["centre"]))
    if (!"center" %in% vNames)
        vAnchor <- c(vAnchor, center=unname(vAnchor["centre"]))
    ## Build glyph info
    info <- na.omit(data.frame(id, x, y,
                               family, weight, style, size, file, index))
    ## Colour can be NA
    if (inherits(info, "omit")) {
        info$colour <- colour[-attr(info, "na.action")]
    } else {
        info$colour <- colour
    }
    attr(info, "width") <- width
    attr(info, "height") <- height
    attr(info, "hAnchor") <- hAnchor
    attr(info, "vAnchor") <- vAnchor
    if (nrow(info) < 1)
        stop("Invalid glyph info")
    class(info) <- c("RGlyphInfo", "data.frame")
    info
}


