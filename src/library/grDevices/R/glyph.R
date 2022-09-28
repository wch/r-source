
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

glyphInfo <- function(id, x, y,
                      family, weight, style, size, file, index, 
                      width, height, hAnchor, vAnchor) {
    id <- as.integer(id)
    x <- as.numeric(x)
    y <- as.numeric(y)
    ## Check font
    family <- as.character(family)
    if (any(nchar(family, "bytes") > 200))
        warning("Font family longer than 200 will be truncated")
    weight <- mapWeight(weight)
    style <- mapStyle(style)
    file <- as.character(file)
    if (any(nchar(file, "bytes") > 500))
        warning("Font file longer than 500 will be truncated")
    index <- as.integer(index)
    size <- as.numeric(size)
    ## Check width/height
    if (!(is.numeric(width) && is.numeric(height) &&
          length(width) && length(height)))
        stop("Width and height must be numeric (and not zero-length)")
    ## Check anchors
    if (missing(hAnchor))
        hAnchor <- c(left=min(x), right=min(x) + width[1],
                     centre=min(x) + width[1]/2, center=min(x) + width[1]/2)
    if (missing(vAnchor))
        vAnchor <- c(bottom=min(y), top=min(y) + height[1],
                     centre=min(y) + height[1]/2, center=min(y) + height[1]/2)
    if (!is.numeric(hAnchor) || !is.numeric(vAnchor))
        stop("Anchors must be numeric")
    hNames <- names(hAnchor)
    vNames <- names(vAnchor)
    if (length(unique(hNames)) != length(hAnchor) ||
        length(unique(vNames)) != length(vAnchor))
        stop("Every anchor must have a unique name")
    if (!("left" %in% hNames && "bottom" %in% vNames))
        stop('There must be anchors named "left" and "bottom"')
    if (!"right" %in% hNames)
        hAnchor <- c(hAnchor, right=unname(hAnchor["left"]) + width[1])
    if (!"top" %in% vNames)
        vAnchor <- c(vAnchor, top=unname(vAnchor["bottom"]) + height[1])
    if (!"centre" %in% hNames)
        hAnchor <- c(hAnchor, centre=unname(hAnchor["left"]) + width[1]/2)
    if (!"centre" %in% vNames)
        vAnchor <- c(vAnchor, centre=unname(vAnchor["bottom"]) + height[1]/2)
    if (!"center" %in% hNames)
        hAnchor <- c(hAnchor, center=unname(hAnchor["centre"]))
    if (!"center" %in% vNames)
        vAnchor <- c(vAnchor, center=unname(vAnchor["centre"]))
    info <- data.frame(id, x, y,
                       family, weight, style, size, file, index)
    attr(info, "width") <- width
    attr(info, "height") <- height
    attr(info, "hAnchor") <- hAnchor
    attr(info, "vAnchor") <- vAnchor
    if (nrow(info) < 1)
        stop("Invalid glyph info")
    class(info) <- c("RGlyphInfo", "data.frame")
    info
}


