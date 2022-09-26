
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
                      family, weight, style, file, index, size, 
                      width, height, hAnchor, vAnchor) {
    id <- as.integer(id)
    x <- as.numeric(x)
    y <- as.numeric(y)
    ## Check font
    family <- as.character(family)
    weight <- mapWeight(weight)
    style <- mapStyle(style)
    file <- as.character(file)
    index <- as.integer(index)
    size <- as.numeric(size)
    ## Check width/height
    if (!(is.numeric(width) && is.numeric(height) &&
          length(width) && length(height)))
        stop("Width and height must be numeric (and not zero-length)")
    ## Check anchors
    if (missing(hAnchor))
        hAnchor <- c(left=min(x), right=max(x), centre=(max(x) - min(x))/2,
                     center=(max(x) - min(x))/2)
    if (missing(vAnchor))
        vAnchor <- c(bottom=min(y), top=max(y), centre=(max(y) - min(y))/2,
                     center=(max(y) - min(y))/2)
    if (!is.numeric(hAnchor) || !is.numeric(vAnchor))
        stop("Anchors must be numeric")
    hNames <- names(hAnchor)
    vNames <- names(vAnchor)
    if (length(unique(hNames)) != length(hAnchor) ||
        length(unique(vNames)) != length(vAnchor))
        stop("Every anchor must have a unique name")
    if (!("left" %in% hNames && "right" %in% hNames &&
          ("centre" %in% hNames || "center" %in% hNames) &&
          "bottom" %in% vNames && "top" %in% vNames &&
          ("centre" %in% vNames || "center" %in% vNames)))
        stop('There must be anchors named "left", "right", "bottom", "top", and "centre" (or "center")')
    if (!"centre" %in% hNames) hAnchor <- c(hAnchor, centre=hAnchor["center"])
    if (!"centre" %in% vNames) vAnchor <- c(vAnchor, centre=vAnchor["center"])
    if (!"center" %in% hNames) hAnchor <- c(hAnchor, centre=hAnchor["centre"])
    if (!"center" %in% vNames) vAnchor <- c(vAnchor, centre=vAnchor["centre"])
    info <- data.frame(id, x, y,
                       family, weight, style, file, index, size)
    attr(info, "width") <- width
    attr(info, "height") <- height
    attr(info, "hAnchor") <- hAnchor
    attr(info, "vAnchor") <- vAnchor
    if (nrow(info) < 1)
        stop("Invalid glyph info")
    class(info) <- c("RGlyphInfo", "data.frame")
    info
}


