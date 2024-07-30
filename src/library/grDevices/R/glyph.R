
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

fontStyles <- c("normal", "italic", "oblique")

mapStyle <- function(x) {
    ## NA passes through
    match(x, fontStyles)
}

invertStyle <- function(x) {
    fontStyles[x]
}

################################################################################
## glyph dimensions, anchors, and justification
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

glyphJust <- function(just, ...) {
    UseMethod("glyphJust")
}
glyphJust.GlyphJust <- function(just, ...) {
    just
}
glyphJust.character <- function(just, ...) {
    class(just) <- "GlyphJust"
    just
}
glyphJust.numeric <- function(just, which=NULL, ...) {
    if (is.null(which)) {
        which <- names(just)
    }
    names(just) <- which
    class(just) <- "GlyphJust"
    just
}

################################################################################
## glyph font
glyphFont <- function(file, index,
                      family, weight, style,
                      PSname=NA) {
    file <- as.character(file)
    nafile <- is.na(file)
    if (any(nchar(file[!nafile], "bytes") > 500))
        warning("Font file longer than 500 will be truncated")
    index <- as.integer(index)
    family <- as.character(family)
    nafamily <- is.na(family)
    if (any(nchar(family[!nafamily], "bytes") > 200))
        warning("Font family longer than 200 will be truncated")
    weight <- mapWeight(weight)
    style <- mapStyle(style)
    PSname <- as.character(PSname)
    ## Missing PSname values are "estimated"
    naPS <- is.na(PSname)
    if (any(naPS)) {
        PSbold <- ifelse(weight >= 700, "Bold", "")
        PSstyle <- ifelse(style > 1,
                          ifelse(style > 2, "Oblique", "Italic"),
                          "")
        face <- paste0(PSbold, PSstyle)
        PSname[naPS] <-
            ifelse(nchar(file[naPS]),
                   sub("([^.]+)\\.[[:alnum:]]+$", "\\1",
                       basename(file[naPS])),
                   paste0(family[naPS],
                          ifelse(nchar(PSstyle), paste0("-", PSstyle), "")))
    }
    if (any(nchar(PSname, "bytes") > 200))
        warning("PostScript font name longer than 200 will be truncated")
    ## Check that family-weight-style and file and PSname all line up
    families <- rle(paste0(family, weight, style))$lengths
    files <- rle(file)$lengths
    names <- rle(PSname)$lengths
    if (!(all(families == files) && all(files == names)))
        stop("Font information is inconsistent")
    
    font <- list(file=file, index=index,
                 family=family, weight=weight, style=style,
                 PSname=PSname)
    class(font) <- "RGlyphFont"
    font
}

print.RGlyphFont <- function(x, ...) {
    cat(paste0(x$family, " wgt: ", x$weight, " style: ", invertStyle(x$style),
               "\n  (", x$file, " [", x$index, "])\n"))
}

glyphFontList <- function(...) {
    fonts <- list(...)
    if (!length(fonts))
        stop("List must include at least one font")
    if (!all(sapply(fonts, function(x) inherits(x, "RGlyphFont"))))
        stop("Invalid glyph font")
    class(fonts) <- "RGlyphFontList"
    fonts
}

################################################################################
## glyph information
glyphInfo <- function(id, x, y, font, size,
                      fontList,
                      width, height,
                      hAnchor, vAnchor,
                      col=NA, rot=0) {
    id <- as.integer(id)
    x <- as.numeric(x)
    y <- as.numeric(y)
    ## Check font
    font <- as.integer(font)
    if (!inherits(fontList, "RGlyphFontList"))
        stop("Invalid font list")
    if (any(is.na(font)) || !all(font %in% seq_along(fontList)))
        stop("Unknown font")
    size <- as.numeric(size)
    rot <- as.numeric(rot)
    ## Check colour (allow any R colour spec)
    nacol <- is.na(col)
    if (any(!nacol)) {
        rgb <- col2rgb(col[!nacol], alpha=TRUE)
        col[!nacol] <- rgb(rgb[1,], rgb[2,], rgb[3,], rgb[4,],
                           maxColorValue=255)
    }
    ## Check width/height
    if (!inherits(width, "GlyphWidth"))
        width <- glyphWidth(width)
    if (!inherits(height, "GlyphHeight"))
        height <- glyphHeight(height)
    ## Check anchors (and provide defaults if missing)
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
    dropNA <- !(is.na(id) | is.na(x) | is.na(y) |
                ## is.na(font) already checked
                is.na(size) | is.na(rot))
    glyphs <- data.frame(id, x, y, font, size, rot)[dropNA, ]
    if (nrow(glyphs) < 1)
        stop("Invalid glyph info")
    ## Colour can be NA
    if (inherits(glyphs, "omit")) {
        glyphs$colour <- col[-attr(glyphs, "na.action")]
    } else {
        glyphs$colour <- col
    }
    ## Reorder to ensure backwards compatibility with code
    ## where rot was not yet included.
    col_order <- c("id", "x", "y", "font", "size", "colour", "rot")
    glyphs <- glyphs[, col_order, drop = FALSE]
    ## Construct final structure
    info <- list(glyphs=glyphs, fonts=fontList,
                 width=width, height=height,
                 hAnchor=hAnchor, vAnchor=vAnchor)
    class(info) <- c("RGlyphInfo")
    info
}


