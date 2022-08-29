
## Generate an object that is a description of a font

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

font <- function(family=NA, weight=NA, style=NA,
                 size=12,
                 file=NA, index=0) {
    if (length(family) != 1 ||
        length(weight) != 1 ||
        length(style) != 1 ||
        length(size) != 1 ||
        length(file) != 1 ||
        length(index) != 1)
        stop("Must specify exactly one font")
    if ((is.na(file) &&
         (is.na(family) || is.na(weight) || is.na(style))) ||
        is.na(size)) {
        stop("Must specify at least one font")
    }
    family <- as.character(family)
    weight <- mapWeight(weight)
    style <- mapStyle(style)
    size <- as.numeric(size)
    file <- as.character(file)
    index <- as.numeric(index)
    obj <- list(family=family, weight=weight, style=style,
                size=size,
                file=file, index=index)
    class(obj) <- c("RFont")
    obj
}
