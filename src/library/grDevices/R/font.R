
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

font <- function(family, weight, style,
                 file, index=0,
                 size=12) {
    nofam <- missing(family)
    nowt <- missing(weight)
    nostyle <- missing(style)
    nofile <- missing(file)
    if (nofile) {
        if (nofam || nowt || nostyle) {
            stop("Must specify at least one font")
        } else {
            file <- NA
        }
    } else {
        if (nofam) family <- "sans"
        if (nowt) weight <- 400
        if (nostyle) style <- "normal"
    }
    family <- as.character(family)
    weight <- mapWeight(weight)
    style <- mapStyle(style)
    size <- as.numeric(size)
    file <- as.character(file)
    index <- as.numeric(index)
    obj <- data.frame(family=family, weight=weight, style=style,
                      size=size,
                      file=file, index=index)
    class(obj) <- c("RFont", class(obj))
    obj
}
