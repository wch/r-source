
## Generate an object containing "marked up" text

## The return value should always end up being a data frame
## (with class "textspan")

## Should the text always end up as UTF-8 ?

flatten <- function(x, params) {
    UseMethod("flatten")
}

flatten.character <- function(x, params) {
    if (length(x) < 1)
        stop("Invalid span text")
    cbind(text=enc2utf8(x), params[1:length(x),])
}

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

span <- function(...,
                 family=NA,
                 weight=NA,
                 style=NA) {
    spanlist <- list(...)
    params <- data.frame(## CHAR
                         family=as.character(family),
                         ## REAL
                         weight=mapWeight(weight),
                         ## INTEGER
                         style=mapStyle(style))
    spans <- lapply(spanlist, flatten, params)
    result <- do.call(rbind, spans)
    if (nrow(result) < 1)
        stop("Invalid span")
    class(result) <- c("textspan", "data.frame")
    result
}
