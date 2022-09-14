
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
    
    cbind(text=enc2utf8(x), params[1:min(nrow(params), length(x)),])
}

span <- function(...,
                 family=NA,
                 weight=NA,
                 style=NA,
                 size=NA,
                 file=NA,
                 index=NA) {
    spanlist <- list(...)
    params <- data.frame(family=as.character(family), ## CHAR
                         weight=mapWeight(weight), ## REAL
                         style=mapStyle(style), ## INTEGER
                         size=as.numeric(size), ## REAL
                         file=as.character(file), ## CHAR
                         index=as.integer(index)) ## INTEGER
    spans <- lapply(spanlist, flatten, params)
    result <- do.call(rbind, spans)
    if (nrow(result) < 1)
        stop("Invalid span")
    class(result) <- c("RTextSpan", "data.frame")
    result
}
