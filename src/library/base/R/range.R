range <- function(..., na.rm=FALSE, finite=FALSE) {
    x <- c(...)
    if(finite) x <- x[is.finite(x)]
    else if(na.rm) x <- x[!is.na(x)]
    if(length(x)) c(min(x), max(x)) else NA
}
