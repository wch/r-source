range <- function(..., na.rm = FALSE)
    .Internal(range(..., na.rm = na.rm))

range.default <- function(..., na.rm = FALSE, finite = FALSE) {
    x <- c(..., recursive = TRUE)
    if(finite) x <- x[is.finite(x)]
    else if(na.rm) x <- x[!is.na(x)]
    if(length(x)) c(min(x), max(x)) else c(NA, NA)
}
