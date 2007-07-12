range <- function(..., na.rm = FALSE)
    .Internal(range(..., na.rm = na.rm))

range.default <- function(..., na.rm = FALSE, finite = FALSE)
{
    x <- c(..., recursive = TRUE)
    if(is.numeric(x)) {
        if(finite) x <- x[is.finite(x)]
        else if(na.rm) x <- x[!is.na(x)]
    }
    c(min(x), max(x)) # even if x is empty from 1.5.0
}
