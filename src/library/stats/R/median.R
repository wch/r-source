median <- function(x, na.rm=FALSE) UseMethod("median")

median.default <- function(x, na.rm = FALSE)
{
    if(is.factor(x) || mode(x) != "numeric") stop("need numeric data")
    if(na.rm) x <- x[!is.na(x)] else if(any(is.na(x))) return(x[FALSE][NA])
    n <- length(x)
    if (n == 0L) return(x[FALSE][NA])
    half <- (n + 1L) %/% 2L
    if(n %% 2L == 1L) sort(x, partial = half)[half]
    else mean(sort(x, partial = half + 0L:1L)[half + 0L:1L])
}
