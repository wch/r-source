median <- function(x, na.rm=FALSE) UseMethod("median")

median.default <- function(x, na.rm = FALSE)
{
    if(is.factor(x) || mode(x) != "numeric") stop("need numeric data")
    if(na.rm) x <- x[!is.na(x)] else if(any(is.na(x))) return(NA)
    n <- length(x)
    if (n == 0) return(NA)
    half <- (n + 1)/2
    if(n %% 2 == 1) sort(x, partial = half)[half]
    else mean(sort(x, partial = c(half, half + 1))[c(half, half + 1)])
}
