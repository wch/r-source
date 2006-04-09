rle <- function(x)
{
    if (!is.vector(x))
        stop("'x' must be a vector")
    n <- length(x)
    if (n == 0)
        return(list(lengths = integer(0), values = x))
    y <- x[-1] != x[-n]
    i <- c(which(y | is.na(y)), n)
    structure(list(lengths = diff(c(0:0, i)), values = x[i]),
              class = "rle")
}

print.rle <- function(x, digits = getOption("digits"), ...)
{
    if(is.null(digits)) digits <- getOption("digits")
    cat("Run Length Encoding\n  lengths:")
    str(x$lengths)
    cat("  values :")
    str(x$values, digits = digits)
    invisible(x)
}

inverse.rle <- function(x, ...)
{
    if(is.null(le <- x$lengths) ||
       is.null(v  <- x$values) || length(le) != length(v))
        stop("invalid 'rle' structure")
    rep(v, le)
}

