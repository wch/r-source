union <- function(x, y) unique(c(as.vector(x), as.vector(y)))

intersect <- function(x, y)
{
    y <- as.vector(y)
    unique(y[match(as.vector(x), y, 0)])
}

setdiff <- function(x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    unique(if(length(x) || length(y)) x[match(x, y, 0) == 0] else x)
}
## Faster versions, see R-devel, Jan.4-6, 2000;  optimize later...
setequal <- function(x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    all(c(match(x, y, 0) > 0, match(y, x, 0) > 0))
}

##  same as %in% ( ./match.R ) but different arg names:
is.element <- function(el, set) match(el, set, 0) > 0
