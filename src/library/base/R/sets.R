union <- function(x, y) unique(c(x, y))

intersect <- function(x, y) unique(y[match(x, y, 0)])

setdiff <- function(x, y)
{
    if(length(x) == 0 || length(y) == 0) unique(x)
    else unique(x[match(x, y, 0) == 0])
}

is.element <- function(el, set) match(el, set, 0) > 0
