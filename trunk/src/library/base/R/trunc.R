## Commented by KH on 1999/01/30.
## trunc() should really be in the `Math' group.

##trunc <- function(x, ...) UseMethod("trunc")
##trunc.default <- function(x) {
##    a <- attributes(x)
##    x <- ifelse(x < 0, ceiling(x), floor(x))
##    attributes(x) <- a
##    x
##}
