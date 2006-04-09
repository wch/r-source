as.pairlist <- function(x) .Internal(as.vector(x, "pairlist"))
pairlist <- function(...) as.pairlist(list(...))
## This is now .Primitive:
##is.pairlist <- function(x) typeof(x) == "pairlist"
