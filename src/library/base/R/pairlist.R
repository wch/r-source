
as.pairlist <- function(x) .Internal(as.vector(x, "pairlist"))

pairlist <- function(...) as.pairlist(list(...))
## This should really be .Primitive:
is.pairlist <- function(x) typeof(x) == "pairlist"
