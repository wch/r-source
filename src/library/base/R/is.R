is.vector <- function(x, mode="any") .Internal(is.vector(x,mode))
## is.finite <- function(x) !is.na(x)

is.name <- .Alias(is.symbol) # which is Primitive
##Was is.symbol <- function(x) typeof(x)=="symbol"


