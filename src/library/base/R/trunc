trunc <- function(x, ...) UseMethod("trunc")

trunc.default <- function(x) {
  a <- attributes(x)
  x <- ifelse(x < 0, ceiling(x), floor(x))
  attributes(x) <- a
  x
}
