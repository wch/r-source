expand.grid <- function(x, y) {
  if (!is.list(x) && !missing(y)) {
    x <- list(x = x, y = y)
  }
  if (length(x) == 1)
    return(x[[1]])
  n1 <- length(x[[1]])
  n2 <- length(x[[2]])
  cbind(rep(x[[1]], n2), rep(x[[2]], rep(n1, n2)))
}
