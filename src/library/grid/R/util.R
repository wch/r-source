
is.odd <- function(x) {
  x %% 2
}

is.even <- function(x) {
  !is.odd(x)
}

grid.pretty <- function(range) {
  if (!is.numeric(range))
    stop("range must be numeric")
  .Call("L_pretty", range, PACKAGE="grid")
}


