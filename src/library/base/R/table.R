table <- function (x, ..., exclude = c(NA, NaN)) {
  if (nargs() == 0) 
    stop("no arguments")
  bin <- 0
  lens <- NULL
  dims <- integer(0)
  pd <- 1
  dn <- NULL
  args <- if (nargs() == 1 && is.list(x)) x else list(x, ...)
  for (a in args) {
    if (is.null(lens)) lens <- length(a)
    else if (length(a) != lens) 
      stop("all arguments must have the same length")
    if (is.factor(a))
      cat <- a
    else
      cat <- factor(a, exclude = exclude)
    nl <- length(l <- levels(cat))
    dims <- c(dims, nl)
    dn <- c(dn, list(l))
    ## requiring   all(unique(as.integer(cat)) == 1:nlevels(cat))  :
    bin <- bin + pd * (as.integer(cat) - 1)
    pd <- pd * nl
  }
  bin <- bin[!is.na(bin)]
  array(tabulate(bin + 1, pd), dims, dimnames = dn)
}
