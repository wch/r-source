print.anova <- function(x, digits = options("digits"),  ...) {
  heading <- attr(x, "heading")
  if (!is.null(heading)) cat(heading, sep = "\n")
  attr(x, "heading") <- NULL
  for (i in 1:NCOL(x)) {
    xr <- x[[i]]
    if (!is.factor(xr) && is.numeric(xr)) {
      cxr <- format(xr, digits=digits)
      cxr[is.na(xr)] <- ""
      x[[i]] <- cxr
    }
  }
  print.data.frame(x)
}
