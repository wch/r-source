print.anova <- function(x, digits = max(.Options$digits - 2, 3), ...)
{
  heading <- attr(x, "heading")
  if (!is.null(heading)) cat(heading, sep = "\n")
  attr(x, "heading") <- NULL
  nn <- names(x)
  for (i in 1:NCOL(x)) {
    xr <- x[[i]]
    if (substr(nn[i],1,2) == "Pr") {
      x[[i]] <- format.pval(xr, digits = max(1, min(5, digits - 1)), na="")
    } else if (!is.factor(xr) && is.numeric(xr)) {
      cxr <- format(zapsmall(xr, digits=digits), digits=digits)
      cxr[is.na(xr)] <- ""
      x[[i]] <- cxr
    }
  }
  print.data.frame(x)
}
