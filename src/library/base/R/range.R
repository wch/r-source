## Commented by KH on 1999/01/30.
## range() should really be in the `Summary' group.
## Code now in `summaries.R'.

##range <- function(..., na.rm=FALSE, finite=FALSE) {
##    x <- c(..., recursive=TRUE)
##    if(finite) x <- x[is.finite(x)]
##    else if(na.rm) x <- x[!is.na(x)]
##    if(length(x)) c(min(x), max(x)) else NA
##}
