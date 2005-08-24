print <- function(x, ...) UseMethod("print")

##- Need '...' such that it can be called as  NextMethod("print", ...):
print.default <- function(x, digits = NULL, quote = TRUE, na.print = NULL,
                          print.gap = NULL, right = FALSE, ...)
{
    noOpt <- missing(digits) && missing(quote) && missing(na.print) &&
      missing(print.gap) && missing(right) && length(list(...)) == 0
    .Internal(print.default(x, digits, quote, na.print, print.gap, right,
                            noOpt))
}

#print.matrix <- print.default  ## back-compatibility

prmatrix <-
    function (x, rowlab = dn[[1]], collab = dn[[2]],
              quote = TRUE, right = FALSE,
              na.print = NULL, ...)
{
    x <- as.matrix(x)
    dn <- dimnames(x)
    .Internal(prmatrix(x, rowlab, collab, quote, right, na.print))
}

noquote <- function(obj) {
    ## constructor for a useful "minor" class
    if(!inherits(obj,"noquote")) class(obj) <- c(attr(obj, "class"),"noquote")
    obj
}

as.matrix.noquote <- function(x) noquote(NextMethod("as.matrix", x))
c.noquote <- function(..., recursive = FALSE) structure(NextMethod(...), class = "noquote")

"[.noquote" <- function (x, ...) {
    attr <- attributes(x)
    r <- unclass(x)[...] ## shouldn't this be NextMethod?
    attributes(r) <- c(attributes(r),
		       attr[is.na(match(names(attr),
                                        c("dim","dimnames","names")))])
    r
}

print.noquote <- function(x, ...) {
    if(!is.null(cl <- attr(x, "class"))) {
	cl <- cl[cl != "noquote"]
        attr(x, "class") <-
          (if(length(cl)>0) cl else NULL)
      }
    print(x, quote = FALSE, ...)
}

## for alias:
print.listof <- function(x, ...)
{
    nn <- names(x)
    ll <- length(x)
    if(length(nn) != ll) nn <- paste("Component", seq(ll))
    for(i in seq(length=ll)) {
	cat(nn[i], ":\n"); print(x[[i]], ...); cat("\n")
    }
    invisible(x)
}

## used for version:
print.simple.list <- function(x, ...)
    print(noquote(cbind("_"=unlist(x))), ...)

