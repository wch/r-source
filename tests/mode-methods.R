
##-- This REALLY does  NOT yet work in R (0.64.0, April 8, 1999) !
##-- just alwas calls	abc.default(..)	 ...

abc <- function(x, ...) {
    if (is.null(class(x))) class(x) <- data.class(x)
    cat("abc: Before dispatching; x="); str(x)
    ##fails:
    UseMethod("abc")
    ##fails in all cases: NextMethod("abc",x)
}


abc.default <- function(x, ...) sys.call()

"abc.(" <- function(x)
    cat("'(' method of abc:", deparse(sys.call(sys.parent())),"\n")

abc.expression <- function(x)
    cat("'expression' method of abc:", deparse(sys.call(sys.parent())),"\n")


abc(1)
e0 <- expression((x))
e1 <- expression(sin(x))
abc(e0)
abc(e1)
abc(e0[[1]])
abc(e1[[1]])
