### Using Method Dispatch on "mode" etc :

abc <- function(x, ...) {
    if (is.null(class(x))) class(x) <- data.class(x)
    cat("abc: Before dispatching; x="); str(x)
    UseMethod("abc", x,...) ## UseMethod("abc") (as in S) fails
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
