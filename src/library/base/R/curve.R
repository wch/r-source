curve <- function(expr, from, to, n=101, add=FALSE, type="l",
                  ylab = NULL, ...)
{
    sexpr <- substitute(expr)
    if(is.call(sexpr)) expr <- sexpr
    expr.t <- paste(deparse(expr), collapse=" ")

    if(!(lx <- length(grep("\\<x\\>", expr.t)))
       || length(grep(pat <- "^function *\\(x\\) *", expr.t))) {
        is.f <- is.function(expr)
        fun <- if(is.f) expr else eval(parse(text=expr.t))
        if(!(is.f ||is.function(fun)))
            stop("'expr' must be a function or contain 'x'")
        if(is.null(ylab))
            ylab <-
                if(is.f) paste(deparse(sexpr), "(x)")
                else if(lx) sub(pat, "", expr.t)# drop "function(x)"
                else paste(expr.t,"(x)")
    } else {
        fun <- NULL
        if(is.null(ylab)) ylab <- expr.t
    }
    lims <- delay(par("usr"))
    if(missing(from)) from <- lims[1]
    if(missing(to)) to <- lims[2]
    x <- seq(from,to,length=n)
    y <- if(is.null(fun)) eval(expr) else fun(x)
    if(add)
	lines(x, y, ...)
    else
	plot(x, y, type="l", ylab = ylab, ...)
}
