curve <- function(expr, from, to, n=101, add=FALSE, type="l",
                  ylab = NULL, ...)
{
    sexpr <- substitute(expr)
    if(is.name(sexpr)) {
        fcall <- paste(sexpr, "(x)")
        expr <- parse(text=fcall)
	if(is.null(ylab)) ylab <- fcall
    } else {
        if(!(is.call(sexpr) && match("x", all.vars(sexpr), nomatch=0)))
            stop("'expr' must be a function or an expression containing 'x'")
        expr <- sexpr
        if(is.null(ylab)) ylab <- deparse(sexpr)
    }
    lims <- delay(par("usr"))
    if(missing(from)) from <- lims[1]
    if(missing(to)) to <- lims[2]
    x <- seq(from,to,length=n)
    y <- eval(expr, envir=list(x = x), enclos=sys.frame(sys.parent(1)))
    if(add)
	lines(x, y, ...)
    else
	plot(x, y, type="l", ylab = ylab, ...)
}
