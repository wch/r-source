curve <- function(expr, from, to, n=101, add=FALSE, type="l",
		  ylab = NULL, log=NULL, ...)
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
    lg <- if(length(log)) log else ""
    x <-
	if(lg != "" && "x" %in% strsplit(lg, NULL)[[1]]) {
	    rm(list="log",envir=sys.frame(1))# else: warning
	    if(any(c(from,to)<=0))
		stop("`from' & `to' must be > 0	 with  log=\"x\"")
	    exp(seq(log(from), log(to), length=n))
	} else seq(from,to,length=n)
    y <- eval(expr, envir=list(x = x), enclos=sys.frame(sys.parent(1)))
    if(add)
	lines(x, y, type=type, ...)
    else
	plot(x, y, type=type, ylab = ylab, log=lg, ...)
}
