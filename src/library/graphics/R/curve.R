#  File src/library/graphics/R/curve.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

curve <- function(expr, from, to, n=101, add=FALSE, type="l",
		  ylab=NULL, log=NULL, xlim=NULL, ...)
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
    if (is.null(xlim)) delayedAssign("lims", {pu <- par("usr")[1:2]
                                 if(par("xlog")) 10^pu else pu})
        else lims <- xlim
    if(missing(from)) from <- lims[1]
    if(missing(to))     to <- lims[2]
    lg <-
        if(length(log)) log
        else paste(if(add && par("xlog"))"x",
                   if(add && par("ylog"))"y", sep="")
    if(length(lg) == 0) lg <- ""
    x <-
	if(lg != "" && "x" %in% strsplit(lg, NULL)[[1]]) {
	    ## unneeded now: rm(list="log",envir=sys.frame(1))# else: warning
	    if(any(c(from,to) <= 0))
		stop("'from' and 'to' must be > 0 with log=\"x\"")
	    exp(seq.int(log(from), log(to), length.out=n))
	} else seq.int(from, to, length.out=n)
    y <- eval(expr, envir=list(x = x), enclos=parent.frame())
    if(add)
	lines(x, y, type=type, ...)
    else
	plot(x, y, type=type, ylab = ylab, xlim = xlim, log=lg, ...)
}
