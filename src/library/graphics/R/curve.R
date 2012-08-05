#  File src/library/graphics/R/curve.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

curve <- function(expr, from = NULL, to = NULL, n = 101, add = FALSE,
                  type = "l", xname = "x", xlab = xname,
                  ylab = NULL, log = NULL, xlim = NULL, ...)
{
    sexpr <- substitute(expr)
    if (is.name(sexpr)) {
        ## beter than parse() !
        expr <- call(as.character(sexpr), as.name(xname))
    } else {
	if ( !( (is.call(sexpr) || is.expression(sexpr)) &&
              xname %in% all.vars(sexpr) ))
	    stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'", xname), domain = NA)
	expr <- sexpr
    }
    if (dev.cur() == 1L && !identical(add, FALSE)) {
        warning("'add' will be ignored as there is no existing plot")
        add <- FALSE
    }
    addF <- identical(add, FALSE)
    if (is.null(ylab)) ylab <- deparse(expr)
    if (is.null(from) || is.null(to)) {
        xl <- if (!is.null(xlim)) xlim
        else if (!addF) {
            ## determine xlim of current plot.
            pu <- par("usr")[1L:2L]
            if (par("xaxs") == "r") pu <- extendrange(pu, f = -1/27)
            if (par("xlog")) 10^pu else pu
       } else c(0, 1) # was c(1/27, 26/27) in R < 2.14.0
        if (is.null(from)) from <- xl[1L]
        if (is.null(to)) to <- xl[2L]
    }
    lg <- if (length(log)) log else if (!addF && par("xlog")) "x" else ""
    if (length(lg) == 0) lg <- ""
    if (grepl("x", lg, fixed = TRUE)) {
        if (from <= 0 || to <= 0)
            stop("'from' and 'to' must be > 0 with log=\"x\"")
        x <- exp(seq.int(log(from), log(to), length.out = n))
    } else x <- seq.int(from, to, length.out = n)
    ll <- list(x = x); names(ll) <- xname
    y <- eval(expr, envir = ll, enclos = parent.frame())
    if (length(y) != length(x))
        stop("'expr' did not evaluate to an object of length 'n'")
    if (isTRUE(add))
	lines(x = x, y = y, type = type, ...)
    else
        plot(x = x, y = y, type = type, xlab = xlab, ylab = ylab,
             xlim = xlim, log = lg, ...)
    invisible(list(x = x, y = y))
}
