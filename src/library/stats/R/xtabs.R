#  File src/library/stats/R/xtabs.R
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

xtabs <- function(formula = ~., data = parent.frame(), subset,
		  na.action, exclude = c(NA, NaN), drop.unused.levels = FALSE)
{
    if(missing(formula) && missing(data))
        stop("must supply either 'formula' or 'data'")
    if(!missing(formula)){
        ## We need to coerce the formula argument now, but model.frame
        ## will coerce the original version later.
        formula <- as.formula(formula)
        if(!inherits(formula, "formula"))
            stop("'formula' missing or incorrect")
    }
    if(any(attr(terms(formula, data=data), "order") > 1))
	stop("interactions are not allowed")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)
    m$... <- m$exclude <- m$drop.unused.levels <- NULL
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    if(length(formula) == 2) {
	by <- mf
	y <- NULL
    }
    else {
	i <- attr(attr(mf, "terms"), "response")
	by <- mf[-i]
	y <- mf[[i]]
    }
    by <- lapply(by, function(u) {
	if(!is.factor(u)) u <- factor(u, exclude = exclude)
	u[ , drop = drop.unused.levels]
    })
    x <-
	if(is.null(y))
	    do.call("table", by)
	else if(NCOL(y) == 1)
	    tapply(y, by, sum)
	else {
	    z <- lapply(as.data.frame(y), tapply, by, sum)
	    array(unlist(z),
		  dim = c(dim(z[[1L]]), length(z)),
		  dimnames = c(dimnames(z[[1L]]), list(names(z))))
	}
    x[is.na(x)] <- 0
    class(x) <- c("xtabs", "table")
    attr(x, "call") <- match.call()
    x
}

print.xtabs <- function(x, ...)
{
    ox <- x
    attr(x, "call") <- NULL
    print.table(x, ...)
    invisible(ox)
}
