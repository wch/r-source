#  File src/library/stats/R/xtabs.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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
#  https://www.R-project.org/Licenses/

xtabs <- function(formula = ~., data = parent.frame(), subset, sparse = FALSE,
		  na.action, addNA = FALSE, exclude = if(!addNA) c(NA, NaN),
		  drop.unused.levels = FALSE)
{
    if (missing(formula) && missing(data))
	stop("must supply either 'formula' or 'data'")
    if(!missing(formula)){
	## We need to coerce the formula argument now, but model.frame
	## will coerce the original version later.
	formula <- as.formula(formula)
	if (!inherits(formula, "formula"))
	    stop("'formula' missing or incorrect")
    }
    if (any(attr(terms(formula, data = data), "order") > 1))
	stop("interactions are not allowed")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)
    m$... <- m$exclude <- m$drop.unused.levels <- m$sparse <- m$addNA <- NULL
    if(addNA && missing(na.action)) m$na.action <- quote(na.pass)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    if(length(formula) == 2L) {
	by <- mf
	y <- NULL
    }
    else {
	i <- attr(attr(mf, "terms"), "response")
	by <- mf[-i]
	y <- mf[[i]]
    }
    has.exclude <- !missing(exclude)
    by <- lapply(by, function(u) {
        if(!is.factor(u)) u <- factor(u, exclude = exclude)
        else if(has.exclude) # Don't drop NA from factors unless explicitly asked
            u <- factor(as.character(u),
                        levels = setdiff(levels(u), exclude),
                        exclude=NULL)
	if(addNA) u <- addNA(u, ifany=TRUE)
	u[ , drop = drop.unused.levels]
    })
    naAct <- if(!is.null(m$na.action)) m$na.action
	     else getOption("na.action", default = quote(na.omit))
    na.rm <- ## true iff na.action is na.omit
	identical(naAct, quote(na.omit)) || identical(naAct, na.omit) ||
        identical(naAct, "na.omit")
    if(!sparse) {
	x <-
	    if(is.null(y))
		table(by, dnn = names(by))
	    else if(NCOL(y) == 1L)
		tapply(y, by, sum, na.rm=na.rm, default = 0L)
	    else {
		z <- lapply(as.data.frame(y), tapply, by, sum, na.rm=na.rm, default = 0L)
		array(unlist(z),
		      dim = c(dim(z[[1L]]), length(z)),
		      dimnames = c(dimnames(z[[1L]]), list(names(z))))
	    }
	class(x) <- c("xtabs", "table")
	attr(x, "call") <- match.call()
	x

    } else { ## sparse
	if (length(by) != 2L)
	    stop(gettextf("%s applies only to two-way tables",
                          "xtabs(*, sparse=TRUE)"),
                 domain = NA)
        ## loadNamespace(.) is very quick, once it *is* loaded:
	if(is.null(tryCatch(loadNamespace("Matrix"), error = function(e)NULL)))
            stop(gettextf("%s needs package 'Matrix' correctly installed",
                          "xtabs(*, sparse=TRUE)"),
                 domain = NA)
	if(length(i.ex <- unique(unlist(lapply(by, function(f) which(is.na(f))))))) {
	    by <- lapply(by, `[`, -i.ex)
	    if(!is.null(y)) y <- y[-i.ex]
	}
	if(na.rm && !is.null(y) && any(isN <- is.na(y))) {
	    ok <- !isN
	    by <- lapply(by, `[`, ok)
	    y <- y[ok]
	}
	rows <- by[[1L]]
	cols <- by[[2L]]
        dnms <- lapply(by, levels)
	x <- if (is.null(y)) rep.int(1, length(rows)) else as.double(y)
	methods::as(methods::new("dgTMatrix", x = x, Dimnames = dnms,
				 i = as.integer(rows) - 1L,
				 j = as.integer(cols) - 1L,
				 Dim = lengths(dnms, use.names=FALSE)),
		    "CsparseMatrix")
    }
}

print.xtabs <- function(x, na.print = "", ...) ## na.print = "NA" is more cautious
{
    ox <- x
    attr(x, "call") <- NULL
    print.table(x, na.print=na.print, ...)
    invisible(ox)
}
