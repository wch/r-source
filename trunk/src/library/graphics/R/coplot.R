#  File src/library/graphics/R/coplot.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

co.intervals <- function (x, number = 6, overlap = 0.5)
{
    x <- sort(x[!is.na(x)])
    n <- length(x)
    ## "from the record"
    r <- n/(number * (1 - overlap) + overlap)
    ii <- 0:(number - 1) * (1 - overlap) * r
    x1 <- x[round(1 + ii)]
    xr <- x[round(r + ii)]
    ## Omit any range of values identical with the previous range;
    ## happens e.g. when `number' is less than the number of distinct x values.
    keep <- c(TRUE, diff(x1) > 0 | diff(xr) > 0)
    ## Set eps > 0 to ensure that the endpoints of a range are never
    ## identical, allowing display of a given.values bar
    j.gt.0 <- 0 < (jump <- diff(x))
    eps <- 0.5 * if(any(j.gt.0)) min(jump[j.gt.0]) else 0
    cbind(x1[keep] - eps, xr[keep] + eps)
}

panel.smooth <- function(x, y, col = par("col"), bg = NA, pch = par("pch"),
			 cex = 1, col.smooth = 2, span = 2/3, iter = 3, ...)
{
    points(x, y, pch=pch, col=col, bg=bg, cex=cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
	lines(stats::lowess(x[ok], y[ok], f=span, iter=iter),
              col = col.smooth, ...)
}

coplot <-
    function(formula, data, given.values, panel=points, rows, columns,
	     show.given = TRUE, col = par("fg"), pch=par("pch"),
	     bar.bg = c(num = gray(0.8), fac = gray(0.95)),
	     xlab = c(x.name, paste("Given :", a.name)),
	     ylab = c(y.name, paste("Given :", b.name)),
	     subscripts = FALSE, axlabels = function(f) abbreviate(levels(f)),
	     number = 6, overlap = 0.5, xlim, ylim, ...)
{
    deparen <- function(expr) {
	while (is.language(expr) && !is.name(expr) &&
               deparse(expr[[1L]])[1L] == "(")
	    expr <- expr[[2L]]
	expr
    }
    bad.formula <- function() stop("invalid conditioning formula")
    bad.lengths <- function() stop("incompatible variable lengths")
    getOp <- function(call) deparse(call[[1L]], backtick=FALSE)[[1L]]

    ## parse and check the formula

    formula <- deparen(formula)
    if (!inherits(formula, "formula"))
	bad.formula()
    y <- deparen(formula[[2L]])
    rhs <- deparen(formula[[3L]])
    if (getOp(rhs) != "|") bad.formula()
    x <- deparen(rhs[[2L]])
    rhs <- deparen(rhs[[3L]])
    if (is.language(rhs) && !is.name(rhs) && getOp(rhs) %in%  c("*", "+")) {
	have.b <- TRUE
	a <- deparen(rhs[[2L]])
	b <- deparen(rhs[[3L]])
    } else {
	have.b <- FALSE
	a <- rhs
    }

    ## evaluate the formulae components to get the data values

    if (missing(data))
	data <- parent.frame()
    x.name <- deparse(x)
    x <- eval(x, data, parent.frame())
    nobs <- length(x)
    y.name <- deparse(y)
    y <- eval(y, data, parent.frame())
    if(length(y) != nobs) bad.lengths()
    a.name <- deparse(a)
    a <- eval(a, data, parent.frame())
    if(length(a) != nobs) bad.lengths()
    if(is.character(a)) a <- as.factor(a)
    a.is.fac <- is.factor(a)
    if (have.b) {
	b.name <- deparse(b)
	b <- eval(b, data, parent.frame())
	if(length(b) != nobs) bad.lengths()
	if(is.character(b)) b <- as.factor(b)
        b.is.fac <- is.factor(b)
	missingrows <- which(is.na(x) | is.na(y) | is.na(a) | is.na(b))
    }
    else {
	missingrows <- which(is.na(x) | is.na(y) | is.na(a))
	b <- NULL
	b.name <- "" # for default ylab
    }

    ## generate the given value intervals

    number <- as.integer(number)
    if(length(number) == 0L || any(number < 1))
        stop("'number' must be integer >= 1")
    if(any(overlap >= 1)) stop("'overlap' must be < 1 (and typically >= 0).")

    bad.givens <- function() stop("invalid 'given.values'")
    if(missing(given.values)) {
	a.intervals <-
	    if(a.is.fac) {
		i <- seq_along(a.levels <- levels(a))
		a <- as.numeric(a)
		cbind(i - 0.5, i + 0.5)
	    } else co.intervals(unclass(a), number=number[1L], overlap=overlap[1L])
	b.intervals <-
	    if (have.b) {
		if(b.is.fac) {
                    i <- seq_along(b.levels <- levels(b))
		    b <- as.numeric(b)
		    cbind(i - 0.5, i + 0.5)
		}
		else {
		    if(length(number) == 1L) number  <- rep.int(number,2)
		    if(length(overlap) == 1L) overlap <- rep.int(overlap,2)
		    co.intervals(unclass(b), number=number[2L], overlap=overlap[2L])
		}
	    }
    } else {
	if(!is.list(given.values))
	    given.values <- list(given.values)
	if(length(given.values) != (if(have.b) 2L else 1L))
	    bad.givens()
	a.intervals <- given.values[[1L]]
	if(a.is.fac) {
	    a.levels <- levels(a)
	    if (is.character(a.intervals))
		a.intervals <- match(a.intervals, a.levels)
	    a.intervals <- cbind(a.intervals - 0.5, a.intervals + 0.5)
	    a <- as.numeric(a)
	}
	else if(is.numeric(a)) {
	    if(!is.numeric(a.intervals)) bad.givens()
	    if(!is.matrix(a.intervals) || ncol(a.intervals) != 2)
		a.intervals <- cbind(a.intervals - 0.5, a.intervals + 0.5)
	}
	if(have.b) {
	    b.intervals <- given.values[[2L]]
	    if(b.is.fac) {
		b.levels <- levels(b)
		if (is.character(b.intervals))
		    b.intervals <- match(b.intervals, b.levels)
		b.intervals <- cbind(b.intervals - 0.5, b.intervals + 0.5)
		b <- as.numeric(b)
	    }
	    else if(is.numeric(b)) {
		if(!is.numeric(b.intervals)) bad.givens()
		if(!is.matrix(b.intervals) || ncol(b.intervals) != 2)
		    b.intervals <- cbind(b.intervals - 0.5, b.intervals + 0.5)
	    }
	}
    }
    if(anyNA(a.intervals) || (have.b && anyNA(b.intervals)))
	bad.givens()

    ## compute the page layout

    if (have.b) {
	rows	<- nrow(b.intervals)
	columns <- nrow(a.intervals)
	nplots <- rows * columns
	if(length(show.given) < 2L) show.given <- rep.int(show.given, 2L)
    }
    else {
	nplots <- nrow(a.intervals)
	if (missing(rows)) {
	    if (missing(columns)) { ## default
		rows <- ceiling(round(sqrt(nplots)))
		columns <- ceiling(nplots/rows)
	    }
	    else rows <- ceiling(nplots/columns)
	}
	else if (missing(columns))
	    columns <- ceiling(nplots/rows)
	if (rows * columns < nplots)
	    stop("rows * columns too small")
    }
    total.columns <- columns
    total.rows <- rows
    f.col <- f.row <- 1
    if(show.given[1L]) {
	total.rows <- rows + 1
	f.row <- rows/total.rows
    }
    if(have.b && show.given[2L]) {
	total.columns <- columns + 1
	f.col <- columns/total.columns
    }

    mar <- if(have.b) rep.int(0, 4) else c(0.5, 0, 0.5, 0)
    oma <- c(5, 6, 5, 4)
    if(have.b) { oma[2L] <- 5 ; if(!b.is.fac) oma[4L] <- 5 }
    if(a.is.fac && show.given[1L]) oma[3L] <- oma[3L] - 1

    ## Start Plotting only now
    opar <- par(mfrow = c(total.rows, total.columns),
		oma = oma, mar = mar, xaxs = "r", yaxs = "r")
    on.exit(par(opar))
    dev.hold(); on.exit(dev.flush(), add = TRUE)
    plot.new()
    ## as.numeric() allowing factors for x & y:
    if(missing(xlim))
	xlim <- range(as.numeric(x), finite = TRUE)
    if(missing(ylim))
	ylim <- range(as.numeric(y), finite = TRUE)
    pch <- rep_len(pch, nobs)
    col <- rep_len(col, nobs)
    do.panel <- function(index, subscripts = FALSE, id) {
	## Use `global' variables
	##	rows, columns,	total.rows, total.columns, nplots, xlim, ylim
        Paxis <- function(side, x) {
            if(nlevels(x)) {
                lab <- axlabels(x)
                axis(side, labels = lab, at = seq(lab), xpd = NA)
            } else
                Axis(x, side=side, xpd = NA)
        }
	istart <- (total.rows - rows) + 1
	i <- total.rows - ((index - 1)%/%columns)
	j <- (index - 1)%%columns + 1
	par(mfg = c(i, j, total.rows, total.columns))
	plot.new()
	plot.window(xlim, ylim)
	if(anyNA(id)) id[is.na(id)] <- FALSE
	if(any(id)) {
	    grid(lty="solid")
	    if(subscripts)
		panel(x[id], y[id], subscripts = id,
		      col = col[id], pch=pch[id], ...)
	    else
		panel(x[id], y[id], col = col[id], pch=pch[id], ...)
	}
	if((i == total.rows) && (j%%2 == 0))
	    Paxis(1, x)
	else if((i == istart || index + columns > nplots) && (j%%2 == 1))
	    Paxis(3, x)

	if((j == 1) && ((total.rows - i)%%2 == 0))
	    Paxis(2, y)
	else if((j == columns || index == nplots) && ((total.rows - i)%%2 == 1))
	    Paxis(4, y)
	box()
    }## END function do.panel()

    if(have.b) {
	count <- 1
	for(i in 1L:rows) {
	    for(j in 1L:columns) {
		id <- ((a.intervals[j,1] <= a) & (a <= a.intervals[j,2]) &
		       (b.intervals[i,1] <= b) & (b <= b.intervals[i,2]))
		do.panel(count, subscripts, id)
		count <- count + 1
	    }
	}
    } else {
	for (i in 1L:nplots) {
	    id <- ((a.intervals[i,1] <= a) & (a <= a.intervals[i,2]))
	    do.panel(i, subscripts, id)
	}
    }
    mtext(xlab[1L], side = 1, at = 0.5*f.col, outer = TRUE, line = 3.5,
          xpd = NA, font = par("font.lab"), cex = par("cex.lab"))
    mtext(ylab[1L], side = 2, at = 0.5*f.row, outer = TRUE, line = 3.5,
          xpd = NA, font = par("font.lab"), cex = par("cex.lab"))

    if(length(xlab) == 1L)
        xlab <- c(xlab, paste("Given :", a.name))
    ##mar <- par("mar")
    if(show.given[1L]) {
	par(fig = c(0, f.col, f.row, 1),
            mar = mar + c(3+ !a.is.fac, 0, 0, 0), new=TRUE)
	plot.new()
	nint <- nrow(a.intervals)
        a.range <- range(a.intervals, finite=TRUE)
        ## 3% correction because axs = "r" extends by 4% :
	plot.window(a.range + c(.03,-.03)*diff(a.range), 0.5 + c(0, nint))
	rect(a.intervals[, 1], 1L:nint - 0.3,
	     a.intervals[, 2], 1L:nint + 0.3,
	     col = bar.bg[if(a.is.fac) "fac" else "num"])
	if(a.is.fac) {
	    text(apply(a.intervals, 1L, mean), 1L:nint, a.levels)
        }
        else {
            Axis(a, side = 3, xpd=NA)
            axis(1, labels=FALSE)
        }
	box()
	mtext(xlab[2L], 3, line = 3 - a.is.fac, at=mean(par("usr")[1L:2]),
              xpd=NA, font = par("font.lab"), cex = par("cex.lab"))
    }
    else { ## i. e. !show.given
	mtext(xlab[2L], 3, line = 3.25, outer = TRUE, at = 0.5*f.col,
              xpd = NA, font = par("font.lab"), cex = par("cex.lab"))
    }
    if(have.b) {
	if(length(ylab) == 1L)
            ylab <- c(ylab, paste("Given :", b.name))
	if(show.given[2L]) {
	    par(fig = c(f.col, 1, 0, f.row),
                mar = mar + c(0, 3+ !b.is.fac, 0, 0), new=TRUE)
	    plot.new()
	    nint <- nrow(b.intervals)
            b.range <- range(b.intervals, finite=TRUE)
            ## 3% correction (see above)
            plot.window(0.5 + c(0, nint), b.range+ c(.03,-.03)*diff(b.range))
	    rect(1L:nint - 0.3, b.intervals[, 1],
                 1L:nint + 0.3, b.intervals[, 2],
                 col = bar.bg[if(b.is.fac)"fac" else "num"])
	    if(b.is.fac) {
                text(1L:nint, apply(b.intervals, 1L, mean), b.levels, srt = 90)
            }
            else {
                Axis(b, side=4, xpd=NA)
                axis(2, labels=FALSE)
            }
	    box()
	    mtext(ylab[2L], 4, line = 3 - b.is.fac,
                  at = mean(par("usr")[3:4]), xpd = NA,
                  font = par("font.lab"), cex = par("cex.lab"))
	}
	else {
	    mtext(ylab[2L], 4, line = 3.25, at=0.5*f.row, outer = TRUE,
                  xpd = NA, font = par("font.lab"), cex = par("cex.lab"))
	}
    }
    if (length(missingrows)) {
	cat("\n", gettextf("Missing rows: %s",
                           paste0(missingrows, collapse = ", ")), "\n")
	invisible(missingrows)
    } else invisible()
}
