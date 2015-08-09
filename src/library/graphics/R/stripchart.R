#  File src/library/graphics/R/stripchart.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

## Dotplots a la Box, Hunter and Hunter

stripchart <- function(x, ...) UseMethod("stripchart")

stripchart.default <-
function(x, method = "overplot", jitter = 0.1, offset = 1/3, vertical = FALSE,
	 group.names, add = FALSE, at = NULL,
	 xlim = NULL, ylim = NULL, ylab = NULL, xlab = NULL,
         dlab = "", glab = "", log = "", pch = 0, col = par("fg"),
         cex = par("cex"), axes = TRUE, frame.plot = axes, ...)
{
    method <- pmatch(method, c("overplot", "jitter", "stack"))[1L]
    if(is.na(method) || method == 0L)
	stop("invalid plotting method")
    groups <-
	if(is.list(x)) x else if(is.numeric(x)) list(x)
    n <- length(groups)
    if(!n) stop("invalid first argument")
    if(!missing(group.names))
	attr(groups, "names") <- group.names
    else if(is.null(attr(groups, "names")))
	attr(groups, "names") <- seq_len(n)
    if(is.null(at)) at <- seq_len(n)
    else if(length(at) != n)
	stop(gettextf("'at' must have length equal to the number %d of groups",
                      n), domain = NA)
    if (is.null(dlab)) dlab <- deparse(substitute(x))

    dev.hold(); on.exit(dev.flush())
    if(!add) {
        dlim <- range(unlist(groups, use.names = FALSE), na.rm = TRUE)
	glim <- c(1L, n) # in any case, not range(at)
	if(method == 2L) { # jitter
	    glim <- glim + jitter * if(n == 1) c(-5, 5) else c(-2, 2)
	} else if(method == 3) { # stack
	    glim <- glim + if(n == 1L) c(-1,1) else c(0, 0.5)
	}
	if(is.null(xlim))
	    xlim <- if(vertical) glim else dlim
	if(is.null(ylim))
	    ylim <- if(vertical) dlim else glim
        plot.new()
        plot.window(xlim, ylim, log, ...)
        if(frame.plot) box() # maybe (...)
	if(vertical) {
	    if (axes) {
		if(n > 1L) axis(1, at = at, labels = names(groups), ...)
		Axis(x, side = 2, ...)
	    }
	    if (is.null(ylab)) ylab <- dlab
	    if (is.null(xlab)) xlab <- glab
	}
	else {
	    if (axes) {
		Axis(x, side = 1, ...)
		if(n > 1L) axis(2, at = at, labels = names(groups), ...)
	    }
	    if (is.null(xlab)) xlab <- dlab
	    if (is.null(ylab)) ylab <- glab
	}
	title(xlab = xlab, ylab = ylab, ...)
    }
    csize <- cex *
	if(vertical) xinch(par("cin")[1L]) else yinch(par("cin")[2L])
    for(i in seq_len(n)) {
	x <- groups[[i]]
	y <- rep.int(at[i], length(x))
	if(method == 2L) ## jitter
	    y <- y + stats::runif(length(y), -jitter, jitter)
	else if(method == 3L) { ## stack
	    xg <- split(x, factor(x))
	    xo <- lapply(xg, seq_along)
	    x <- unlist(xg, use.names=FALSE)
	    y <- rep.int(at[i], length(x)) +
		(unlist(xo, use.names=FALSE) - 1) * offset * csize
	}
	if(vertical)
            points(y, x, col = col[(i - 1L) %% length(col) + 1L],
                   pch = pch[(i - 1L) %% length(pch) + 1L], cex = cex, ...)
	else
            points(x, y, col = col[(i - 1L) %% length(col) + 1L],
                   pch = pch[(i - 1L) %% length(pch) + 1L], cex = cex, ...)
    }
    invisible()
}

stripchart.formula <-
    function(x, data = NULL, dlab = NULL, ..., subset, na.action = NULL)
{
    if(missing(x) || (length(x) != 3L))
	stop("formula missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)
    m$... <- NULL
    m$formula <- m$x
    m$x <- NULL
    m$na.action <- na.action # force use of default for this method
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    if (is.null(dlab)) dlab <- names(mf)[response]
    stripchart(split(mf[[response]], mf[-response]), dlab = dlab, ...)
}
