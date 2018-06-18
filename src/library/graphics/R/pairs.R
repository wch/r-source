#  File src/library/graphics/R/pairs.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
#  Some parts  Copyright (C) 1999 Dr. Jens Oehlschlaegel-Akiyoshi
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

pairs <- function(x, ...) UseMethod("pairs")

pairs.formula <-
function(formula, data = NULL, ..., subset, na.action = stats::na.pass)
{
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action # force in even if  default
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    pairs(mf, ...)
}

#################################################
## some of the changes are from code
## Copyright (C) 1999 Dr. Jens Oehlschlaegel-Akiyoshi
## Others are by BDR and MM
## This version distributed under GPL (version 2 or later)
#################################################

pairs.default <-
function (x, labels, panel = points, ...,
          horInd = 1:nc, verInd = 1:nc,
          lower.panel = panel, upper.panel = panel,
          diag.panel = NULL, text.panel = textPanel,
          label.pos = 0.5 + has.diag/3, line.main = 3,
          cex.labels = NULL, font.labels = 1,
          row1attop = TRUE, gap = 1, log = "")
{
    if(doText <- missing(text.panel) || is.function(text.panel))
	textPanel <-
	    function(x = 0.5, y = 0.5, txt, cex, font)
		text(x, y, txt, cex = cex, font = font)

    localAxis <- function(side, x, y, xpd, bg, col=NULL, main, oma, ...) {
      ## Explicitly ignore any color argument passed in as
      ## it was most likely meant for the data points and
      ## not for the axis.
        xpd <- NA
        if(side %% 2L == 1L && xl[j]) xpd <- FALSE
        if(side %% 2L == 0L && yl[i]) xpd <- FALSE
        if(side %% 2L == 1L) Axis(x, side = side, xpd = xpd, ...)
        else Axis(y, side = side, xpd = xpd, ...)
    }

    localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
    localLowerPanel <- function(..., main, oma, font.main, cex.main)
        lower.panel(...)
    localUpperPanel <- function(..., main, oma, font.main, cex.main)
        upper.panel(...)

    localDiagPanel <- function(..., main, oma, font.main, cex.main)
        diag.panel(...)

    dots <- list(...); nmdots <- names(dots)
    if (!is.matrix(x)) {
        x <- as.data.frame(x)
        for(i in seq_along(names(x))) {
            if(is.factor(x[[i]]) || is.logical(x[[i]]))
               x[[i]] <- as.numeric(x[[i]])
            if(!is.numeric(unclass(x[[i]])))
                stop("non-numeric argument to 'pairs'")
        }
    } else if (!is.numeric(x)) stop("non-numeric argument to 'pairs'")
    panel <- match.fun(panel)
    if((has.lower <- !is.null(lower.panel)) && !missing(lower.panel))
        lower.panel <- match.fun(lower.panel)
    if((has.upper <- !is.null(upper.panel)) && !missing(upper.panel))
        upper.panel <- match.fun(upper.panel)
    if((has.diag  <- !is.null( diag.panel)) && !missing( diag.panel))
        diag.panel <- match.fun( diag.panel)

    if(row1attop) {
        tmp <- lower.panel; lower.panel <- upper.panel; upper.panel <- tmp
        tmp <- has.lower; has.lower <- has.upper; has.upper <- tmp
    }

    nc <- ncol(x)
    if (nc < 2L) stop("only one column in the argument to 'pairs'")
    if(!all(horInd >= 1L && horInd <= nc))
        stop("invalid argument 'horInd'")
    if(!all(verInd >= 1L && verInd <= nc))
        stop("invalid argument 'verInd'")
    if(doText) {
	if (missing(labels)) {
	    labels <- colnames(x)
	    if (is.null(labels)) labels <- paste("var", 1L:nc)
	}
	else if(is.null(labels)) doText <- FALSE
    }
    oma  <- if("oma"  %in% nmdots) dots$oma
    main <- if("main" %in% nmdots) dots$main
    if (is.null(oma))
	oma <- c(4, 4, if(!is.null(main)) 6 else 4, 4)
    opar <- par(mfcol = c(length(horInd), length(verInd)),
                mar = rep.int(gap/2, 4), oma = oma)
    on.exit(par(opar))
    dev.hold(); on.exit(dev.flush(), add = TRUE)

    xl <- yl <- logical(nc)
    if (is.numeric(log)) xl[log] <- yl[log] <- TRUE
    else {xl[] <- grepl("x", log); yl[] <- grepl("y", log)}
    ni <- length(iSet <- if(row1attop) horInd else rev(horInd))
    nj <- length(jSet <- verInd)
    for(j in jSet)
        for(i in iSet) {
            l <- paste0(if(xl[j]) "x" else "",
                        if(yl[i]) "y" else "")
            localPlot(x[, j], x[, i], xlab = "", ylab = "",
                      axes = FALSE, type = "n", ..., log = l)
            if(i == j || (i < j && has.lower) || (i > j && has.upper) ) {
                box()
                j.odd <- (match(j, jSet) + !row1attop) %% 2L
                i.odd <- (match(i, iSet) + !row1attop) %% 2L
                if(i == iSet[1L] && (!j.odd || !has.upper || !has.lower))
                    localAxis(3L, x[, j], x[, i], ...)
                if(i == iSet[ni] && ( j.odd || !has.upper || !has.lower))
                    localAxis(1L, x[, j], x[, i], ...)
                if(j == jSet[1L] && (!i.odd || !has.upper || !has.lower))
                    localAxis(2L, x[, j], x[, i], ...)
                if(j == jSet[nj] && ( i.odd || !has.upper || !has.lower))
                    localAxis(4L, x[, j], x[, i], ...)
                mfg <- par("mfg")
                if(i == j) {
                    if (has.diag) localDiagPanel(as.vector(x[, i]), ...)
		    if (doText) {
                        par(usr = c(0, 1, 0, 1))
                        if(is.null(cex.labels)) {
                            l.wid <- strwidth(labels, "user")
                            cex.labels <- max(0.8, min(2, .9 / max(l.wid)))
                        }
                        xlp <- if(xl[i]) 10^0.5 else 0.5
                        ylp <- if(yl[j]) 10^label.pos else label.pos
                        text.panel(xlp, ylp, labels[i],
                                   cex = cex.labels, font = font.labels)
                    }
                } else if(i < j)
                    localLowerPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
                else
                    localUpperPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
                if (any(par("mfg") != mfg))
                    stop("the 'panel' function made a new plot")
            }
            else par(new = FALSE)
        }
    if (!is.null(main)) {
        font.main <- if("font.main" %in% nmdots) dots$font.main else par("font.main")
        cex.main  <- if("cex.main"  %in% nmdots) dots$cex.main  else par("cex.main")
        mtext(main, 3, line.main, outer=TRUE, at = 0.5, cex = cex.main, font = font.main)
    }
    invisible(NULL)
}
