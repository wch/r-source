#  File src/library/stats/R/qqplot.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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

qqplot <- function(x, y, plot.it = TRUE,
                   xlab = deparse1(substitute(x)),
                   ylab = deparse1(substitute(y)), ..., 
                   conf.level = NULL, 
                   conf.args = list(exact = NULL, simulate.p.value = FALSE,
                                    B = 2000, col = NA, border = NULL))
{
    force(xlab) ; force(ylab) # get defaults explicitly before touching x, y
    ## Sort x, y in place without length adjustment
    x <- sx <- sort(x)
    y <- sy <- sort(y)
    lenx <- length(sx)
    leny <- length(sy)
    if( leny < lenx )
        sx <- approx(1L:lenx, sx, n = leny)$y
    else if( leny > lenx )
        sy <- approx(1L:leny, sy, n = lenx)$y

    if (is.null(conf.level)) {
        if(plot.it)
            plot(sx, sy, xlab = xlab, ylab = ylab, ...)
        return(invisible(list(x = sx, y = sy)))
    }

    if (conf.level < 0 || conf.level > 1)
        stop("'conf.level' is not a probability")

    N <- lenx + leny
    if (is.null(conf.args$exact)) conf.args$exact <- NULL
    exact <- conf.args$exact
    if (is.null(conf.args$simulate.p.value)) conf.args$simulate.p.value <- FALSE
    simulate <- conf.args$simulate.p.value
    if (is.null(conf.args$B)) conf.args$B <- 2000
    if (is.null(conf.args$col)) conf.args$col <- NA
    if (is.null(conf.args$border)) conf.args$border <- NULL

    if (is.null(exact)) exact <- lenx * leny < 10000
    z <- c(x, y)
    TIES <- any(duplicated(z))
    if (!TIES) z <- NULL
    ca <- qsmirnov(conf.level, sizes = c(lenx, leny), z = z,
                   exact = exact && !simulate, 
                   simulate = simulate, B = conf.args$B)

    ### Switzer (1976, Biometrika) 10.1093/biomet/63.1.13, eq. 4 + 5
    ### NB, all indices are in terms of sorted x, y
    i <- as.double(seq_along(x))
    Rl <- ceiling(i * N / lenx - ca * as.double(leny)) - i
    Rl[Rl < 1] <- NA
    Rl[Rl > leny] <- NA
    Rr <- floor(i * N / lenx - leny / lenx + ca * as.double(leny)) - i + 1
    Rr[Rr < 1] <- NA
    Rr[Rr > leny] <- NA
    ## Indices refer to original sorted data
    lwr <- y[Rl]
    upr <- y[Rr]
    ## Reduce length of confidence bands when needed
    if (leny < lenx) {
    	lwr <- approx(x, lwr, xout = sx)$y
    	upr <- approx(x, upr, xout = sx)$y
    }
    ret <- list(x = sx, y = sy, lwr = lwr, upr = upr)

    if (plot.it) {
        plot(sx, sy, xlab = xlab, ylab = ylab, type = "n", ...)
        x <- c(min(x) - diff(range(x)) / 2, sx, 
               max(x) + diff(range(x)) / 2)
        lwr <- c(min(sy) - diff(range(sy)) / 2, lwr, 
                 max(sy) + diff(range(sy)) / 2)
        upr <- c(min(sy) - diff(range(sy)) / 2, upr, 
                 max(sy) + diff(range(sy)) / 2)
        x <- c(x, rev(x))
        y <- c(lwr, rev(upr))
        x <- x[!is.na(y)]
        y <- y[!is.na(y)]
        xn <- c(x[1L], rep(x[-1L], each = 2))
        yn <- c(rep(y[-length(y)], each = 2), y[length(y)])
        polygon(x = xn, y = yn, col = conf.args$col,
                border = conf.args$border)
        points(sx, sy, ...)
    }	

    invisible(ret)
}
