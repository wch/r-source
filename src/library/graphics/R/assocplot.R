#  File src/library/graphics/R/assocplot.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

assocplot <- function(x, col = c("black", "red"), space = 0.3,
                      main = NULL, xlab = NULL, ylab = NULL)
{
    if(length(dim(x)) != 2L)
        stop("'x' must be a 2-d contingency table")
    if(any(x < 0) || anyNA(x))
        stop("all entries of 'x' must be nonnegative and finite")
    if((n <- sum(x)) == 0L)
        stop("at least one entry of 'x' must be positive")
    if(length(col) != 2L)
        stop("incorrect 'col': must be length 2")

    f <- x[ , rev(1L:NCOL(x))]           # rename for convenience;
                                        # f is observed freqs
                                        # reverse to be consistent with
                                        # mosaicplot().
    e <- outer(rowSums(f), colSums(f), "*") / n
                                        # e is expected freqs
    d <- (f - e) / sqrt(e)              # Pearson residuals
    e <- sqrt(e)
    x.w <- apply(e, 1L, max)             # the widths of the x columns
    y.h <- apply(d, 2L, max) - apply(d, 2L, min)
                                        # the heights of the y rows
    x.delta <- mean(x.w) * space
    y.delta <- mean(y.h) * space
    xlim <- c(0, sum(x.w) + NROW(f) * x.delta)
    ylim <- c(0, sum(y.h) + NCOL(f) * y.delta)
    dev.hold(); on.exit(dev.flush())
    plot.new()
    plot.window(xlim, ylim, log = "")
    x.r <- cumsum(x.w + x.delta)
    x.m <- (c(0, x.r[-NROW(f)]) + x.r) / 2
    y.u <- cumsum(y.h + y.delta)
    y.m <- y.u - apply(pmax(d, 0), 2L, max) - y.delta / 2
    z <- expand.grid(x.m, y.m)
    rect(z[, 1] - e / 2, z[, 2],
         z[, 1] + e / 2, z[, 2] + d,
         col = col[1 + (d < 0)])
    axis(1, at = x.m, labels = rownames(f), tick = FALSE)
    axis(2, at = y.m, labels = colnames(f), tick = FALSE)
    abline(h = y.m, lty = 2)
    ndn <- names(dimnames(f))
    if(length(ndn) == 2L) {
        if(is.null(xlab))
            xlab <- ndn[1L]
        if(is.null(ylab))
            ylab <- ndn[2L]
    }
    title(main = main, xlab = xlab, ylab = ylab)
}
