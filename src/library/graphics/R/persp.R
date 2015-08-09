#  File src/library/graphics/R/persp.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

persp <- function(x, ...) UseMethod("persp")

persp.default <-
function (x = seq(0, 1, length.out = nrow(z)),
          y = seq(0, 1, length.out = ncol(z)),
          z, xlim = range(x), ylim = range(y), zlim = range(z, na.rm = TRUE),
          xlab = NULL, ylab = NULL, zlab = NULL, main = NULL, sub = NULL,
          theta = 0, phi = 15, r = sqrt(3), d = 1, scale = TRUE, expand = 1,
          col = "white", border = NULL, ltheta = -135, lphi = 0, shade = NA,
          box = TRUE, axes = TRUE, nticks = 5, ticktype = "simple", ...)
{
    if (is.null(xlab))
        xlab <- if (!missing(x)) deparse(substitute(x)) else "X"
    if (is.null(ylab))
        ylab <- if (!missing(y)) deparse(substitute(y)) else "Y"
    if (is.null(zlab))
        zlab <- if (!missing(z)) deparse(substitute(z)) else "Z"
    ## labcex is disregarded since we do NOT yet put  ANY labels...
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing 'x' and 'y' values expected")
    if (box) {
        zz <- z[!is.na(z)]
        if(any(x < xlim[1]) || any(x > xlim[2]) ||
           any(y < ylim[1]) || any(y > ylim[2]) ||
           any(zz < zlim[1]) || any(zz > zlim[2]))
            warning("surface extends beyond the box")
    }
    ticktype <- pmatch(ticktype, c("simple", "detailed"))
    plot.new()
    r <- .External.graphics(C_persp, x, y, z, xlim, ylim, zlim, theta, phi, r, d,
                            scale, expand, col, border, ltheta, lphi, shade,
                            box, axes, nticks, ticktype,
                            as.character(xlab), as.character(ylab),
                            as.character(zlab), ...)
    for(fun in getHook("persp")) {
        if(is.character(fun)) fun <- get(fun)
        try(fun())
    }
    if(!is.null(main) || !is.null(sub))
        title(main = main, sub = sub, ...)
    invisible(r)
}
