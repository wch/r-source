#  File src/library/grDevices/R/colorRamp.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/


colorRampPalette <- function(colors,...)
{
    ramp <- colorRamp(colors,...)
    function(n) {
        x <- ramp(seq.int(0, 1, length.out = n))
        if (ncol(x) == 4L)
            rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
        else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
    }

}

colorRamp <- function(colors, bias = 1, space = c("rgb","Lab"),
                    interpolate = c("linear","spline"), alpha = FALSE)
{
    if (bias <= 0) stop("'bias' must be positive")
    if (!missing(space) && alpha)
        stop("'alpha' must be false if 'space' is specified")

    colors <- t(col2rgb(colors, alpha = alpha)/255)
    space <- match.arg(space)
    interpolate <- match.arg(interpolate)

    if (space == "Lab")
        colors <- convertColor(colors, from = "sRGB", to = "Lab")


    interpolate <- switch(interpolate,
                          linear = stats::approxfun,
                          spline = stats::splinefun)

    if((nc <- nrow(colors)) == 1L) {
        colors <- colors[c(1L, 1L) ,]
        nc <- 2L
    }
    x <- seq.int(0, 1, length.out = nc)^bias
    palette <- c(interpolate(x, colors[, 1L]),
                 interpolate(x, colors[, 2L]),
                 interpolate(x, colors[, 3L]),
                 if(alpha) interpolate(x, colors[, 4L]))

    roundcolor <- function(rgb) ## careful to preserve matrix:
	pmax(pmin(rgb, 1), 0)

    if (space == "Lab")
        function(x)
            roundcolor(convertColor(cbind(palette[[1L]](x),
                                          palette[[2L]](x),
                                          palette[[3L]](x),
                                          if(alpha) palette[[4L]](x)),
                                    from = "Lab", to = "sRGB"))*255
    else
        function(x)
            roundcolor(cbind(palette[[1L]](x),
                             palette[[2L]](x),
                             palette[[3L]](x),
                             if(alpha) palette[[4L]](x)))*255
}
