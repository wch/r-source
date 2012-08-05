#  File src/library/graphics/R/pie.R
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

pie <-
    function (x, labels = names(x), edges = 200, radius = 0.8,
              clockwise = FALSE, init.angle = if(clockwise) 90 else 0,
              density = NULL, angle = 45, col = NULL, border = NULL,
              lty = NULL, main = NULL, ...)
{
    if (!is.numeric(x) || any(is.na(x) | x < 0))
	stop("'x' values must be positive.")
    if (is.null(labels))
	labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    # NOTE: this needs to happen AFTER the plot.new so that
    # we enquire about the CURRENT plot region size, not the
    # PREVIOUS plot region size
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L]) xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    dev.hold(); on.exit(dev.flush())
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
        col <- if(is.null(density))
            c("white", "lightblue", "mistyrose", "lightcyan",
              "lavender", "cornsilk")
        else par("fg")
    if(!is.null(col))
        col <- rep_len(col, nx)
    if(!is.null(border))
        border <- rep_len(border, nx)
    if(!is.null(lty))
        lty <- rep_len(lty, nx)
    angle <- rep(angle, nx)
    if(!is.null(density))
        density <- rep_len(density, nx)
    twopi <- if(clockwise) -2*pi else 2*pi
    t2xy <- function(t) {
        t2p <- twopi*t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
	n <- max(2, floor(edges * dx[i]))
	P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
	polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i],
                border = border[i], col = col[i], lty = lty[i])
	P <- t2xy(mean(x[i + 0:1]))
        lab <-  as.character(labels[i])
        if(!is.na(lab) && nzchar(lab)) {
            lines(c(1, 1.05)*P$x, c(1, 1.05)*P$y)
            text(1.1*P$x, 1.1*P$y, labels[i], xpd = TRUE,
                 adj = ifelse(P$x < 0, 1, 0), ...)
        }
    }
    title(main = main, ...)
    invisible(NULL)
}
