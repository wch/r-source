#  File src/library/grid/R/function.R
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


validDetails.functiongrob <- function(x, ...) {
    if (x$n < 1)
        stop("invalid 'n'")
    if (!(is.character(x$range) && x$range %in% c("x", "y")))
        x$range <- as.numeric(x$range)
    if (!is.function(x$f))
        stop("invalid 'f'")
    x
}

genXY <- function(x) {
    if (is.numeric(x$range)) {
        range <- range(x$range)
    } else {
        if (x$range == "x")
            range <- current.viewport()$xscale
        else
            range <- current.viewport()$yscale
    }
    input <- seq(range[1], range[2], length.out=x$n)
    x$f(input)
}

makeContent.functiongrob <- function(x) {
    xy <- genXY(x)
    linesGrob(xy$x, xy$y, default.units=x$units,
              name=x$name, gp=x$gp, vp=x$vp)
}

xDetails.functiongrob <- function(x, theta) {
    xy <- genXY(x)
    xDetails(linesGrob(xy$x, xy$y, default.units=x$units), theta)
}

yDetails.functiongrob <- function(x, theta) {
    xy <- genXY(x)
    yDetails(linesGrob(xy$x, xy$y, default.units=x$units), theta)
}

widthDetails.functiongrob <- function(x) {
    xy <- genXY(x)
    widthDetails(linesGrob(xy$x, xy$y, default.units=x$units))
}

heightDetails.functiongrob <- function(x) {
    xy <- genXY(x)
    heightDetails(linesGrob(xy$x, xy$y, default.units=x$units))
}

functionGrob <- function(f, n=101, range="x", units="native",
                         name=NULL, gp=gpar(), vp=NULL) {
    grob(f=f, range=range, units=units, n=n,
         gp=gp, vp=vp, name=name, cl="functiongrob")
}

grid.function <- function(...) {
    grid.draw(functionGrob(...))
}

# Convenience wrappers
grid.abline <- function(intercept=0, slope=1, ...) {
    grid.function(function(x) list(x=x, y=intercept + slope*x), ...)
}

##############
# Tests
tests <- function() {

    # editing
    grid.newpage()
    pushViewport(viewport(xscale=c(0, 2*pi), yscale=c(-1, 1)))
    grid.function(function(x) list(x=x, y=sin(x)), name="fg")
    grid.edit("fg", n=10)
    grid.edit("fg", f=function(x) list(x=x, y=x))

    # x/y/width/height calculations
    grid.newpage()
    pushViewport(viewport(xscale=c(0, 2*pi), yscale=c(-2, 2)))
    grid.function(function(x) list(x=x, y=sin(x)), name="fg")
    grid.segments(0, 1,
                  grobX("fg", 135), grobY("fg", 135),
                  arrow=arrow())

}
