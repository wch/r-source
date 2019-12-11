#  File src/library/grDevices/R/gradients.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2019      The R Foundation
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

## Create R objects defining patterns

#######################################################
## MUST match C #defines in
## ../../../main/patterns.c and
## ../../../include/R_ext/GraphicsEngine.h
#######################################################

patternTypes <- c("LinearGradient", "RadialGradient")

extendOptions <- c("pad", "repeat", "reflect", "none")

pattern <- function(type, ...) {
    type <- match(type, patternTypes)
    if (is.na(type))
        stop("Invalid pattern type")
    
    pattern <- c(list(type=as.integer(type)), list(...))
    class(pattern) <- c(type, "Pattern")
    pattern
}

linearGradient <- function(colours = c("black", "white"),
                              stops = seq(0, 1, length.out = length(colours)),
                              x1 = 0, y1 = 0, x2 = 1, y2 = 1,
                              extend = "pad") {
    ## Vectorising colours & stops
    nstops <- max(length(colours), length(stops))
    colours <- rep(colours, length.out = nstops)
    stops <- rep(stops, length.out = nstops)

    extend <- match(extend, extendOptions)
    if (is.na(extend))
        stop("Invalid 'extend' value")
    
    ## Make sure we really have colours in some form
    ## AND convert to "#RRGGBB" format
    RGBA <- col2rgb(colours, alpha=TRUE)
    colours <- rgb(t(RGBA[1:3,]), alpha=RGBA[4,], maxColorValue=255)
    
    pattern("LinearGradient",
            x1 = as.numeric(x1), y1 = as.numeric(y1),
            x2 = as.numeric(x2), y2 = as.numeric(y2),
            stops = as.numeric(stops), colours = colours,
            extend = as.integer(extend))
}

radialGradient <- function(colours = c("black", "white"),
                              stops = seq(0, 1, length.out = length(colours)),
                              cx1 = 0, cy1 = 0, r1 = 0,
                              cx2 = 1, cy2 = 1, r2 = .5,
                              extend = "pad") {
    ## Vectorising colours & stops
    nstops <- max(length(colours), length(stops))
    colours <- rep(colours, length.out = nstops)
    stops <- rep(stops, length.out = nstops)

    extend <- match(extend, extendOptions)
    if (is.na(extend))
        stop("Invalid 'extend' value")
    
    ## Make sure we really have colours in some form
    ## AND convert to "#RRGGBB" format
    RGBA <- col2rgb(colours, alpha=TRUE)
    colours <- rgb(t(RGBA[1:3,]), alpha=RGBA[4,], maxColorValue=255)
    
    pattern("RadialGradient",
            cx1 = as.numeric(cx1), cy1 = as.numeric(cy1),
            r1 = as.numeric(r1),
            cx2 = as.numeric(cx2), cy2 = as.numeric(cy2),
            r2 = as.numeric(r2),
            stops = as.numeric(stops), colours = colours,
            extend = as.integer(extend))
}

setPattern <- function(pattern) {
    .External(C_setPattern, pattern)
}

