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

## Create R objects defining gradients

extendOptions <- c("pad", "repeat", "reflect", "none")

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
    colours <- rgb(t(col2rgb(colours)), maxColorValue=255)
    
    grad <- list(x1 = as.numeric(x1), y1 = as.numeric(y1),
                 x2 = as.numeric(x2), y2 = as.numeric(y2),
                 stops = as.numeric(stops), colours = colours,
                 extend = as.integer(extend))
    class(grad) <- c("LinearGradient", "Gradient")
    grad
}
