#  File src/library/stats/R/ksmooth.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1998-2012 The R Core Team
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

ksmooth <-
  function(x, y, kernel = c("box", "normal"), bandwidth = 0.5,
           range.x = range(x), n.points = max(100L, length(x)), x.points)
{
    ## box is [-0.5, 0.5]. normal is sd = 1.4826/4
    if(missing(y) || is.null(y))
	stop("numeric y must be supplied.\nFor density estimation use density()")
    kernel <- match.arg(kernel)
    krn <- switch(kernel, "box" = 1L, "normal" = 2L)
    x.points <-
	if(missing(x.points))
	    seq.int(range.x[1L], range.x[2L], length.out = n.points)
	else { n.points <- length(x.points); sort(x.points) }
    ord <- order(x)
    .Call(C_ksmooth, x[ord], y[ord], x.points, krn, bandwidth)
}

