#  File src/library/stats/R/cpgram.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1994-9  W. N. Venables and B. D. Ripley
#  Copyright (C) 1999-2012 The R Core Team
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

## from MASS package

cpgram <-
    function(ts, taper = 0.1,
             main = paste("Series: ", deparse(substitute(ts))),
             ci.col = "blue")
{
    main
    if(NCOL(ts) > 1)
	stop("only implemented for univariate time series")
    x <- as.vector(ts)
    x <- x[!is.na(x)]
    x <- spec.taper(scale(x, TRUE, FALSE), p=taper)
    y <- Mod(fft(x))^2/length(x)
    y[1L] <- 0
    n <- length(x)
    x <- (0:(n/2))*frequency(ts)/n
    if(length(x)%%2==0) {
	n <- length(x)-1
	y <- y[1L:n]
	x <- x[1L:n]
    } else y <- y[seq_along(x)]
    xm <- frequency(ts)/2
    mp <- length(x)-1
    crit <- 1.358/(sqrt(mp)+0.12+0.11/sqrt(mp))
    oldpty <- par(pty ="s")
    on.exit(par(oldpty))
    plot(x, cumsum(y)/sum(y), type="s", xlim=c(0, xm),
	 ylim=c(0, 1), xaxs="i", yaxs="i", xlab="frequency",
	 ylab="")
    lines(c(0, xm*(1-crit)), c(crit, 1), col = ci.col, lty = 2)
    lines(c(xm*crit, xm), c(0, 1-crit), col = ci.col, lty = 2)
    title(main = main)
    invisible()
}
