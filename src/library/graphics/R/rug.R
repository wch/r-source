rug <- function(x, ticksize = 0.03, side = 1, lwd = 0.5, col,
		quiet = getOption("warn") < 0, ...)
{
    x <- as.vector(x)
    ok <- is.finite(x)
    x <- x[ok]
    oldtick <- par(tck = ticksize)
    on.exit(par(oldtick))
    if( !missing(col) ) {
	oldcol <- par(fg = col)
	on.exit(par(oldcol), add=TRUE)
    }
    if(!quiet) {
	u <- par("usr")
	u <- if (side %% 2 == 1) {
	    if(par("xlog")) 10^u[1:2] else u[1:2]
	} else {
	    if(par("ylog")) 10^u[3:4] else u[3:4]
	}
	if(any(x < u[1] | x > u[2]))
	    warning("some values will be clipped")
    }
    Axis(side = side, at = x, labels = FALSE, lwd = lwd, ...)
}
