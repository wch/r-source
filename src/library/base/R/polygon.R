polygon <- function(x, y=NULL, col=NA, border=NULL, lty=NULL, xpd=NULL,
		    density = -1, angle = 45, ...)
{
    if (!missing(density)) .NotYetUsed("density", error = FALSE)
    if (!missing(angle))   .NotYetUsed("angle",   error = FALSE)
    xy <- xy.coords(x, y)
    ##-- FIXME: what if `log' is active, for x or y?
    .Internal(polygon(xy$x, xy$y, col, border, lty, xpd, ...))
}
