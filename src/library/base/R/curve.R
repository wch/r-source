curve <- function(expr, from, to, n=101, add=FALSE, type="l", ...) {
	expr <- substitute(expr)
	lims <- par("usr")
	if(missing(from)) from <- lims[1]
	if(missing(to)) to <- lims[2]
	x <- seq(from,to,length=n)
	y <- eval(expr)
	if(add)
		lines(x, y, ...)
	else
		plot(x, y, type="l", ...)
}
