polygon <-
function(x, y=NULL, border=par("fg"), ...)
{
	xy <- xy.coords(x, y)
	.Internal(polygon(xy$x, xy$y, border=border, ...))
}
