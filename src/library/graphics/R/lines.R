lines <- function(x, ...) UseMethod("lines")

lines.default <- function(x, y=NULL, type="l", col=par("col"),
                          lty=par("lty"), ...)
{
    plot.xy(xy.coords(x, y), type=type, col=col, lty=lty, ...)
}
