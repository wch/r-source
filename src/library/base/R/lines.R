lines <- function(x, ...) UseMethod("lines")

lines.default <- function(x, y=NULL, type="l", col=par("col"), ...) {
    plot.xy(xy.coords(x, y), type=type, col=col, ...)
}
