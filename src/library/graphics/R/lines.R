lines <- function(x, ...) UseMethod("lines")

lines.default <- function(x, y=NULL, type="l", ...)
    plot.xy(xy.coords(x, y), type=type, ...)

