points <- function(x, ...) UseMethod("points")

points.default <- function(x, y=NULL, type="p", ...)
    plot.xy(xy.coords(x,y), type=type, ...)

