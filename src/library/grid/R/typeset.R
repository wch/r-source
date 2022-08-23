
validDetails.typesetgrob <- function(x) {
    if (!inherits(x$span, "textspan"))
        stop("Invalid span for typesetting")
    if (!is.unit(x$x) || !is.unit(x$y))
        stop("'x' and 'y' must be units")
    ## Make sure that x and y are of length > 0
    if (length(x$x) < 1 || length(x$y) < 1)
        stop("'x' and 'y' must have length > 0")
    x
}

drawDetails.typesetgrob <- function(x, recording=TRUE) {
    ## TODO:  fill in any NAs in span with 'gp' values
    grid.Call.graphics(C_typeset, x$span, x$x, x$y)
}

## NOTE that 'gp' settings only use the FIRST value
## as defaults for the span
typesetGrob <- function(span,
                        x=.5, y=.5, default.units="npc",
                        gp=gpar(), vp=NULL, name=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    grob(span=span, x=x, y=y, gp=gp, vp=vp, name=name,
         cl="typesetgrob")
}

grid.typeset <- function(...) {
    grid.draw(typesetGrob(...))
}
