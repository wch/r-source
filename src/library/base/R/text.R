as.char.or.expr <- function(x) {
    if (is.expression(x)) x 
    else if (is.call(x)) as.expression(x)
    else as.character(x)
}

text <- function(x, ...) UseMethod("text")
text.default <- function(x, y = NULL, labels = seq(along = x), adj =
			 NULL, ...) {
    if (!missing(y) && (is.character(y) || is.expression(y))) {
	labels <- y; y <- NULL
    }
    .Internal(text(xy.coords(x,y, recycle=TRUE),
		   as.char.or.expr(labels), adj, ...))
}
