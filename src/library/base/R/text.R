as.char.or.expr <- function(x) {
    if (is.expression(x)) x 
    else if (is.call(x)) as.expression(x)
    else as.character(x)
}

text <- function(x, ...) UseMethod("text")
text.default <- function(x, y = NULL, labels = seq(along = x),
                         adj = NULL, pos = NULL, offset = 0.5, 
                         vfont = NULL, ...) {
    if (!missing(y) && (is.character(y) || is.expression(y))) {
	labels <- y; y <- NULL
    }
    if (!is.null(vfont)) {
        typeface <- pmatch(vfont[1], c("serif", "sans serif", "script",
		                       "gothic english", "gothic german",
			      	       "gothic italian", "serif symbol",
				       "sans serif symbol"))
        fontindex <- pmatch(vfont[2], c("symbol", "plain", "italic", "bold",
				        "bold italic"))
        vfont <- c(typeface-1, fontindex-1)
    }
    .Internal(text(xy.coords(x,y, recycle=TRUE),
		   as.char.or.expr(labels), adj, pos, offset, vfont, ...))
}
