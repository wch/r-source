as.char.or.expr <-
function(x) if(is.expression(x)) x else as.character(x)

text <-
function(x, y=NULL, labels = seq(along=x), ...)
	.Internal(text(xy.coords(x,y), as.char.or.expr(labels), ...))
