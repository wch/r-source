as.char.or.expr <-
function(x) if(is.expression(x)) x else unlist(strsplit(as.character(x),"\n"))

text <-
function(x, y=NULL, labels = seq(along=x), ...)
	.Internal(text(xy.coords(x,y), as.char.or.expr(labels), ...))
