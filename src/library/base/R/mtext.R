mtext <- function(text, side=3, line=0, outer=FALSE, at=NULL, adj=NA, ...)
  .Internal(mtext(as.char.or.expr(text), side, line, outer, at, adj, ...))
	#> ../../../main/plot.c
