axis <- function(side, at=NULL, labels=TRUE, tick=TRUE, line=0, pos=NA,
                 outer=FALSE, font=NA, vfont=NULL, ...) {
    if (!is.null(vfont))
	vfont <- c(typeface = pmatch(vfont[1], Hershey$typeface) - 1,
		   fontindex= pmatch(vfont[2], Hershey$fontindex))
    .Internal(axis(side, at, labels, tick, line, pos, outer, font, vfont, ...))
}
