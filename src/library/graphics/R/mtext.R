mtext <-
function (text, side = 3, line = 0, outer = FALSE, at = NA,
	  adj = NA, padj = NA, cex = NA, col = NA, font = NA,
          vfont = NULL, ...)
{
    if (!is.null(vfont))
	vfont <- c(typeface = pmatch(vfont[1], Hershey$typeface) - 1,
		   fontindex= pmatch(vfont[2], Hershey$fontindex))
    .Internal(mtext(text, side, line, outer, at, adj, padj,
                    cex, col, font, vfont, ...))
}
##> do_mtext in ../../../main/plot.c
