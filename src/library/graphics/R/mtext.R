mtext <-
function (text, side = 3, line = 0, outer = FALSE, at = NA,
	  adj = NA, padj = NA, cex = NA, col = NA, font = NA, ...)
    .Internal(mtext(text, side, line, outer, at, adj, padj,
                    cex, col, font, ...))

