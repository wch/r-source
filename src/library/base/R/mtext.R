mtext <-
  function (text, side = 3, line = 0, outer = FALSE, at = NA,
            adj = NA, cex = NA, col = NA, font = NA, ...) 
  .Internal(mtext(as.char.or.expr(text), side, line, outer, at, 
                  adj, cex, col, font, ...))
##> ../../../main/plot.c
