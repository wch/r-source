axis <- function(side, at=NULL, labels=TRUE, tick=TRUE, line=0, pos=NA,
                 outer=FALSE, ...)
    .Internal(axis(side, at, labels, tick, line, pos, outer, ...))
