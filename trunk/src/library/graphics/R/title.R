title <- function(main=NULL, sub=NULL, xlab=NULL, ylab=NULL,
                  line=NA, outer=FALSE, ...)
{
    main <- as.graphicsAnnot(main)
    sub <- as.graphicsAnnot(sub)
    xlab <- as.graphicsAnnot(xlab)
    ylab <- as.graphicsAnnot(ylab)
    .Internal(title(main, sub, xlab, ylab, line, outer, ...))
}
