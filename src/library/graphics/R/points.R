points <- function(x, ...) UseMethod("points")

### NOTE: cex = 1 is correct, cex = par("cex") gives *square* of intended!

points.default <-
    function(x, y=NULL, type="p", pch=par("pch"), col=par("col"), bg=NA,
             cex=1, ...)
{
    plot.xy(xy.coords(x,y), type=type, pch=pch, col=col, bg=bg, cex=cex,...)
}
