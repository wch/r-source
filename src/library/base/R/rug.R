rug <- function(x, ticksize = 0.03, side = 1, lwd = 0.5) {
    x <- as.vector(x)
    on.exit(par(oldtick))
    oldtick <- par(tck = ticksize)
    axis(side, at = x, lab = FALSE, lwd = lwd)
}
