rug <- function(x, ticksize = 0.03, side = 1, lwd = 0.5) {
    x <- as.vector(x)
    ok <- is.finite(x)
    x <- x[ok]
    oldtick <- par(tck = ticksize)
    on.exit(par(oldtick))
    usr <- par("usr")
    usr <- if (side %% 2 == 1)  usr[1:2] else usr[3:4]
    if(any(x < usr[1] | x > usr[2]))
        warning("some values will be clipped")
    axis(side, at = x, lab = FALSE, lwd = lwd)
}
