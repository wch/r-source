"grid" <-
    function (nx=3, ny=3, col="lightgray", lty="dotted")
{
    lims <- par("usr")
    if (nx > 1) {
	coord <- seq(lims[1], lims[2], len = nx + 2)[c(-1, -(nx + 2))]
	abline(v = coord, col = col, lty = lty)
    }
    if (ny > 1) {
	coord <- seq(lims[3], lims[4], len = ny + 2)[c(-1, -(ny + 2))]
	abline(h = coord, col = col, lty = lty)
    }
}
