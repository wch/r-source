grid <- function (nx=NULL, ny=NULL, col="lightgray", lty="dotted")
{
    if (is.null(nx)|| nx >= 1) {
        axp <- par("xaxp")
        if(is.null(nx)) nx <- axp[3]
	abline(v = seq(axp[1],axp[2],len=1+nx), col = col, lty = lty)
    }
    if (is.null(ny)|| ny >= 1) {
        axp <- par("yaxp")
        if(is.null(ny)) ny <- axp[3]
	abline(h = seq(axp[1],axp[2],len=1+ny), col = col, lty = lty)
    }
}
