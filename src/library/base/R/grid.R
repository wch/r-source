grid <- function (nx=NULL, ny=NULL, col="lightgray", lty="dotted")
{
    if (is.null(nx)|| nx >= 1) {
        axp <- par("xaxp")
        if(is.null(nx)) nx <- axp[3]
        at <- if(par("xlog")) 10^seq(log10(axp[1]), log10(axp[2]), len= 1+nx)
            else seq(axp[1],axp[2],len=1+nx)
	abline(v = at, col = col, lty = lty)
    }
    if (is.null(ny)|| ny >= 1) {
        axp <- par("yaxp")
        if(is.null(ny)) ny <- axp[3]
        at <- if(par("ylog")) 10^seq(log10(axp[1]), log10(axp[2]), len= 1+ny)
            else seq(axp[1],axp[2],len=1+ny)
	abline(h = at, col = col, lty = lty)
    }
}
