xinch <- function(x=1, warn.log=TRUE) {
    if(warn.log && par("xlog")) warning("x log scale:  xinch() is non-sense")
    x * diff(par("usr")[1:2])/par("pin")[1]
}
yinch <- function(y=1, warn.log=TRUE) {
    if(warn.log && par("ylog")) warning("y log scale:  yinch() is non-sense")
    y * diff(par("usr")[3:4])/par("pin")[2]
}

xyinch <- function(xy=1, warn.log=TRUE) {
    if(warn.log && (par("xlog") || par("ylog")))
	warning("log scale:  xyinch() is non-sense")
    u <- par("usr"); xy * c(u[2]-u[1], u[4]-u[3]) / par("pin")
}
