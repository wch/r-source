rect <-
    function(xleft, ybottom, xright, ytop,
	     col=NULL, border=par("fg"), lty=NULL, lwd=par("lwd"), xpd=FALSE)
    .Internal(rect(as.double(xleft),
                   as.double(ybottom),
                   as.double(xright),
                   as.double(ytop),
                   col=col, border=border,
                   lty=lty, lwd=lwd, xpd=xpd))

