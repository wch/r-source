arrows <- function(x0, y0, x1, y1, length=0.25, angle=30, code=2,
		   col=par("fg"), lty=NULL, lwd=par("lwd"), xpd=NULL)
{
 .Internal(arrows(x0, y0,
		  x1, y1,
		  length=length,
		  angle=angle,
		  code=code,
		  col=col,
		  lty=lty,
		  lwd=lwd,
		  xpd=xpd))
}
