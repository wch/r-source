qqnorm <- function(y, ylim, main="Normal Q-Q Plot",
	xlab="Theoretical Quantiles", ylab="Sample Quantiles", ...) {
	y <- y[!is.na(y)]
	if(missing(ylim)) ylim <- c(min(y),max(y))
	x <- (1:length(y)-0.5)/length(y)
	plot(qnorm(x), sort(y), main=main ,xlab=xlab, ylab=ylab, ylim=ylim, ...)
}

qqline <- 
function(y, ...)
{
	y <- quantile(y[!is.na(y)],c(0.25, 0.75)) 
	x <- qnorm(c(0.25, 0.75))
	slope <- diff(y)/diff(x)
	int <- y[1]-slope*x[1]
	abline(int, slope, ...)    
}
