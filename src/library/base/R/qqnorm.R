qqnorm <- function(y, ...) UseMethod("qqnorm")

qqnorm.default <-
    function(y, ylim, main="Normal Q-Q Plot",
	     xlab="Theoretical Quantiles", ylab="Sample Quantiles",
	     plot.it=TRUE, ...)
{
    y <- y[!is.na(y)]
    if(0 == (n <- length(y))) stop("y is empty")
    if (missing(ylim)) ylim <- range(y)
    x <- qnorm(ppoints(n))[order(order(y))]
    if(plot.it)
	plot(x, y, main= main, xlab= xlab, ylab= ylab, ylim= ylim, ...)
    invisible(list(x = x, y = y))
}

## Splus also has qqnorm.aov(), qqnorm.aovlist(), qqnorm.maov() ...

qqline <- function(y, ...)
{
    y <- quantile(y[!is.na(y)],c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1]-slope*x[1]
    abline(int, slope, ...)
}
