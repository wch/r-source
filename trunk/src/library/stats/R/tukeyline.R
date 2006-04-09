line <- function(x, y=NULL)
{
    xy <- xy.coords(x, y)
    ok <- complete.cases(xy$x,xy$y)
    n <- length(ok)
    if(n <= 1) stop("insufficient observations")
    z <- .C("tukeyline",
	    as.double(xy$x[ok]),
	    as.double(xy$y[ok]),
	    double(n),
	    double(n),
	    n,
	    double(2),
	    DUP=FALSE, PACKAGE="stats")
    value <- list(call=sys.call(), coefficients = z[[6]],
                  residuals = z[[3]], fitted.values = z[[4]])
    class(value) <- "tukeyline"
    value
}
#coef.tukeyline <- coef.lm
residuals.tukeyline <- residuals.lm
# fitted.tukeyline <- fitted.lm
print.tukeyline <- print.lm
