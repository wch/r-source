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
	    DUP=FALSE, PACKAGE="eda")
    value <- list(call=sys.call(), coefficients = z[[6]],
                  residuals = z[[3]], fitted.values = z[[4]])
    class(value) <- "tukeyline"
    value
}
coef.tukeyline <- .Alias(coef.lm)
residuals.tukeyline <- .Alias(residuals.lm)
fitted.tukeyline <- .Alias(fitted.lm)
print.tukeyline <- .Alias(print.lm)
