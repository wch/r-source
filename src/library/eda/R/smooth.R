smooth <- function(x)
{
    if(!is.numeric(x))
	stop("attempt to smooth non-numeric values")
    if(any(is.na(x)))
	stop("attempt to smooth NA values")
    n <- length(x)
    smo <- .C("tukeysmooth",
	      as.double(x),
	      y = double(n),
	      double(n),
	      double(n),
	      n,
	      DUP=FALSE, PACKAGE="eda") $ y
    if(is.ts(x))
	smo <- ts(smo, start=start(x), freq=frequency(x))
    smo
}
