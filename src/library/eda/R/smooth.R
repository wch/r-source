smooth <- function(x)
{
	if(!is.numeric(x))
		stop("attempt to smooth non-numeric values")
	if(any(is.na(x)))
		stop("attempt to smooth NA values")
	n <- length(x)
	smo <- .C("tukeysmooth",
		as.double(x),
		double(n),
		double(n),
		double(n),
		n,
		double(1),
		DUP=FALSE)[[2]]
	if(is.ts(x))
		smo <- ts(smo, start=start(x), freq=frequency(x))
	smo
}
