tabulate <- function(bin, nbins = max(bin))
{
	if(!is.numeric(bin) && !is.factor(bin))
		stop("tabulate: bin must be numeric or a factor")
	if((n <- length(bin)) == 0) bin <- 1
	else bin <- as.integer(bin)
	.C("tabulate",
		ans = integer(nbins),
		bin,
		n)$ans
}
