tabulate <- function(bin, nbins = max(bin))
{
    if(!is.numeric(bin) && !is.factor(bin))
	stop("tabulate: bin must be numeric or a factor")
    bin <- as.integer(if((n <- length(bin)) == 0) 1 else bin)
    .C("tabulate",
       bin,
       n,
       ans = integer(nbins))$ans
}
