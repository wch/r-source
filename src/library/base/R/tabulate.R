tabulate <- function(bin, nbins = max(1,bin))
{
    if(!is.numeric(bin) && !is.factor(bin))
	stop("tabulate: bin must be numeric or a factor")
    .C("tabulate",
       as.integer(bin),
       as.integer(length(bin)),
       as.integer(nbins),
       ans = integer(nbins))$ans
}
