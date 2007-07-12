tabulate <- function(bin, nbins = max(1L, bin))
{
    if(!is.numeric(bin) && !is.factor(bin))
	stop("'bin' must be numeric or a factor")
    .C("R_tabulate",
       as.integer(bin),
       as.integer(length(bin)),
       as.integer(nbins),
       ans = integer(nbins),
       PACKAGE="base")$ans
}
