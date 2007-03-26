sample <- function(x, size, replace=FALSE, prob=NULL)
{
    if(length(x) == 1 && x >= 1) {
	if(missing(size)) size <- x
	.Internal(sample(x, size, replace, prob))
    }
    else {
	if(missing(size)) size <- length(x)
	x[.Internal(sample(length(x), size, replace, prob))]
    }
}
