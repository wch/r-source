sample <- function(x, size, replace=FALSE)
{
	if(length(x) == 1 && x >= 1) {
		if(missing(size)) size <- x
		.Internal(sample(x, size, replace))
	}
	else {
		if(missing(size)) size <- length(x)
		x[.Internal(sample(length(x), size, replace))]
	}
}
