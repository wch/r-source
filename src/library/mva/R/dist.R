dist <-
function(x, method="euclidian")
{
	method <-  pmatch(method, c("euclidian", "maximum",
		"manhattan", "canberra", "binary"))
	if(is.na(method))
		stop("invalid distance method")
	if(method == -1)
		stop("ambiguous distance method")

	x <- as.matrix(x)
	len <- nrow(x)*(nrow(x) - 1)/2

	d <- .C("dist",
		as.double(x),
		nrow(x),
		ncol(x),
		double(len),
		as.integer(method))[[4]]
	attr(d, "Size") <- nrow(x)
	attr(d, "Labels") <- dimnames(x)[[1]]
	class(d) <- "dist"
	return(d)
}

print.dist <-
function(d)
{
	size <- attr(d, "Size")
	df <- matrix(NA, size, size)
	df[row(df) > col(df)] <- d
	labels <- attr(d, "Labels")
	if(is.null(labels))
		dimnames(df) <- list(1:size,1:size)
	else
		dimnames(df) <- list(labels,labels)
	print(df[-1,-size], na="")
}

names.dist <-
function(d)
attr(d, "Labels")

"names<-.dist" <- function(d, n)
{
	if(length(n) != attr(d, "Size"))
		stop("invalid names for dist object")
	attr(d, "Labels") <- n
	d
}
