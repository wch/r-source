dist <- function(x, method="euclidian", diag=FALSE, upper=FALSE)
{
    method <- pmatch(method, c("euclidian", "maximum",
			       "manhattan", "canberra", "binary"))
    if(is.na(method))
	stop("invalid distance method")
    if(method == -1)
	stop("ambiguous distance method")

    N <- nrow(x <- as.matrix(x))
    d <- .C("distance",
	    x = as.double(x),
	    nr= N,
	    nc= ncol(x),
	    d = double(N*(N - 1)/2),
            diag  = as.integer(1),
	    method= as.integer(method)) $ d
    attr(d, "Size") <- N
    attr(d, "Labels") <- dimnames(x)[[1]]
    attr(d, "Diag") <- diag
    attr(d, "Upper") <- upper
    class(d) <- "dist"
    return(d)
}

names.dist <- function(d) attr(d, "Labels")

"names<-.dist" <- function(d, n)
{
    if(length(n) != attr(d, "Size"))
	stop("invalid names for dist object")
    attr(d, "Labels") <- n
    d
}

as.matrix.dist <- function(obj)
{
    size <- attr(obj, "Size")
    df <- matrix(0, size, size)
    df[row(df) > col(df)] <- obj
    df <- df + t(df)
    labels <- attr(obj, "Labels")
    dimnames(df) <-
	if(is.null(labels)) list(1:size,1:size) else list(labels,labels)
    df
}

print.dist <- function(obj, diag=NULL, upper=NULL)
{
    if(is.null(diag))
	diag <- if(is.null(attr(obj, "Diag"))) FALSE else attr(obj, "Diag")
    if(is.null(upper))
	upper <- if(is.null(attr(obj,"Upper")))FALSE else attr(obj, "Upper")

    size <- attr(obj, "Size")
    df <- as.matrix(obj)
    if(!upper)
	df[row(df) < col(df)] <- NA
    if(!diag)
	df[row(df) == col(df)] <- NA
    print(if(diag || upper) df else df[-1,-size], na="")
    invisible(obj)
}


