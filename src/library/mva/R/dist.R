dist <- function(x, method="euclidean", diag=FALSE, upper=FALSE)
{
    ## account for possible spellings of euclid?an
    if(!is.na(pmatch(method, "euclidian")))
	method <- "euclidean"

    METHODS <- c("euclidean", "maximum",
		 "manhattan", "canberra", "binary")
    method <- pmatch(method, METHODS)
    if(is.na(method))
	stop("invalid distance method")
    if(method == -1)
	stop("ambiguous distance method")

    N <- nrow(x <- as.matrix(x))
    d <- .C("R_distance",
	    x = as.double(x),
	    nr= N,
	    nc= ncol(x),
	    d = double(N*(N - 1)/2),
	    diag  = as.integer(FALSE),
	    method= as.integer(method),
	    DUP = FALSE, NAOK=TRUE, PACKAGE="mva")$d
    attr(d, "Size") <- N
    attr(d, "Labels") <- dimnames(x)[[1]]
    attr(d, "Diag") <- diag
    attr(d, "Upper") <- upper
    attr(d, "method") <- METHODS[method]
    attr(d, "call") <- match.call()
    class(d) <- "dist"
    return(d)
}

names.dist <- function(x) attr(x, "Labels")

"names<-.dist" <- function(x, value)
{
    if(length(value) != attr(x, "Size"))
	stop("invalid names for dist object")
    attr(x, "Labels") <- value
    x
}

## Because names(d) != length(d) for "dist"-object d, we need
format.dist <- function(x, ...) format(as.vector(x), ...)

as.matrix.dist <- function(x)
{
    size <- attr(x, "Size")
    df <- matrix(0, size, size)
    df[row(df) > col(df)] <- x
    df <- df + t(df)
    labels <- attr(x, "Labels")
    dimnames(df) <-
	if(is.null(labels)) list(1:size,1:size) else list(labels,labels)
    df
}


as.dist <- function(m, diag = FALSE, upper = FALSE)
{
    if (inherits(m,"dist"))
	ans <- m
    else { ## matrix |-> dist
	m <- as.matrix(m)
	ans <- m[row(m) > col(m)]
	attributes(ans) <- NULL
	if(!is.null(rownames(m)))
	    attr(ans,"Labels") <- rownames(m)
	else if(!is.null(colnames(m)))
	    attr(ans,"Labels") <- colnames(m)
	attr(ans,"Size") <- nrow(m)
	attr(ans, "call") <- match.call()
	class(ans) <- "dist"
    }
    if(is.null(attr(ans,"Diag")) || !missing(diag))
	attr(ans,"Diag") <- diag
    if(is.null(attr(ans,"Upper")) || !missing(upper))
	attr(ans,"Upper") <- upper
    ans
}


print.dist <- function(x, diag = NULL, upper = NULL, ...)
{
    if(is.null(diag))
	diag <-	 if(is.null(a <- attr(x, "Diag"))) FALSE else a
    if(is.null(upper))
	upper <- if(is.null(a <- attr(x,"Upper"))) FALSE else a

    size <- attr(x, "Size")
    df <- as.matrix.dist(x)
    if(!upper)
	df[row(df) < col(df)] <- NA
    if(!diag)
	df[row(df) == col(df)] <- NA
    print(if(diag || upper) df else df[-1, -size], na = "", ...)
    invisible(x)
}
