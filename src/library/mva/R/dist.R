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
	    DUP = FALSE, PACKAGE="base")$d
    attr(d, "Size") <- N
    attr(d, "Labels") <- dimnames(x)[[1]]
    attr(d, "Diag") <- diag
    attr(d, "Upper") <- upper
    attr(d, "method") <- METHODS[method]
    attr(d, "call") <- match.call()
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


as.dist <- function(m, diag = FALSE, upper=FALSE)
{
    m <- as.matrix(m)

    retval <-  m[row(m) > col(m)]

    attributes(retval) <- NULL

    if(!is.null(rownames(m)))
        attr(retval,"Labels") <- rownames(m)
    else if(!is.null(colnames(m)))
        attr(retval,"Labels") <- colnames(m)

    attr(retval,"Size") <- nrow(m)
    attr(retval,"Diag") <- diag
    attr(retval,"Upper") <- upper
    attr(retval, "call") <- match.call()
    class(retval) <- "dist"
    retval
}


print.dist <- function(obj, diag=NULL, upper=NULL)
{
    if(is.null(diag))
	diag <- if(is.null(attr(obj, "Diag"))) FALSE else attr(obj, "Diag")
    if(is.null(upper))
	upper <- if(is.null(attr(obj,"Upper")))FALSE else attr(obj, "Upper")

    size <- attr(obj, "Size")
    df <- as.matrix.dist(obj)
    if(!upper)
	df[row(df) < col(df)] <- NA
    if(!diag)
	df[row(df) == col(df)] <- NA
    print(if(diag || upper) df else df[-1,-size], na="")
    invisible(obj)
}


