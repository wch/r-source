forwardsolve <- function(l, x, k=ncol(l), upper.tri = FALSE, transpose = FALSE)
    backsolve(l,x, k=k, upper.tri= upper.tri, transpose= transpose)

backsolve <- function(r, x, k=ncol(r), upper.tri = TRUE, transpose = FALSE)
{
    r <- as.matrix(r)# nr  x  k
    storage.mode(r) <- "double"
    x.mat <- is.matrix(x)
    if(!x.mat) x <- as.matrix(x)# k  x	nb
    storage.mode(x) <- "double"
    k <- as.integer(k)
    if(k <= 0 || nrow(x) < k) stop("invalid parameters in backsolve")
    nb <- ncol(x)
    upper.tri <- as.logical(upper.tri)
    transpose <- as.logical(transpose)
    job <- as.integer((upper.tri) + 10*(transpose))
    z <- .C("bakslv",
	    t  = r, ldt= nrow(r), n  = k,
	    b  = x, ldb= k,	  nb = nb,
	    x  = matrix(0, k, nb),
	    job = job,
	    info = integer(1),
	    DUP = FALSE, PACKAGE = "base")[c("x","info")]
    if(z$info != 0)
	stop("singular matrix in backsolve. First zero in diagonal [", z$info,"]")
    if(x.mat) z$x else drop(z$x)
}
