diag <-
    function(x = 1, nrow, ncol = n)
{
    if(is.matrix(x) && nargs() == 1)
        if((m <- min(dim(x))) > 0)
            return(c(x)[1 + 0:(m - 1) * (dim(x)[1] + 1)])
        else return(numeric(0))
    if(missing(x))
	n <- nrow
    else if(length(x) == 1 && missing(nrow) && missing(ncol)) {
	n <- as.integer(x)
	x <- 1
    }
    else n <- length(x)
    if(!missing(nrow))
	n <- nrow
    p <- ncol
    y <- array(0, c(n, p))
    if((m <- min(n, p)) > 0) y[1 + 0:(m - 1) * (n + 1)] <- x
    y
}

"diag<-" <-
    function(x, value)
{
    dx <- dim(x)
    if(length(dx) != 2 || prod(dx) != length(x))
	stop("only matrix diagonals can be replaced")
    i <- seq(length=min(dx))
    if(length(value) != 1 && length(value) != length(i))
	stop("replacement diagonal has wrong length")
    x[cbind(i, i)] <- value
    x
}
