diag <-
function(x = 1, nrow, ncol = n)
{
	if(is.matrix(x) && nargs() == 1)
		return(as.matrix(x)[1 + 0:(min(dim(x)) - 1) * (dim(x)[1] + 1)])
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
	y[1 + 0:(min(n, p) - 1) * (n + 1)] <- x
	y
}

"diag<-" <-
function(x, value)
{
	dx <- dim(x)
	if(length(dx) != 2 || prod(dx) != length(x))
		stop("only matrix diagonals can be replaced")
	i <- 1:min(dx)
	if(length(value) != 1 && length(value) != length(i))
		stop("replacement diagonal has wrong length")
	x[cbind(i, i)] <- value
	x
}
