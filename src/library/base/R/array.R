array <-
function(data = NA, dim = length(data), dimnames = NULL)
{
    data <- as.vector(data)
    vl <- prod(dim)
    if( length(data) != vl  ) {
	t1 <- ceiling(vl/length(data))
	data <- rep(data,t1)
	if( length(data) != vl )
	    data <- data[1:vl]
    }
    if(length(dim))
	dim(data) <- dim
    if(is.list(dimnames) && length(dimnames))
	dimnames(data) <- dimnames
    data
}

slice.index <-
function(x, MARGIN)
{
    d <- dim(x)
    if(is.null(d))
        d <- length(x)
    n <- length(d)

    if((length(MARGIN) > 1) || (MARGIN < 1) || (MARGIN > n))
        stop("incorrect value for MARGIN")

    if(any(d == 0)) return(array(integer(0), d))
    
    y <- rep(rep(seq(1 : d[MARGIN]),
                 prod(d[seq(length = MARGIN - 1)]) * rep(1, d[MARGIN])),
             prod(d[seq(from = MARGIN + 1, length = n - MARGIN)]))
    dim(y) <- d
    y
}
