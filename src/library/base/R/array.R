array <-
function(data = NA, dim = length(data), dimnames = NULL)
{
    data <- as.vector(data)
    vl <- prod(dim)
    if(length(data) != vl) {
        if(vl > .Machine$integer.max)
            stop("'dim' specifies too large an array")
        data <- rep(data, length.out=vl)
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
        stop("incorrect value for 'MARGIN'")

    if(any(d == 0)) return(array(integer(0), d))

    y <- rep.int(rep.int(1:d[MARGIN],
			 prod(d[seq_len(MARGIN - 1)]) * rep.int(1, d[MARGIN])),
		 prod(d[seq.int(from = MARGIN + 1, length.out = n - MARGIN)]))
    dim(y) <- d
    y
}
