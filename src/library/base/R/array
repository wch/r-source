array <- function(data = NA, dim = length(data), dimnames = NULL)
{
	data <- as.vector(data)
	vl <- prod(dim)
	if( length(data) != vl  ) {
		t1 <- ceiling(vl/length(data))
		data <- rep(data,t1)
		if( length(data) != vl )
			data <- data[1:vl]
	}
	dim(data) <- dim
	if(is.list(dimnames))
		dimnames(data) <- dimnames
	data
}
