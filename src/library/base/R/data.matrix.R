data.matrix <-
    function(frame)
{
    if(!is.data.frame(frame))
	return(as.matrix(frame))
    log <- unlist(lapply(frame, is.logical))
    num <- unlist(lapply(frame, is.numeric))
    fac <- unlist(lapply(frame, is.factor))

    if(!all(log|fac|num))
	stop("non-numeric data type in frame")

    d <- dim(frame)
    x <- matrix(nr=d[1],nc=d[2],dimnames=dimnames(frame))
    for(i in 1:length(frame)) {
	xi <- frame[[i]]
	if(is.logical(xi)) x[,i] <- as.numeric(xi)
	else if(is.numeric(xi)) x[,i] <- xi
	else x[,i] <- codes(xi)
    }
    x
}
