data.matrix <- function(frame)
{
    if(!is.data.frame(frame))
	return(as.matrix(frame))
    d <- dim(frame)
    if(all(d > 0)) {
	log <- unlist(lapply(frame, is.logical))
	num <- unlist(lapply(frame, is.numeric))
	fac <- unlist(lapply(frame, is.factor))

	if(!all(log|fac|num))
	    stop("non-numeric data type in frame")
        cl <- sapply(frame[log|num], function(x) {
            cl <- class(x)
            length(cl) > 1 || ! (cl %in% c("numeric", "integer", "logical"))
        })
        if(length(cl) && any(cl))
            warning("class information lost from one or more columns")
    }
    x <- matrix(nr=d[1], nc=d[2], dimnames=dimnames(frame))
    for(i in seq(len=d[2])) {
	xi <- frame[[i]]
	x[,i] <-
	    if(is.logical(xi) || is.factor(xi)) as.numeric(xi) else xi
    }
    x
}
