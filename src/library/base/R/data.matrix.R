data.matrix <- function(frame, rownames.force = NA)
{
    if(!is.data.frame(frame)) return(as.matrix(frame))

    d <- dim(frame)
    if(d[2] > 0) {
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
    rn <- if(rownames.force %in% FALSE) NULL
    else if(rownames.force %in% TRUE) row.names(frame)
    else {if(.row_names_info(frame) <= 0) NULL else row.names(frame)}
    x <- matrix(NA_integer_, nrow = d[1], ncol = d[2],
		dimnames = list(rn, names(frame)) )
    for(i in seq_len(d[2]) ) {
	xi <- frame[[i]]
	x[,i] <- if(is.logical(xi) || is.factor(xi)) as.integer(xi) else xi
    }
    x
}
