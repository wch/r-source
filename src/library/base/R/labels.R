labels <- function(object, ...) UseMethod("labels")

labels.default <- function(object, ...)
{
    if(length(d <- dim(object))) {	# array or data frame
	nt <- dimnames(object)
	if(is.null(nt)) nt <- vector("list", length(d))
	for(i in 1:length(d))
	    if(!length(nt[[i]])) nt[[i]] <- as.character(seq(length = d[i]))
    } else {
	nt <- names(object)
	if(!length(nt)) nt <- as.character(seq(along = object))
    }
    nt
}

labels.terms <- function(object, ...) attr(object, "term.labels")

labels.lm <- function(object, ...)
{
    tl <- attr(object$terms, "term.labels")
    asgn <- object$assign[object$qr$pivot[1:object$rank]]
    tl[unique(asgn)]
}
