rank <- function(x, na.last = TRUE,
                 ties.method=c("average", "first", "random", "max", "min"))
{
    nas <- is.na(x)
    ties.method <- match.arg(ties.method)
    y <- switch(ties.method,
                "average"= , "min"= , "max" =
                .Internal(rank(   x[!nas], ties.method)),
                "first" = sort.list(sort.list(x[!nas])),
                "random" = sort.list(order(   x[!nas],
                stats::runif(sum(!nas)))))
    if(!is.na(na.last) && any(nas)) {
	## the internal code has ranks in [1, length(y)]
	storage.mode(x) <- "double"
	NAkeep <- (na.last == "keep")
	if(NAkeep || na.last) {
	    x[!nas] <- y
	    if(!NAkeep) x[nas] <- (length(y) + 1L):length(x)
	} else {
	    len <- sum(nas)
	    x[!nas] <- y + len
	    x[nas] <- 1 : len
	}
	y <- x
    } else names(y) <- names(x)[!nas]
    y
}
