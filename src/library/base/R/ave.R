ave <- function (x, ..., FUN = mean)
{
    l <- list(...)
    if (is.null(l)) {
	x[] <- FUN(x)
    }
    else {
	g <- 1
	nlv <- 1
	for (i in 1:length(l)) {
	    l[[i]] <- li <- as.factor(l[[i]])
	    g <- g + nlv * (as.numeric(li) - 1)
	    nlv <- nlv * length(levels(li))
	}
	x[] <- unlist(lapply(split(x, g), FUN))[g]
    }
    x
}
