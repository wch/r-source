pmax <- function (..., na.rm = FALSE)
{
    elts <- list(...)
    maxmm <- as.vector(elts[[1]])
    for (each in elts[-1]) {
	work <- cbind(maxmm, as.vector(each))
	nas <- is.na(work)
	work[,1][nas[,1]] <- work[,2][nas[,1]]
	work[,2][nas[,2]] <- work[,1][nas[,2]]
	change <- work[,1] < work[,2]
	work[,1][change] <- work[,2][change]
	if (!na.rm) work[,1][nas[,1]+nas[,2] > 0] <- NA
	maxmm <- work[,1]
    }
    attributes(maxmm) <- attributes(elts[[1]])
    maxmm
}
