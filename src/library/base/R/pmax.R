### pmax() & pmin() only differ by name and ONE character :

pmax <- function (..., na.rm = FALSE)
{
    elts <- list(...)
    mmm <- as.vector(elts[[1]])
    has.na <- FALSE
    for (each in elts[-1]) {
	work <- cbind(mmm, as.vector(each)) # recycling..
        nas <- is.na(work)
	if(has.na || (has.na <- any(nas))) {
            work[,1][nas[,1]] <- work[,2][nas[,1]]
            work[,2][nas[,2]] <- work[,1][nas[,2]]
        }
        change <- work[,1] < work[,2]
        change <- change & !is.na(change)
	work[,1][change] <- work[,2][change]
	if (has.na && !na.rm) work[,1][nas[,1] | nas[,2]] <- NA
	mmm <- work[,1]
    }
    mostattributes(mmm) <- attributes(elts[[1]])
    mmm
}

pmin <- function (..., na.rm = FALSE)
{
    elts <- list(...)
    mmm <- as.vector(elts[[1]])
    has.na <- FALSE
    for (each in elts[-1]) {
	work <- cbind(mmm, as.vector(each)) # recycling..
        nas <- is.na(work)
	if(has.na || (has.na <- any(nas))) {
            work[,1][nas[,1]] <- work[,2][nas[,1]]
            work[,2][nas[,2]] <- work[,1][nas[,2]]
        }
	change <- work[,1] > work[,2]
        change <- change & !is.na(change)
	work[,1][change] <- work[,2][change]
	if(has.na && !na.rm) work[,1][nas[,1] | nas[,2]] <- NA
	mmm <- work[,1]
    }
    mostattributes(mmm) <- attributes(elts[[1]])
    mmm
}
