Sys.getenv <- function(x) {
    if (missing(x)) {
	x <- strsplit(.Internal(getenv(character())), "=", fixed=TRUE)
	v <- n <- character(LEN <- length(x))
	for (i in 1:LEN) {
	    n[i] <- x[[i]][1]
	    v[i] <- paste(x[[i]][-1], collapse = "=")
	}
	structure(v, names = n)
    } else {
	structure(.Internal(getenv(x)), names = x)
    }
}

Sys.putenv <- function(...)
{
    x <- list(...)
    nm <- names(x)
    val <- as.character(unlist(x))
    x <- paste(nm,val, sep="=")
    invisible(.Internal(putenv(x)))
}

Sys.getpid <- function() .Internal(getpid())
