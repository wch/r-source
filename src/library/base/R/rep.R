if(FALSE) {
rep <- function(x, times, ...) UseMethod("rep")

rep.default <- function(x, times, length.out, each, ...)
{
    if (length(x) == 0)
        return(if(missing(length.out)) x else x[seq_len(length.out)])
    if (!missing(each)) {
        tm <- .Internal(rep.int(each, length(x)))
        nm <- names(x)
        x <- .Internal(rep.int(x, tm))
        if(!is.null(nm)) names(x) <- .Internal(rep.int(nm, tm))
        if(missing(length.out) && missing(times)) return(x)
    }
    if (!missing(length.out)) # takes precedence over times
	times <- ceiling(length.out/length(x))
    r <- .Internal(rep.int(x, times))
    if(!is.null(nm <- names(x))) names(r) <- .Internal(rep.int(nm, times))
    if (!missing(length.out))
	return(r[if(length.out > 0) 1:length.out else integer(0)])
    return(r)
}
}

rep.int <- function(x, times) .Internal(rep.int(x, times))

rep.factor <- function(x, ...)
{
    y <- NextMethod()
    structure(y, class=class(x), levels=levels(x))
}
