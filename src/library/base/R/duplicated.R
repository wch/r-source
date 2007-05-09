duplicated <- function(x, incomparables = FALSE, ...) UseMethod("duplicated")

duplicated.default <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    if(is.na(fromLast <- as.logical(fromLast[1])))
        stop("'fromLast' must be TRUE or FALSE")
    .Internal(duplicated(x, fromLast))
}

duplicated.data.frame <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    duplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
}

duplicated.matrix <- duplicated.array <-
    function(x, incomparables = FALSE , MARGIN = 1, fromLast = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    ndim <- length(dim(x))
    if (length(MARGIN) > ndim || any(MARGIN > ndim))
        stop("MARGIN = ", MARGIN, " is invalid for dim = ", dim(x))
    temp <- apply(x, MARGIN, function(x) paste(x, collapse = "\r"))
    res <- duplicated(as.vector(temp), fromLast = fromLast)
    dim(res) <- dim(temp)
    dimnames(res) <- dimnames(temp)
    res
}

unique <- function(x, incomparables = FALSE, ...) UseMethod("unique")


## NB unique.default is used by factor to avoid unique.matrix,
## so it needs to handle some other cases.
unique.default <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    if(is.na(fromLast <- as.logical(fromLast[1])))
        stop("'fromLast' must be TRUE or FALSE")
    z <- .Internal(unique(x, fromLast))
    if(is.factor(x))
	factor(z, levels = seq_len(nlevels(x)), labels = levels(x),
               ordered = is.ordered(x))
    else if(inherits(x, "POSIXct") || inherits(x, "Date"))

        structure(z, class=class(x))
    else z
}

unique.data.frame <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    x[!duplicated(x, fromLast = fromLast),  , drop = FALSE]
}

unique.matrix <- unique.array <-
    function(x, incomparables = FALSE , MARGIN = 1, fromLast = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    ndim <- length(dim(x))
    if (length(MARGIN) > 1 || any(MARGIN > ndim))
        stop("MARGIN = ", MARGIN, " is invalid for dim = ", dim(x))
    temp <- apply(x, MARGIN, function(x) paste(x, collapse = "\r"))
    args <- rep(alist(a=), ndim)
    names(args) <- NULL
    args[[MARGIN]] <- !duplicated(as.vector(temp), fromLast = fromLast)
    do.call("[", c(list(x=x), args, list(drop=FALSE)))
}
