duplicated <- function(x, incomparables = FALSE, ...) UseMethod("duplicated")

duplicated.default <- function(x, incomparables = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    .Internal(duplicated(x))
}

duplicated.data.frame <- function(x, incomparables = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    duplicated(do.call("paste", c(x, sep="\r")))
}

duplicated.matrix <- duplicated.array <-
    function(x, incomparables = FALSE , MARGIN = 1, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    ndim <- length(dim(x))
    if (length(MARGIN) > ndim || any(MARGIN > ndim))
        stop("MARGIN = ", MARGIN, " is invalid for dim = ", dim(x))
    temp <- apply(x, MARGIN, function(x) paste(x, collapse = "\r"))
    res <- duplicated(as.vector(temp))
    dim(res) <- dim(temp)
    dimnames(res) <- dimnames(temp)
    res
}

duplicated.list <- function(x, incomparables = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    .Internal(duplicated.list(x))
}

unique <- function(x, incomparables = FALSE, ...) UseMethod("unique")


## NB unique.default is used by factor to avoid unique.matrix,
## so it needs to handle some other cases.
unique.default <- function(x, incomparables = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    z <- .Internal(unique(x))
    if(is.factor(x))
	factor(z, levels = seq(len=nlevels(x)), labels = levels(x),
               ordered = is.ordered(x))
    else if(inherits(x, "POSIXct") || inherits(x, "Date"))
        structure(z, class=class(x))
    else z
}

unique.data.frame <- function(x, incomparables = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    x[!duplicated(x),  , drop = FALSE]
}

unique.matrix <- unique.array <-
    function(x, incomparables = FALSE , MARGIN = 1, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    ndim <- length(dim(x))
    if (length(MARGIN) > 1 || any(MARGIN > ndim))
        stop("MARGIN = ", MARGIN, " is invalid for dim = ", dim(x))
    temp <- apply(x, MARGIN, function(x) paste(x, collapse = "\r"))
    args <- rep(alist(a=), ndim)
    names(args) <- NULL
    args[[MARGIN]] <- !duplicated(as.vector(temp))
    do.call("[", c(list(x=x), args, list(drop=FALSE)))
}

unique.list <- function(x, incomparables = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    x[!duplicated(x)]
}

