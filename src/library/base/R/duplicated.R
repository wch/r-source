duplicated <- function(x, incomparables = FALSE, ...)
{
    if (!is.null(x) && is.null(class(x))) class(x) <- data.class(x)
    UseMethod("duplicated", x, incomparables, ...)
}

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
        stop(paste("MARGIN = ", MARGIN," is invalid for dim = ",
                   dim(x), sep = ""))
    temp <- apply(x, MARGIN, function(x) paste(x, collapse = "\r"))
    res <- duplicated(as.vector(temp))
    dim(res) <- dim(temp)
    dimnames(res) <- dimnames(temp)
    res
}

unique <- function(x, incomparables = FALSE, ...)
{
    if (!is.null(x) && is.null(class(x))) class(x) <- data.class(x)
    UseMethod("unique", x, incomparables, ...)
}

unique.default <- function(x, incomparables = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    z <- .Internal(unique(x))
    if(is.factor(x))
	z <- factor(z, levels = 1:nlevels(x), labels = levels(x))
    z
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
        stop(paste("MARGIN = ", MARGIN," is invalid for dim = ",
                   dim(x), sep = ""))
    temp <- apply(x, MARGIN, function(x) paste(x, collapse = "\r"))
    args <- rep(alist(a=), ndim)
    names(args) <- NULL
    args[[MARGIN]] <- !duplicated(as.vector(temp))
    do.call("[", c(list(x=x), args, list(drop=FALSE)))
}
