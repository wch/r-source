duplicated <- function(x, incomparables = FALSE) UseMethod("duplicated")

duplicated.default <- function(x, incomparables = FALSE)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    .Internal(duplicated(x))
}

duplicated.data.frame <- function(x, incomparables = FALSE)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    duplicated(do.call("paste", c(x, sep="\r")))
}

unique <- function(x, incomparables = FALSE) UseMethod("unique")

unique.default <- function(x, incomparables = FALSE)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    z <- .Internal(unique(x))
    if(is.factor(x))
	z <- factor(z, levels = 1:nlevels(x), labels = levels(x))
    z
}

unique.data.frame <- function(x, incomparables = FALSE)
{
    if(!is.logical(incomparables) || incomparables)
	.NotYetUsed("incomparables != FALSE")
    x[!duplicated(x),  , drop = FALSE]
}
