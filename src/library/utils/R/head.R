### placed in the public domain 2002
### Patrick Burns patrick@burns-stat.com
###
### Adapted for negative arguments by Vincent Goulet
### <vincent.goulet@act.ulaval.ca>, 2006

head <- function(x, ...) UseMethod("head")

head.default <- function(x, n = 6, ...)
{
    stopifnot(length(n) == 1)
    n <- if (n < 0) max(length(x) + n, 0) else min(n, length(x))
    x[seq(len = n)]
}

## head.matrix and tail.matrix are now exported (to be used for other classes)
head.matrix <- function(x, n = 6, ...)
{
    stopifnot(length(n) == 1)
    n <- if (n < 0) max(nrow(x) + n, 0) else min(n, nrow(x))
    x[seq(length = n), , drop=FALSE]
}
head.data.frame <- head.table <- head.matrix

head.ftable <- function(x, n = 6, ...) head.matrix(format(x), n = n, ...)

head.function <- function(x, n = 6, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq(along=lines),"")
    noquote(head(lines, n=n))
}

tail <- function(x, ...) UseMethod("tail")

tail.default <- function(x, n = 6, ...)
{
    stopifnot(length(n) == 1)
    xlen <- length(x)
    n <- if (n < 0) max(xlen + n, 0) else min(n, xlen)
    x[seq(to = xlen, length = n)]
}

tail.data.frame <- function(x, n = 6, ...)
{
    stopifnot(length(n) == 1)
    nrx <- nrow(x)
    n <- if (n < 0) max(nrx + n, 0) else min(n, nrx)
    x[seq(to = nrx, length = n), , drop = FALSE]
}

tail.matrix <- function(x, n = 6, addrownums = TRUE, ...)
{
    stopifnot(length(n) == 1)
    nrx <- nrow(x)
    n <- if (n < 0) max(nrx + n, 0) else min(n, nrx)
    sel <- seq(to = nrx, length = n)
    ans <- x[sel, , drop = FALSE]
    if (addrownums && is.null(rownames(x)))
    	rownames(ans) <- paste("[", sel, ",]", sep="")
    ans
}
tail.table <- tail.matrix

tail.ftable <- function(x, n = 6, ...) tail.matrix(format(x), n = n, ...)

tail.function <- function(x, n = 6, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq(along=lines),"")
    noquote(tail(lines, n=n))
}
