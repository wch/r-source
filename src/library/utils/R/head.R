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
    x[seq_len(n)]
}

## head.matrix and tail.matrix are now exported (to be used for other classes)
head.data.frame <- head.matrix <- function(x, n = 6, ...)
{
    stopifnot(length(n) == 1)
    n <- if (n < 0) max(nrow(x) + n, 0) else min(n, nrow(x))
    x[seq_len(n), , drop=FALSE]
}
head.table  <- function(x, n = 6, ...) {
    (if(length(dim(x)) == 2) head.matrix else head.default)(x, n=n)
}

head.ftable <- function(x, n = 6, ...) {
    r <- format(x)
    dimnames(r) <- list(rep.int("", nrow(r)), rep.int("", ncol(r)))
    noquote(head.matrix(r, n = n + nrow(r) - nrow(x), ...))
}

head.function <- function(x, n = 6, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq_along(lines),"")
    noquote(head(lines, n=n))
}

tail <- function(x, ...) UseMethod("tail")

tail.default <- function(x, n = 6, ...)
{
    stopifnot(length(n) == 1)
    xlen <- length(x)
    n <- if (n < 0) max(xlen + n, 0) else min(n, xlen)
    x[seq.int(to = xlen, length = n)]
}

tail.data.frame <- function(x, n = 6, ...)
{
    stopifnot(length(n) == 1)
    nrx <- nrow(x)
    n <- if (n < 0) max(nrx + n, 0) else min(n, nrx)
    x[seq.int(to = nrx, length = n), , drop = FALSE]
}

tail.matrix <- function(x, n = 6, addrownums = TRUE, ...)
{
    stopifnot(length(n) == 1)
    nrx <- nrow(x)
    n <- if (n < 0) max(nrx + n, 0) else min(n, nrx)
    sel <- seq.int(to = nrx, length = n)
    ans <- x[sel, , drop = FALSE]
    if (addrownums && is.null(rownames(x)))
    	rownames(ans) <- paste("[", sel, ",]", sep="")
    ans
}
tail.table  <- function(x, n = 6, addrownums = TRUE, ...) {
    (if(length(dim(x)) == 2) tail.matrix else tail.default)(x, n=n,
	      addrownums = addrownums, ...)
}

tail.ftable <- function(x, n = 6, addrownums = FALSE, ...) {
    r <- format(x)
    dimnames(r) <- list(if(!addrownums) rep.int("", nrow(r)),
			rep.int("", ncol(r)))
    noquote(tail.matrix(r, n = n, addrownums = addrownums, ...))
}

tail.function <- function(x, n = 6, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq_along(lines),"")
    noquote(tail(lines, n=n))
}
