### placed in the public domain 2002
### Patrick Burns patrick@burns-stat.com

head <- function(x, ...) UseMethod("head")

head.default <- function(x, n=6, ...)
{
    ans <- x[seq(len=min(n, length(x)))]
    if(length(dim(x)) == 1) array(ans, n, list(names(ans))) else ans
}

head.data.frame <- head.matrix <- function(x, n=6, ...)
    x[seq(len=min(n, nrow(x))), , drop=FALSE]

head.function <- function(x, n=6, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq(along=lines),"")
    noquote(head(lines, n=n))
}

tail <- function(x, ...) UseMethod("tail")

tail.default <- function(x, n=6, ...)
{
    xlen <- length(x)
    n <- min(n, xlen)
    ans <- x[seq(to=xlen, length=n)]
    if(length(dim(x)) == 1) array(ans, n, list(names(ans))) else ans
}

tail.data.frame <- tail.matrix <- function(x, n=6, ...)
{
    nrx <- nrow(x)
    x[seq(to=nrx, length=min(n, nrx)), , drop=FALSE]
}

tail.function <- function(x, n=6, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq(along=lines),"")
    noquote(tail(lines, n=n))
}
