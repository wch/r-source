matrix <- function(data=NA, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL) {
    data <- as.vector(data)
    if(missing(nrow))
        nrow <- ceiling(length(data)/ncol)
    else if(missing(ncol))
        ncol <- ceiling(length(data)/nrow)
    x <- .Internal(matrix(data, nrow, ncol, byrow))
    dimnames(x) <- dimnames
    x
}
nrow <- function(x) dim(x)[1]
ncol <- function(x) dim(x)[2]

NROW <- function(x) if(is.array(x)||is.data.frame(x)) nrow(x) else length(x)
NCOL <- function(x) if(is.array(x) && length(dim(x)) > 1||is.data.frame(x)) ncol(x) else as.integer(1)

rownames <- function(x, do.NULL = TRUE, prefix = "row")
{
    dn <- dimnames(x)
    if(!is.null(dn[[1]]))
	dn[[1]]
    else {
	if(do.NULL) NULL else paste(prefix, seq(length=NROW(x)), sep="")
    }
}
"rownames<-" <- function(x, value) {
    dn <- dimnames(x)
    dimnames(x) <- list(value, if(!is.null(dn)) dn[[2]])
    x
}
colnames <- function(x, do.NULL = TRUE, prefix = "col")
{
    dn <- dimnames(x)
    if(!is.null(dn[[2]]))
	dn[[2]]
    else {
	if(do.NULL) NULL else paste(prefix, seq(length=NCOL(x)), sep="")
    }
}
"colnames<-" <- function(x, value) {
    dn <- dimnames(x)
    dimnames(x) <- list(if(!is.null(dn)) dn[[1]], value)
    x
}

row <- function(x, as.factor=FALSE) {
    if(as.factor) factor(.Internal(row(x)), labels=rownames(x))
    else .Internal(row(x))
}

col <- function(x, as.factor=FALSE) {
    if(as.factor) factor(.Internal(col(x)), labels=colnames(x))
    else .Internal(col(x))
}

crossprod <- function(x, y=x) .Internal(crossprod(x,y))

t <- function(x) UseMethod("t")
## t.default is <primitive>
t.data.frame<- function(x)
{
    x <- as.matrix(x)
    NextMethod("t")
}
## as.matrix  is in "as"
