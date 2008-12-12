#  File src/library/base/R/matrix.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

matrix <- function(data=NA, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL)
{
    data <- as.vector(data)
    if(missing(nrow))
        nrow <- ceiling(length(data)/ncol)
    else if(missing(ncol))
        ncol <- ceiling(length(data)/nrow)
    .Internal(matrix(data, nrow, ncol, byrow, dimnames))
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
        nr <- NROW(x)
	if(do.NULL) NULL
        else if(nr > 0) paste(prefix, seq_len(nr), sep="")
        else character(0)
    }
}

`rownames<-` <- function(x, value)
{
    if(is.data.frame(x)) {
        row.names(x) <- value
    } else {
        dn <- dimnames(x)
        if(is.null(dn)) {
            if(is.null(value)) return(x)
            if((nd <- length(dim(x))) < 1)
                stop("attempt to set rownames on object with no dimensions")
            dn <- vector("list", nd)
        }
        if(length(dn) < 1)
            stop("attempt to set rownames on object with no dimensions")
        if(is.null(value)) dn[1] <- list(NULL) else dn[[1]] <- value
        dimnames(x) <- dn
    }
    x
}

colnames <- function(x, do.NULL = TRUE, prefix = "col")
{
    if(is.data.frame(x) && do.NULL)
	return(names(x))
    dn <- dimnames(x)
    if(!is.null(dn[[2]]))
	dn[[2]]
    else {
        nc <- NCOL(x)
	if(do.NULL) NULL
        else if(nc > 0) paste(prefix, seq_len(nc), sep="")
        else character(0)
    }
}

`colnames<-` <- function(x, value)
{
    if(is.data.frame(x)) {
        names(x) <- value
    } else {
        dn <- dimnames(x)
        if(is.null(dn)) {
            if(is.null(value)) return(x)
            if((nd <- length(dim(x))) < 2)
                stop("attempt to set colnames on object with less than two dimensions")
            dn <- vector("list", nd)
        }
        if(length(dn) < 2)
            stop("attempt to set colnames on object with less than two dimensions")
        if(is.null(value)) dn[2] <- list(NULL) else dn[[2]] <- value
        dimnames(x) <- dn
    }
    x
}

row <- function(x, as.factor=FALSE) {
    if(as.factor) {
        labs <- rownames(x, do.NULL=FALSE, prefix="")
        res <- factor(.Internal(row(dim(x))), labels=labs)
        dim(res) <- dim(x)
        res
    } else .Internal(row(dim(x)))
}

col <- function(x, as.factor=FALSE) {
    if(as.factor) {
        labs <- colnames(x, do.NULL=FALSE, prefix="")
        res <- factor(.Internal(col(dim(x))), labels=labs)
        dim(res) <- dim(x)
        res
    } else .Internal(col(dim(x)))
}

crossprod <- function(x, y=NULL) .Internal(crossprod(x,y))
tcrossprod <- function(x, y=NULL) .Internal(tcrossprod(x,y))

t <- function(x) UseMethod("t")
## t.default is <primitive>
t.data.frame<- function(x)
{
    x <- as.matrix(x)
    NextMethod("t")
}
## as.matrix  is in "as"
