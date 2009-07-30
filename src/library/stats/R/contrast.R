#  File src/library/stats/R/contrast.R
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


## This is also called from C : do_model_matrix() { ../../../main/model.c }:
contrasts <- function (x, contrasts = TRUE, sparse = FALSE)
{
    if (is.logical(x)) x <- factor(x, levels=c(FALSE, TRUE))
    if (!is.factor(x))
	stop("contrasts apply only to factors")
    if(!contrasts)
        return(structure(diag(nlevels(x)), dimnames=list(levels(x), levels(x))))
    ctr <- attr(x, "contrasts")
    if ((NL <- is.null(ctr)) || is.character(ctr)) {
	if(NL) ctr <- getOption("contrasts")[[if (is.ordered(x)) 2L else 1L]]
	ctrfn <- get(ctr, mode="function", envir=parent.frame())
	useSparse <- isTRUE(sparse) && "sparse" %in% names(as.list(args(ctrfn)))
        ctr <- if(useSparse)
            ctrfn(levels(x), contrasts = contrasts, sparse = sparse)
        else ctrfn(levels(x), contrasts = contrasts)
    }
    ctr
}

"contrasts<-" <-
    function(x, how.many, value)
{
    if (is.logical(x)) x <- factor(x, levels=c(FALSE, TRUE))
    if(!is.factor(x))
	stop("contrasts apply only to factors")
    if(nlevels(x) < 2L)
        stop("contrasts can be applied only to factors with 2 or more levels")
    if(is.function(value)) value <- value(nlevels(x))
    if(is.numeric(value)) {
	value <- as.matrix(value)
	nlevs <- nlevels(x)
	if(nrow(value) != nlevs)
	    stop("wrong number of contrast matrix rows")
	n1 <- if(missing(how.many)) nlevs - 1L else how.many
	nc <- ncol(value)
	rownames(value) <- levels(x)
	if(nc  < n1) {
	    cm <- qr(cbind(1,value))
	    if(cm$rank != nc+1) stop("singular contrast matrix")
	    cm <- qr.qy(cm, diag(nlevs))[, 2L:nlevs]
	    cm[,1L:nc] <- value
	    dimnames(cm) <- list(levels(x),NULL)
	    if(!is.null(nmcol <- dimnames(value)[[2L]]))
		dimnames(cm)[[2L]] <- c(nmcol, rep.int("", n1-nc))
	} else cm <- value[, 1L:n1, drop=FALSE]
    }
    else if(is.character(value)) cm <- value
    else if(is.null(value)) cm <- NULL
    else stop("numeric contrasts or contrast name expected")
    attr(x, "contrasts") <- cm
    x
}

## contr.poly() is in contr.poly.R

.sparse.array <- function(x, dim, dimnames) {
    ## loadNamespace(.) is very quick, once it *is* loaded:
    if(is.null(tryCatch(loadNamespace("Matrix"), error= function(e)NULL)))
        stop("contr*(.., sparse=TRUE) needs package \"Matrix\" correctly installed")
    dim <- as.integer(dim)
    new("dgCMatrix", Dim = dim, p = rep.int(0L, dim[2L]+1L), Dimnames = dimnames)
}

contr.helmert <-
    function (n, contrasts = TRUE, sparse = FALSE)
{
    if (length(n) <= 1L) {
	if(is.numeric(n) && length(n) == 1L && n > 1L) levels <- seq_len(n)
	else stop("not enough degrees of freedom to define contrasts")
    } else levels <- n
    lenglev <- length(levels <- as.character(levels))
    if(sparse) array <- .sparse.array
    if (contrasts) {
	cont <- array(-1, c(lenglev, lenglev-1L), list(levels, NULL))
	cont[col(cont) <= row(cont) - 2L] <- 0
	cont[col(cont) == row(cont) - 1L] <- 1L:(lenglev-1L)
    } else {
	cont <- array(0, c(lenglev, lenglev), list(levels, levels))
	cont[col(cont) == row(cont)] <- 1
    }
    cont
}

contr.treatment <-
    function(n, base = 1, contrasts = TRUE, sparse = FALSE)
{
    if(is.numeric(n) && length(n) == 1L) {
	if(n > 1L) levels <- as.character(seq_len(n))
	else stop("not enough degrees of freedom to define contrasts")
    } else {
	levels <- as.character(n)
	n <- length(n)
    }

    if(sparse) array <- .sparse.array
    contr <- array(0, c(n, n), list(levels, levels))
    diag(contr) <- 1
    if(contrasts) {
	if(n < 2L)
	    stop(gettextf("contrasts not defined for %d degrees of freedom",
                          n - 1L), domain = NA)
	if (base < 1L | base > n)
	    stop("baseline group number out of range")
	contr <- contr[, -base, drop = FALSE]
    }
    contr
}

contr.sum <-
    function (n, contrasts = TRUE, sparse = FALSE)
{
    if (length(n) <= 1L) {
	if (is.numeric(n) && length(n) == 1L && n > 1L)
	    levels <- seq_len(n)
	else stop("not enough degrees of freedom to define contrasts")
    } else levels <- n
    lenglev <- length(levels <- as.character(levels))
    if(sparse) array <- .sparse.array
    if (contrasts) {
	cont <- array(0, c(lenglev, lenglev - 1L), list(levels, NULL))
	cont[col(cont) == row(cont)] <- 1
	cont[lenglev, ] <- -1
    } else {
	cont <- array(0, c(lenglev, lenglev), list(levels, levels))
	cont[col(cont) == row(cont)] <- 1
    }
    cont
}

contr.SAS <- function(n, contrasts = TRUE, sparse = FALSE)
{
    contr.treatment(n,
                    base = if (is.numeric(n) && length(n) == 1) n else length(n),
                    contrasts, sparse=sparse)
}
