#  File src/library/stats/R/contrast.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
#  https://www.R-project.org/Licenses/


## This is also called from C : do_model_matrix() { ../../../main/model.c }:
contrasts <- function (x, contrasts = TRUE, sparse = FALSE)
{
    if (is.logical(x)) x <- factor(x, levels=c(FALSE, TRUE))
    if (!is.factor(x))
	stop("contrasts apply only to factors")
    if(!contrasts)
        return(.Diag(levels(x), sparse=sparse))

    ctr <- attr(x, "contrasts")
    if ((NL <- is.null(ctr)) || is.character(ctr)) {
	if(NL) ctr <- getOption("contrasts")[[if (is.ordered(x)) 2L else 1L]]
	ctrfn <- get(ctr, mode="function", envir=parent.frame())
	if(useSparse <- isTRUE(sparse)) {
	    if(!(useSparse <- any("sparse" == names(formals(ctrfn)))))
		warning(sprintf(
		"contrast function '%s' does not support 'sparse = TRUE'",
				ctr), domain = NA)
	}
        ctr <- if(useSparse)
            ctrfn(levels(x), contrasts = contrasts, sparse = sparse)
        else ctrfn(levels(x), contrasts = contrasts)
    }
    ctr
}

`contrasts<-` <- function(x, how.many, value)
{
    if (is.logical(x)) x <- factor(x, levels=c(FALSE, TRUE))
    if(!is.factor(x))
	stop("contrasts apply only to factors")
    if(nlevels(x) < 2L)
        stop("contrasts can be applied only to factors with 2 or more levels")
    if(is.function(value)) value <- value(nlevels(x))
    if((is.n <- is.numeric(value)) ||
        (isS4(value) && methods::is(value, "Matrix"))) {
	## also work for "sparseMatrix"
	if(is.n) value <- as.matrix(value)
	nlevs <- nlevels(x)
	if(nrow(value) != nlevs)
	    stop("wrong number of contrast matrix rows")
	n1 <- if(missing(how.many)) nlevs - 1L else how.many
	nc <- ncol(value)
	rownames(value) <- levels(x)
	if(nc < n1) {
	    if(!is.n) value <- as.matrix(value) ## for now use traditional qr():
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


## a fast version of diag(n, .) / sparse-Diagonal() + dimnames
.Diag <- function(nms, sparse) {
    ## no error checking here
    n <- as.integer(length(nms))
    d <- c(n,n)
    dn <- list(nms, nms)
    if(sparse) {
        if(!suppressPackageStartupMessages(requireNamespace("Matrix")))
	    stop(gettextf("%s needs package 'Matrix' correctly installed",
                          "contr*(.., sparse=TRUE)"),
                 domain = NA)
	methods::new("ddiMatrix", diag = "U", Dim = d, Dimnames = dn)
    } else
	array(c(rep.int(c(1, numeric(n)), n-1L), 1), d, dn)
}

.asSparse <- function(m) {
    ## ensure helpful error message when Matrix is missing:
    if(!suppressPackageStartupMessages(requireNamespace("Matrix")))
	stop(gettextf("%s needs package 'Matrix' correctly installed",
                      "contr*(.., sparse=TRUE)"),
             domain = NA)
    methods::as(m, "sparseMatrix")
}

## contr.poly() is in ./contr.poly.R

contr.helmert <-
    function (n, contrasts = TRUE, sparse = FALSE)
{
    if (length(n) <= 1L) {
	if(is.numeric(n) && length(n) == 1L && n > 1L) levels <- seq_len(n)
	else stop("not enough degrees of freedom to define contrasts")
    } else levels <- n
    levels <- as.character(levels)

    if (contrasts) {
        n <- length(levels)
	cont <- array(-1, c(n, n-1L), list(levels, NULL))
	cont[col(cont) <= row(cont) - 2L] <- 0
	cont[col(cont) == row(cont) - 1L] <- seq_len(n-1L)
        colnames(cont) <- NULL
        if(sparse) .asSparse(cont) else cont
    }
    else
        .Diag(levels, sparse=sparse)
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

    contr <- .Diag(levels, sparse=sparse)
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

contr.sum <- function (n, contrasts = TRUE, sparse = FALSE)
{
    if (length(n) <= 1L) {
	if (is.numeric(n) && length(n) == 1L && n > 1L)
	    levels <- seq_len(n)
	else stop("not enough degrees of freedom to define contrasts")
    } else levels <- n

    levels <- as.character(levels)
    cont <- .Diag(levels, sparse=sparse)
    if (contrasts) {
        cont <- cont[, -length(levels), drop = FALSE]
        cont[length(levels), ] <- -1
        colnames(cont) <- NULL
    }
    cont
}

contr.SAS <- function(n, contrasts = TRUE, sparse = FALSE)
{
    contr.treatment(n,
                    base = if (is.numeric(n) && length(n) == 1L) n else length(n),
                    contrasts=contrasts, sparse=sparse)
}
