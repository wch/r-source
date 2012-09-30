#  File src/library/stats/R/proj.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1998 B. D. Ripley
#  Copyright (C) 1998-2012 The R Core Team
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

proj <- function(object, ...) UseMethod("proj")

proj.default <- function(object, onedf = TRUE, ...)
{
    if(!is.qr(object$qr))
	stop("argument does not include a 'qr' component")
    if(is.null(object$effects))
	stop("argument does not include an 'effects' component")
    RB <- c(object$effects[seq(object$rank)],
	    rep.int(0, nrow(object$qr$qr) - object$rank))
    prj <- as.matrix(qr.Q(object$qr, Dvec = RB))
    DN <- dimnames(object$qr$qr)
    dimnames(prj) <- list(DN[[1L]], DN[[2L]][seq(ncol(prj))])
    prj
}

proj.lm <- function(object, onedf = FALSE, unweighted.scale = FALSE, ...)
{
    if(inherits(object, "mlm"))
	stop("'proj' is not implemented for multiple responses")
    rank <- object$rank
    if(rank > 0) {
	prj <- proj.default(object, onedf = TRUE)[, 1L:rank, drop = FALSE]
	if(onedf) {
	    df <- rep.int(1, rank)
	    result <- prj
	} else {
	    asgn <- object$assign[object$qr$pivot[1L:object$rank]]
	    uasgn <- unique(asgn)
	    nmeffect <- c("(Intercept)",
			  attr(object$terms, "term.labels"))[1 + uasgn]
	    nterms <- length(uasgn)
	    df <- vector("numeric", nterms)
	    result <- matrix(0, length(object$residuals), nterms)
	    dimnames(result) <- list(rownames(object$fitted.values), nmeffect)
	    for(i in seq_along(uasgn)) {
		select <- (asgn == uasgn[i])
		df[i] <- sum(select)
		result[, i] <- prj[, select, drop = FALSE] %*% rep.int(1, df[i])
	    }
	}
    } else {
	result <- NULL
	df <- NULL
    }
    if(!is.null(wt <- object$weights) && unweighted.scale)
	result <- result/sqrt(wt)
    use.wt <- !is.null(wt) && !unweighted.scale
    if(object$df.residual > 0) {
        res <- if(use.wt) object$residuals * sqrt(wt) else object$residuals
	if(!is.matrix(result)) {
	    result <- matrix(res, length(res), 1L,
			     dimnames = list(names(res), "Residuals"))
	} else {
	    dn <- dimnames(result)
	    d <- dim(result)
	    result <- setNames(c(result, res), NULL)
	    dim(result) <- d + c(0, 1)
	    dimnames(result) <- list(names(res), c(dn[[2L]], "Residuals"))
        }
	df <- c(df, object$df.residual)
    }
    names(df) <- colnames(result)
    attr(result, "df") <- df
    attr(result, "formula") <- object$call$formula
    attr(result, "onedf") <- onedf
    if(!is.null(wt)) attr(result, "unweighted.scale") <- unweighted.scale
    result
}

proj.aov <- function(object, onedf = FALSE, unweighted.scale = FALSE, ...)
{
    if(inherits(object, "maov"))
	stop("'proj' is not implemented for multiple responses")
    factors.aov <- function(pnames, tfactor)
    {
	if(!is.na(int <- match("(Intercept)", pnames)))
	    pnames <- pnames[ - int]
	tnames <- setNames(lapply(colnames(tfactor), function(x, mat)
				  rownames(mat)[mat[, x] > 0], tfactor),
			   colnames(tfactor))
	if(!is.na(match("Residuals", pnames))) {
	    enames <- c(rownames(tfactor)
			[as.logical(tfactor %*% rep.int(1, ncol(tfactor)))],
			"Within")
	    tnames <- append(tnames, list(Residuals = enames))
	}
	result <- tnames[match(pnames, names(tnames))]
	if(!is.na(int)) result <- c("(Intercept)" = "(Intercept)", result)
	## should reorder result, but probably OK
	result
    }
    projections <- NextMethod("proj")
    t.factor <- attr(terms(object), "factors")
    attr(projections, "factors") <-
	factors.aov(colnames(projections), t.factor)
    attr(projections, "call") <- object$call
    attr(projections, "t.factor") <- t.factor
    class(projections) <- "aovproj"
    projections
}


proj.aovlist <- function(object, onedf = FALSE, unweighted.scale = FALSE, ...)
{
    attr.xdim <- function(x)
    {
	## all attributes except names, dim and dimnames
	atrf <- attributes(x)
	atrf[is.na(match(names(atrf), c("names", "dim", "dimnames")))]
    }
    "attr.assign<-" <- function(x, value)
    {
	## assign to x all attributes in attr.x
	##    attributes(x)[names(value)] <- value not allowed in R
	for(nm in names(value)) attr(x, nm) <- value[nm]
	x
    }
    factors.aovlist <- function(pnames, tfactor,
				strata = FALSE, efactor = FALSE)
    {
	if(!is.na(int <- match("(Intercept)", pnames))) pnames <- pnames[-int]
	tnames <- apply(tfactor, 2L, function(x, nms)
			nms[as.logical(x)], rownames(tfactor))
	if(!missing(efactor)) {
	    enames <- NULL
	    if(!is.na(err <- match(strata, colnames(efactor))))
		enames <- (rownames(efactor))[as.logical(efactor[, err])]
	    else if(strata == "Within")
		enames <- c(rownames(efactor)
			    [as.logical(efactor %*% rep.int(1, ncol(efactor)))],
			    "Within")
	    if(!is.null(enames))
		tnames <- append(tnames, list(Residuals = enames))
	}
	result <- tnames[match(pnames, names(tnames))]
	if(!is.na(int))
	    result <- c("(Intercept)" = "(Intercept)", result)
	##should reorder result, but probably OK
	result
    }
    if(unweighted.scale && is.null(attr(object, "weights")))
	unweighted.scale <- FALSE
    err.qr <- attr(object, "error.qr")
    Terms <- terms(object, "Error")
    t.factor <- attr(Terms, "factors")
    i <- attr(Terms, "specials")$Error
    t <- attr(Terms, "variables")[[1 + i]]
    error <- Terms
    error[[3L]] <- t[[2L]]
    e.factor <- attr(terms(formula(error)), "factors")
    n <- nrow(err.qr$qr)
    n.object <- length(object)
    result <- setNames(vector("list", n.object), names(object))
    D1 <- seq_len(NROW(err.qr$qr))
    if(unweighted.scale) wt <- attr(object, "weights")
    for(i in names(object)) {
	prj <- proj.lm(object[[i]], onedf = onedf)
	if(unweighted.scale) prj <- prj/sqrt(wt)
	result.i <- matrix(0, n, ncol(prj), dimnames = list(D1, colnames(prj)))
	select <- rownames(object[[i]]$qr$qr)
	if(is.null(select)) select <- rownames(object[[i]]$residuals)
	result.i[select,  ] <- prj
	result[[i]] <- as.matrix(qr.qy(err.qr, result.i))
	attr.assign(result[[i]]) <- attr.xdim(prj)
	D2i <- colnames(prj)
	dimnames(result[[i]]) <- list(D1, D2i)
	attr(result[[i]], "factors") <-
	    factors.aovlist(D2i, t.factor, strata = i, efactor = e.factor)
    }
    attr(result, "call") <- attr(object, "call")
    attr(result, "e.factor") <- e.factor
    attr(result, "t.factor") <- t.factor
    class(result) <- c("aovprojlist", "listof")
    result
}

terms.aovlist <- function(x, ...)
{
    x <- attr(x, "terms")
    terms(x, ...)
}

## wish of PR#13505
as.data.frame.aovproj <- function(x, ...) as.data.frame(unclass(x), ...)
