#  File src/library/splines/R/splines.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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

bs <- function(x, df = NULL, knots = NULL, degree = 3, intercept = FALSE,
               Boundary.knots = range(x), warn.outside = TRUE)
{
    ord <- 1L + (degree <- as.integer(degree))
    if(ord <= 1) stop("'degree' must be integer >= 1")
    nx <- names(x)
    x <- as.vector(x)
    nax <- is.na(x)
    if(nas <- any(nax))
        x <- x[!nax]
    outside <- if(!missing(Boundary.knots)) {
        Boundary.knots <- sort(Boundary.knots)
        (ol <- x < Boundary.knots[1L]) | (or <- x > Boundary.knots[2L])
    } else FALSE

    if(mk.knots <- !is.null(df) && is.null(knots)) {
	nIknots <- df - ord + (1L - intercept) # ==  #{inner knots}
        if(nIknots < 0L) {
            nIknots <- 0L
            warning(gettextf("'df' was too small; have used %d",
                             ord - (1L - intercept)), domain = NA)
        }
        knots <-
            if(nIknots > 0L) {
                knots <- seq.int(from = 0, to = 1,
                                 length.out = nIknots + 2L)[-c(1L, nIknots + 2L)]
                quantile(x[!outside], knots, names=FALSE)
            }
    }
    else if(!all(is.finite(knots))) stop("non-finite knots")
    if(mk.knots && length(knots) && any(lrEq <- range(knots) %in% Boundary.knots)) {
        if(lrEq[1L]) {
            aE <- all(i <- knots == (piv <- Boundary.knots[1L]))
            if(aE)
                warning("all interior knots match left boundary knot")
            else
                knots[i] <- knots[i] + (min(knots[knots > piv]) - piv)/8
        }
        if(lrEq[2L]) {
            aE2 <- all(i <- knots == (piv <- Boundary.knots[2L]))
            if(aE2)
                warning("all interior knots match right boundary knot")
            else
                knots[i] <- knots[i] - (piv - max(knots[knots < piv]))/8
        }
        if(!(lrEq[1L] && aE || lrEq[2L] && aE2)) # haven't warned yet
            warning("shoving 'interior' knots matching boundary knots to inside")
    }
    Aknots <- sort(c(rep(Boundary.knots, ord), knots))
    if(any(outside)) {
        if(warn.outside) warning("some 'x' values beyond boundary knots may cause ill-conditioned bases")
        derivs <- 0:degree
        scalef <- gamma(1L:ord)# factorials
        basis <- array(0, c(length(x), length(Aknots) - degree - 1L))
	e <- 1/4 # in theory anything in (0,1); was (implicitly) 0 in R <= 3.2.2
        if(any(ol)) {
	    ## left pivot inside, i.e., a bit to the right of the boundary knot
	    k.pivot <- (1-e)*Boundary.knots[1L] + e*Aknots[ord+1]
            xl <- cbind(1, outer(x[ol] - k.pivot, 1L:degree, `^`))
            tt <- splineDesign(Aknots, rep(k.pivot, ord), ord, derivs)
            basis[ol, ] <- xl %*% (tt/scalef)
        }
        if(any(or)) {
	    ## right pivot inside, i.e., a bit to the left of the boundary knot:
	    k.pivot <- (1-e)*Boundary.knots[2L] + e*Aknots[length(Aknots)-ord]
            xr <- cbind(1, outer(x[or] - k.pivot, 1L:degree, `^`))
            tt <- splineDesign(Aknots, rep(k.pivot, ord), ord, derivs)
            basis[or, ] <- xr %*% (tt/scalef)
        }
        if(any(inside <- !outside))
            basis[inside,  ] <- splineDesign(Aknots, x[inside], ord)
    }
    else basis <- splineDesign(Aknots, x, ord)
    if(!intercept)
        basis <- basis[, -1L , drop = FALSE]
    n.col <- ncol(basis)
    if(nas) {
        nmat <- matrix(NA, length(nax), n.col)
        nmat[!nax,  ] <- basis
        basis <- nmat
    }
    dimnames(basis) <- list(nx, 1L:n.col)
    a <- list(degree = degree, knots = if(is.null(knots)) numeric(0L) else knots,
              Boundary.knots = Boundary.knots, intercept = intercept)
    attributes(basis) <- c(attributes(basis), a)
    class(basis) <- c("bs", "basis", "matrix")
    basis
}

ns <- function(x, df = NULL, knots = NULL, intercept = FALSE,
               Boundary.knots = range(x))
{
    nx <- names(x)
    x <- as.vector(x)
    nax <- is.na(x)
    if(nas <- any(nax))
        x <- x[!nax]
    outside <- if(!missing(Boundary.knots)) {
        Boundary.knots <- sort(Boundary.knots)
        (ol <- x < Boundary.knots[1L]) | (or <- x > Boundary.knots[2L])
    }
    else {
	if(length(x) == 1L) ## && missing(Boundary.knots) : special treatment
	    Boundary.knots <- x*c(7,9)/8 # symmetrically around x
	FALSE # rep(FALSE, length = length(x))
    }
    if(mk.knots <- !is.null(df) && is.null(knots)) {
        ## df = number(interior knots) + 1 + intercept
        nIknots <- df - 1L - intercept
        if(nIknots < 0L) {
            nIknots <- 0L
            warning(gettextf("'df' was too small; have used %d",
                             1L + intercept), domain = NA)
        }
        knots <-
            if(nIknots > 0L) {
                knots <- seq.int(from = 0, to = 1,
                                 length.out = nIknots + 2L)[-c(1L, nIknots + 2L)]
                quantile(x[!outside], knots, names=FALSE)
            }
    } else {
        if(!all(is.finite(knots))) stop("non-finite knots")
        nIknots <- length(knots)
    }
    if(mk.knots && length(knots) && any(lrEq <- range(knots) %in% Boundary.knots)) {
        if(lrEq[1L]) {
            i <- knots == (piv <- Boundary.knots[1L])
            if(all(i)) stop("all interior knots match left boundary knot")
            knots[i] <- knots[i] + (min(knots[knots > piv]) - piv)/8
        }
        if(lrEq[2L]) {
            i <- knots == (piv <- Boundary.knots[2L])
            if(all(i)) stop("all interior knots match right boundary knot")
            knots[i] <- knots[i] - (piv - max(knots[knots < piv]))/8
        }
        warning("shoving 'interior' knots matching boundary knots to inside")
    }
    Aknots <- sort(c(rep(Boundary.knots, 4L), knots))
    if(any(outside)) {
        basis <- array(0, c(length(x), nIknots + 4L))
        if(any(ol)) {
            k.pivot <- Boundary.knots[1L]
            xl <- cbind(1, x[ol] - k.pivot)
            tt <- splineDesign(Aknots, rep(k.pivot, 2L), 4, c(0, 1))
            basis[ol,  ] <- xl %*% tt
        }
        if(any(or)) {
            k.pivot <- Boundary.knots[2L]
            xr <- cbind(1, x[or] - k.pivot)
            tt <- splineDesign(Aknots, rep(k.pivot, 2L), 4, c(0, 1))
            basis[or,  ] <- xr %*% tt
        }
        if(any(inside <- !outside))
            basis[inside,  ] <- splineDesign(Aknots, x[inside], 4)
    }
    else basis <- splineDesign(Aknots, x, ord = 4L)
    const <- splineDesign(Aknots, Boundary.knots, ord = 4L, derivs = c(2L, 2L))
    if(!intercept) {
        const <- const[, -1 , drop = FALSE]
        basis <- basis[, -1 , drop = FALSE]
    }
    qr.const <- qr(t(const))
    basis <- as.matrix((t(qr.qty(qr.const, t(basis))))[,  - (1L:2L), drop = FALSE])
    n.col <- ncol(basis)
    if(nas) {
        nmat <- matrix(NA, length(nax), n.col)
        nmat[!nax, ] <- basis
        basis <- nmat
    }
    dimnames(basis) <- list(nx, 1L:n.col)
    a <- list(degree = 3L, knots = if(is.null(knots)) numeric() else knots,
              Boundary.knots = Boundary.knots, intercept = intercept)
    attributes(basis) <- c(attributes(basis), a)
    class(basis) <- c("ns", "basis", "matrix")
    basis
}

predict.bs <- function(object, newx, ...)
{
    if(missing(newx))
        return(object)
    a <- c(list(x = newx), attributes(object)[
                c("degree", "knots", "Boundary.knots", "intercept")])
    do.call("bs", a)
}

predict.ns <- function(object, newx, ...)
{
    if(missing(newx))
        return(object)
    a <- c(list(x = newx), attributes(object)[
                c("knots", "Boundary.knots", "intercept")])
    do.call("ns", a)
}

### FIXME:  Also need  summary.basis() and probably print.basis()  method!

makepredictcall.ns <- function(var, call)
{
    ## check must work correctly when call is a symbol, both for quote(ns) and quote(t1):
    if(as.character(call)[1L] == "ns" || (is.call(call) && identical(eval(call[[1L]]), ns))) {
	at <- attributes(var)[c("knots", "Boundary.knots", "intercept")]
	call <- call[1L:2L]
	call[names(at)] <- at
    }
    call
}

makepredictcall.bs <- function(var, call)
{
    if(as.character(call)[1L] == "bs" || (is.call(call) && identical(eval(call[[1L]]), bs))) {
	at <- attributes(var)[c("degree", "knots", "Boundary.knots", "intercept")]
	call <- call[1L:2L]
	call[names(at)] <- at
    }
    call
}


## this is *not* used anymore by our own code [but has been exported "forever"]
spline.des <- function(knots, x, ord = 4, derivs = integer(length(x)),
		       outer.ok = FALSE, sparse = FALSE)
{
    if(is.unsorted(knots <- as.numeric(knots)))
	knots <- sort.int(knots)
    list(knots = knots, order = ord, derivs = derivs,
	 design = splineDesign(knots, x, ord, derivs,
			       outer.ok = outer.ok, sparse = sparse))
}
## splineDesign() is in ./splineClasses.R
