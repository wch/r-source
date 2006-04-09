### $Id: splines.R,v 1.6 2002/05/08 17:32:12 ripley Exp $

bs <- function(x, df = NULL, knots = NULL, degree = 3, intercept = FALSE,
               Boundary.knots = range(x))
{
    nx <- names(x)
    x <- as.vector(x)
    nax <- is.na(x)
    if(nas <- any(nax))
        x <- x[!nax]
    if(!missing(Boundary.knots)) {
        Boundary.knots <- sort(Boundary.knots)
        outside <- (ol <- x < Boundary.knots[1]) | (or <- x > Boundary.knots[2])
    }
    else outside <- FALSE #rep(FALSE, length = length(x))

    ord <- 1 + (degree <- as.integer(degree))
    if(ord <= 1) stop("'degree' must be integer >= 1")
    if(!missing(df) && missing(knots)) {
        nIknots <- df - ord + (1 - intercept)
        if(nIknots < 0) {
            nIknots <- 0
            warning("'df' was too small; have used  ", ord - (1 - intercept))
        }
        knots <-
            if(nIknots > 0) {
                knots <- seq(from = 0, to = 1,
                             length = nIknots + 2)[-c(1, nIknots + 2)]
                quantile(x[!outside], knots)
            }
    }
    Aknots <- sort(c(rep(Boundary.knots, ord), knots))
    if(any(outside)) {
        warning("some 'x' values beyond boundary knots may cause ill-conditioned bases")
        derivs <- 0:degree
        scalef <- gamma(1:ord)# factorials
        basis <- array(0, c(length(x), length(Aknots) - degree - 1))
        if(any(ol)) {
            k.pivot <- Boundary.knots[1]
            xl <- cbind(1, outer(x[ol] - k.pivot, 1:degree, "^"))
            tt <- spline.des(Aknots, rep(k.pivot, ord), ord, derivs)$design
            basis[ol,  ] <- xl %*% (tt/scalef)
        }
        if(any(or)) {
            k.pivot <- Boundary.knots[2]
            xr <- cbind(1, outer(x[or] - k.pivot, 1:degree, "^"))
            tt <- spline.des(Aknots, rep(k.pivot, ord), ord, derivs)$design
            basis[or,  ] <- xr %*% (tt/scalef)
        }
        if(any(inside <- !outside))
            basis[inside,  ] <- spline.des(Aknots, x[inside], ord)$design
    }
    else basis <- spline.des(Aknots, x, ord)$design
    if(!intercept)
        basis <- basis[, -1 , drop = FALSE]
    n.col <- ncol(basis)
    if(nas) {
        nmat <- matrix(NA, length(nax), n.col)
        nmat[!nax,  ] <- basis
        basis <- nmat
    }
    dimnames(basis) <- list(nx, 1:n.col)
    a <- list(degree = degree, knots = if(is.null(knots)) numeric(0) else knots,
              Boundary.knots = Boundary.knots, intercept = intercept)
    attributes(basis) <- c(attributes(basis), a)
    class(basis) <- c("bs", "basis")
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
    if(!missing(Boundary.knots)) {
        Boundary.knots <- sort(Boundary.knots)
        outside <- (ol <- x < Boundary.knots[1]) | (or <- x > Boundary.knots[2])
    }
    else outside <- FALSE # rep(FALSE, length = length(x))
    if(!missing(df) && missing(knots)) {
        ## df = number(interior knots) + 1 + intercept
        nIknots <- df - 1 - intercept
        if(nIknots < 0) {
            nIknots <- 0
            warning("'df' was too small; have used ", 1 + intercept)
        }
        knots <- if(nIknots > 0) {
            knots <- seq(0, 1, length = nIknots + 2)[-c(1, nIknots + 2)]
            quantile(x[!outside], knots)
        } ## else  NULL
    } else nIknots <- length(knots)
    Aknots <- sort(c(rep(Boundary.knots, 4), knots))
    if(any(outside)) {
        basis <- array(0, c(length(x), nIknots + 4))
        if(any(ol)) {
            k.pivot <- Boundary.knots[1]
            xl <- cbind(1, x[ol] - k.pivot)
            tt <- spline.des(Aknots, rep(k.pivot, 2), 4, c(0, 1))$design
            basis[ol,  ] <- xl %*% tt
        }
        if(any(or)) {
            k.pivot <- Boundary.knots[2]
            xr <- cbind(1, x[or] - k.pivot)
            tt <- spline.des(Aknots, rep(k.pivot, 2), 4, c(0, 1))$design
            basis[or,  ] <- xr %*% tt
        }
        if(any(inside <- !outside))
            basis[inside,  ] <- spline.des(Aknots, x[inside], 4)$design
    }
    else basis <- spline.des(Aknots, x, 4)$design
    const <- spline.des(Aknots, Boundary.knots, 4, c(2, 2))$design
    if(!intercept) {
        const <- const[, -1 , drop = FALSE]
        basis <- basis[, -1 , drop = FALSE]
    }
    qr.const <- qr(t(const))
    basis <- as.matrix((t(qr.qty(qr.const, t(basis))))[,  - (1:2)])
    n.col <- ncol(basis)
    if(nas) {
        nmat <- matrix(NA, length(nax), n.col)
        nmat[!nax, ] <- basis
        basis <- nmat
    }
    dimnames(basis) <- list(nx, 1:n.col)
    a <- list(degree = 3, knots = if(is.null(knots)) numeric(0) else knots,
              Boundary.knots = Boundary.knots, intercept = intercept)
    attributes(basis) <- c(attributes(basis), a)
    class(basis) <- c("ns", "basis")
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
    if(as.character(call)[1] != "ns") return(call)
    at <- attributes(var)[c("knots", "Boundary.knots", "intercept")]
    xxx <- call[1:2]
    xxx[names(at)] <- at
    xxx
}

makepredictcall.bs <- function(var, call)
{
    if(as.character(call)[1] != "bs") return(call)
    at <- attributes(var)[c("degree", "knots", "Boundary.knots", "intercept")]
    xxx <- call[1:2]
    xxx[names(at)] <- at
    xxx
}


spline.des <-
    function(knots, x, ord = 4, derivs = integer(length(x)), outer.ok = FALSE)
{
    list(knots = sort(as.vector(knots)), order = ord, derivs = derivs,
         design = splineDesign(knots, x, ord, derivs, outer.ok = outer.ok))
}
## splineDesign() is in ./splineClasses.R
