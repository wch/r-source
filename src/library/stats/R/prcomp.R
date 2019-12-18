#  File src/library/stats/R/prcomp.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

prcomp <- function (x, ...) UseMethod("prcomp")

prcomp.default <-
    function(x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL,
             rank. = NULL, ...)
{
    chkDots(...)
    x <- as.matrix(x)
    x <- scale(x, center = center, scale = scale.)
    cen <- attr(x, "scaled:center")
    sc <- attr(x, "scaled:scale")
    if(any(sc == 0))
        stop("cannot rescale a constant/zero column to unit variance")
    n <- nrow(x)
    p <- ncol(x)
    k <- if(!is.null(rank.)) {
	     stopifnot(length(rank.) == 1, is.finite(rank.), as.integer(rank.) > 0)
	     min(as.integer(rank.), n, p)
	     ## Note that La.svd() *still* needs a (n x p) and a (p x p) auxiliary
	 } else
	     min(n, p)
    s <- svd(x, nu = 0, nv = k)
    j <- seq_len(k)
    s$d <- s$d / sqrt(max(1, n - 1))
    if (!is.null(tol)) {
        ## we get rank at least one even for a 0 matrix.
        rank <- sum(s$d > (s$d[1L]*tol))
        if (rank < k) {
            j <- seq_len(k <- rank)
            s$v <- s$v[,j , drop = FALSE]
        }
    }
    dimnames(s$v) <- list(colnames(x), paste0("PC", j))
    r <- list(sdev = s$d, rotation = s$v,
              center = if(is.null(cen)) FALSE else cen,
              scale = if(is.null(sc)) FALSE else sc)
    if (retx) r$x <- x %*% s$v
    class(r) <- "prcomp"
    r
}

prcomp.formula <- function (formula, data = NULL, subset, na.action, ...)
{
    mt <- terms(formula, data = data)
    if (attr(mt, "response") > 0L)
        stop("response not allowed in formula")
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf$... <- NULL
    ## need stats:: for non-standard evaluation
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval.parent(mf)
    ## this is not a `standard' model-fitting function,
    ## so no need to consider contrasts or levels
    if (.check_vars_numeric(mf))
        stop("PCA applies only to numerical variables")
    na.act <- attr(mf, "na.action")
    mt <- attr(mf, "terms")
    attr(mt, "intercept") <- 0L
    x <- model.matrix(mt, mf)
    res <- prcomp.default(x, ...)
    ## fix up call to refer to the generic, but leave arg name as `formula'
    cl[[1L]] <- as.name("prcomp")
    res$call <- cl
    if (!is.null(na.act)) {
        res$na.action <- na.act
        if (!is.null(sc <- res$x))
            res$x <- napredict(na.act, sc)
    }
    res
}

plot.prcomp <- function(x, main = deparse1(substitute(x)), ...)
    screeplot.default(x, main = main, ...)

print.prcomp <- function(x, print.x = FALSE, ...) {
    cat(sprintf("Standard deviations (1, .., p=%d):\n", length(x$sdev)))
    print(x$sdev, ...)
    d <- dim(x$rotation)
    cat(sprintf("\nRotation (n x k) = (%d x %d):\n", d[1], d[2]))
    print(x$rotation, ...)
    if (print.x && length(x$x)) {
        cat("\nRotated variables:\n")
        print(x$x, ...)
    }
    invisible(x)
}

summary.prcomp <- function(object, ...)
{
    chkDots(...)
    vars <- object$sdev^2
    vars <- vars/sum(vars)
    importance <- rbind("Standard deviation" = object$sdev,
                        "Proportion of Variance" = round(vars, 5),
                        "Cumulative Proportion" = round(cumsum(vars), 5))
    k <- ncol(object$rotation)
    colnames(importance) <- c(colnames(object$rotation), rep("", length(vars) - k))
    object$importance <- importance
    class(object) <- "summary.prcomp"
    object
}

print.summary.prcomp <-
function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    dr <- dim(x$rotation); k <- dr[2]
    p <- length(x$sdev)
    if(k < p) {
	cat(sprintf("Importance of first k=%d (out of %d) components:\n", k, p))
	print(x$importance[, 1:k, drop=FALSE], digits = digits, ...)
    } else {
	cat("Importance of components:\n")
	print(x$importance, digits = digits, ...)
    }
    invisible(x)
}

predict.prcomp <- function(object, newdata, ...)
{
    chkDots(...)
    if (missing(newdata)) {
        if(!is.null(object$x)) return(object$x)
        else stop("no scores are available: refit with 'retx=TRUE'")
    }
    if(length(dim(newdata)) != 2L)
        stop("'newdata' must be a matrix or data frame")
    nm <- rownames(object$rotation)
    if(!is.null(nm)) {
        if(!all(nm %in% colnames(newdata)))
            stop("'newdata' does not have named columns matching one or more of the original columns")
        newdata <- newdata[, nm, drop = FALSE]
    } else {
        if(NCOL(newdata) != NROW(object$rotation) )
            stop("'newdata' does not have the correct number of columns")
    }
    ## next line does as.matrix
    scale(newdata, object$center, object$scale) %*% object$rotation
}

.check_vars_numeric <- function(mf)
{
    ## we need to test just the columns which are actually used.
    mt <- attr(mf, "terms")
    mterms <- attr(mt, "factors")
    mterms <- rownames(mterms)[apply(mterms, 1L, function(x) any(x > 0L))]
    any(sapply(mterms, function(x) is.factor(mf[,x]) || !is.numeric(mf[,x])))
}
