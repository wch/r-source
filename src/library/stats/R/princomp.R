#  File src/library/stats/R/princomp.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

princomp <- function(x, ...) UseMethod("princomp")

## use formula to allow update() to be used.
princomp.formula <- function(formula, data = NULL, subset, na.action, ...)
{
    mt <- terms(formula, data = data)
    if(attr(mt, "response") > 0) stop("response not allowed in formula")
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
    mt <- attr(mf, "terms") # allow model.frame to update it
    attr(mt, "intercept") <- 0
    x <- model.matrix(mt, mf)
    res <- princomp.default(x, ...)
    ## fix up call to refer to the generic, but leave arg name as `formula'
    cl[[1L]] <- as.name("princomp")
    res$call <- cl
    if(!is.null(na.act)) {
        res$na.action <- na.act # not currently used
        if(!is.null(sc <- res$scores))
            res$scores <- napredict(na.act, sc)
    }
    res
}

princomp.default <-
    function(x, cor = FALSE, scores = TRUE, covmat = NULL,
             subset = rep_len(TRUE, nrow(as.matrix(x))), ...)
{
    chkDots(...)
    cl <- match.call()
    cl[[1L]] <- as.name("princomp")
    z <- if(!missing(x)) as.matrix(x)[subset, , drop = FALSE]
    if (is.list(covmat)) {
        if(any(is.na(match(c("cov", "n.obs"), names(covmat)))))
            stop("'covmat' is not a valid covariance list")
        cv <- covmat$cov
        n.obs <- covmat$n.obs
        cen <- covmat$center
    } else if(is.matrix(covmat)) {
	if(!missing(x)) ## warn only here; x is used for scores when we have 'cen'
	    warning("both 'x' and 'covmat' were supplied: 'x' will be ignored")
        cv <- covmat
        n.obs <- NA
        cen <- NULL
    } else if(is.null(covmat)){
        dn <- dim(z)
        if(dn[1L] < dn[2L])
            stop("'princomp' can only be used with more units than variables")
        covmat <- cov.wt(z)             # returns list, cov() does not
        n.obs <- covmat$n.obs
        cv <- covmat$cov * (1 - 1/n.obs)# for S-PLUS compatibility
        cen <- covmat$center
    } else stop("'covmat' is of unknown type")
    if(!is.numeric(cv)) stop("PCA applies only to numerical variables")
    if (cor) {
        sds <- sqrt(diag(cv))
        if(any(sds == 0))
            stop("cannot use 'cor = TRUE' with a constant variable")
        cv <- cv/(sds %o% sds)
    }
    edc <- eigen(cv, symmetric = TRUE)
    ev <- edc$values
    if (any(neg <- ev < 0)) { # S-PLUS sets all := 0
        ## 9 * : on Solaris found case where 5.59 was needed (MM)
        if (any(ev[neg] < - 9 * .Machine$double.eps * ev[1L]))
            stop("covariance matrix is not non-negative definite")
        else
            ev[neg] <- 0
    }
    cn <- paste0("Comp.", 1L:ncol(cv))
    names(ev) <- cn
    dimnames(edc$vectors) <- if(missing(x))
        list(dimnames(cv)[[2L]], cn) else list(dimnames(x)[[2L]], cn)
    sdev <- sqrt(ev)
    sc <- setNames(if (cor) sds else rep.int(1, ncol(cv)),
		   colnames(cv))
    scr <- if (scores && !missing(x) && !is.null(cen))
        scale(z, center = cen, scale = sc) %*% edc$vectors
    if (is.null(cen)) cen <- rep(NA_real_, nrow(cv))
    edc <- list(sdev = sdev,
                loadings = structure(edc$vectors, class="loadings"),
                center = cen, scale = sc, n.obs = n.obs,
                scores = scr, call = cl)
    ## The Splus function also return list elements factor.sdev,
    ## correlations and coef, but these are not documented in the help.
    ## coef seems to equal load.  The Splus function also returns list
    ## element terms which is not supported here.
    class(edc) <- "princomp"
    edc
}

print.princomp <- function(x, ...)
{
    cat("Call:\n"); dput(x$call, control=NULL)
    cat("\nStandard deviations:\n")
    print(x$sdev, ...)
    cat("\n", length(x$scale), " variables and ", x$n.obs,
        "observations.\n")
    invisible(x)
}
