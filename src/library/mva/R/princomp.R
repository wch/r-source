princomp <- function(x, cor = FALSE, scores = TRUE, covmat = NULL,
                     subset = rep(TRUE, nrow(as.matrix(x))))
{
    z <- if(!missing(x)) as.matrix(x)[subset, , drop = FALSE]
    if (is.list(covmat)) {
        if(any(is.na(match(c("cov", "n.obs"), names(covmat)))))
            stop("covmat is not a valid covariance list")
        cv <- covmat$cov
        n.obs <- covmat$n.obs
        cen <- covmat$center
    } else if(is.matrix(covmat)) {
        cv <- covmat
        n.obs <- NA
        cen <- NULL
    } else if(is.null(covmat)){
        covmat <- cov.wt(z)             # returns list, cov() does not
        n.obs <- covmat$n.obs
        cv <- covmat$cov * (1 - 1/n.obs)# for S-PLUS compatibility
        cen <- covmat$center
    } else stop("covmat is of unknown type")
    if (cor) {
        sds <- sqrt(diag(cv))
        cv <- cv/(sds %o% sds)
    }
    edc <- La.eigen(cv, symmetric = TRUE)
    ev <- .Alias(edc$values)
    if (any(neg <- ev < 0)) { # S-PLUS sets all := 0
        ## 9 * : on Solaris found case where 5.59 was needed (MM)
        if (any(ev[neg] < - 9 * .Machine$double.eps * ev[1]))
            stop("covariance matrix is not non-negative definite")
        else
            ev[neg] <- 0
    }
    cn <- paste("Comp.", 1:ncol(cv), sep = "")
    names(ev) <- cn
    dimnames(edc$vectors) <- if(missing(x))
        list(dimnames(cv)[[2]], cn) else list(dimnames(x)[[2]], cn)
    sdev <- sqrt(ev)
    sc <- if (cor) sds else rep(1, ncol(cv))
    names(sc) <- colnames(cv)
    scr <- if (scores && !missing(x))
        scale(z, center = TRUE, scale = sc) %*% edc$vectors
    if (is.null(cen)) cen <- rep(NA, nrow(cv))
    edc <- list(sdev = sdev,
                loadings = structure(edc$vectors, class="loadings"),
                center = cen, scale = sc, n.obs = n.obs,
                scores = scr, call = match.call())
    ## The Splus function also return list elements factor.sdev,
    ## correlations and coef, but these are not documented in the help.
    ## coef seems to equal load.  The Splus function also returns list
    ## element terms which is not supported here.
    class(edc) <- "princomp"
    edc
}

print.princomp <- function(x, ...)
{
    cat("Call:\n"); dput(x$call)
    cat("\nStandard deviations:\n")
    print(x$sdev, ...)
    cat("\n", length(x$scale), " variables and ", x$n.obs,
        "observations.\n")
    invisible(x)
}
