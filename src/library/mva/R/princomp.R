princomp <- function(x, cor = FALSE, scores = TRUE, covmat = NULL,
                     subset = rep(TRUE, nrow(as.matrix(x)))) {
    if (!missing(x)) z <- as.matrix(x)[subset, , drop = FALSE]
    else z <- NULL
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
        covmat <- cov.wt(z)
        n.obs <- covmat$n.obs
        cv <- covmat$cov * (1 - 1/n.obs) # for S-PLUS compatibility
        cen <- covmat$center
    } else stop("covmat is of unknown type")
    if (cor) {
        sds <- sqrt(diag(cv))
        cv <- cv/(sds %o% sds)
    }
    edc <- eigen(cv)
    if (any(edc$values < 0))
        stop("covariance matrix is not non-negative definite")
    cn <- paste("Comp.", 1:ncol(cv), sep = "")
    names(edc$values) <- cn
    dimnames(edc$vectors) <- list(dimnames(x)[[2]], cn)
    sdev <- sqrt(edc$values)
    if (cor) sc <- sds
    else sc <- rep(1, ncol(z))
    names(sc) <- colnames(cv)
    scr <- NULL
    if (scores && !missing(x))
        scr <- scale(z, center = TRUE, scale = sc) %*% edc$vectors
    if (is.null(cen)) cen <- rep(NA, nrow(cv))
    edc <-list(sdev = sdev, loadings = edc$vectors,
               center = cen, scale = sc, n.obs = n.obs,
               scores = scr, call = match.call())
    ## The Splus function also return list elements factor.sdev,
    ## correlations and coef, but these are not documented in the help.
    ## coef seems to equal load.  The Splus function also returns list
    ## element terms which is not supported here.
    class(edc) <- "princomp"
    edc
}

print.princomp <- function(x)
{
    cat("Call:\n"); dput(x$call)
    cat("\nStandard deviations:\n")
    print(x$sdev)
    cat("\n", length(x$scale), " variables and ", x$n.obs,
        "observations.\n")
    invisible(x)
}
