princomp <- function(x, ...) UseMethod("princomp")

## use formula to allow update() to be used.
princomp.formula <- function(formula, data = NULL, subset, na.action, ...)
{
    mt <- terms(formula, data = data)
    if(attr(mt, "response") > 0) stop("response not allowed in formula")
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf$... <- NULL
    mf[[1]] <- as.name("model.frame")
    mf <- eval.parent(mf)
    ## this is not a `standard' model-fitting function,
    ## so no need to consider contrasts or levels
    if(any(sapply(mf, function(x) is.factor(x) || !is.numeric(x))))
        stop("PCA applies only to numerical variables")
    na.act <- attr(mf, "na.action")
    mt <- attr(mf, "terms") # allow model.frame to update it
    attr(mt, "intercept") <- 0
    x <- model.matrix(mt, mf)
    res <- princomp.default(x, ...)
    ## fix up call to refer to the generic, but leave arg name as `formula'
    cl[[1]] <- as.name("princomp")
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
             subset = rep(TRUE, nrow(as.matrix(x))), ...)
{
    cl <- match.call()
    cl[[1]] <- as.name("princomp")
    if(!missing(x) && !missing(covmat))
        warning("both 'x' and 'covmat' were supplied: 'x' will be ignored")
    z <- if(!missing(x)) as.matrix(x)[subset, , drop = FALSE]
    if (is.list(covmat)) {
        if(any(is.na(match(c("cov", "n.obs"), names(covmat)))))
            stop("'covmat' is not a valid covariance list")
        cv <- covmat$cov
        n.obs <- covmat$n.obs
        cen <- covmat$center
    } else if(is.matrix(covmat)) {
        cv <- covmat
        n.obs <- NA
        cen <- NULL
    } else if(is.null(covmat)){
        dn <- dim(z)
        if(dn[1] < dn[2])
            stop("'princomp' can only be used with more units than variables")
        covmat <- cov.wt(z)             # returns list, cov() does not
        n.obs <- covmat$n.obs
        cv <- covmat$cov * (1 - 1/n.obs)# for S-PLUS compatibility
        cen <- covmat$center
    } else stop("'covmat' is of unknown type")
    if(!is.numeric(cv)) stop("PCA applies only to numerical variables")
    if (cor) {
        sds <- sqrt(diag(cv))
        cv <- cv/(sds %o% sds)
    }
    edc <- eigen(cv, symmetric = TRUE)
    ev <- edc$values
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
    scr <- if (scores && !missing(x) && !is.null(cen))
        scale(z, center = cen, scale = sc) %*% edc$vectors
    if (is.null(cen)) cen <- rep(as.numeric(NA), nrow(cv))
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
    cat("Call:\n"); dput(x$call)
    cat("\nStandard deviations:\n")
    print(x$sdev, ...)
    cat("\n", length(x$scale), " variables and ", x$n.obs,
        "observations.\n")
    invisible(x)
}
