princomp <- function(x, cor = FALSE, scores = TRUE,
                     subset = rep(TRUE, nrow(as.matrix(x)))) {
    ## it is tempting to add use="all.obs" which could be passed to cov
    ## or cor but then the calculation of N is complicated.
    z<- as.matrix(x)[subset, , drop = FALSE]
    N <- nrow(z)
    if (cor)
        cv <- get("cor", envir = .GlobalEnv)(z)
    else
        cv <- cov(z)
    ## (svd can be used but gives different signs for some vectors)
    edc <- eigen(cv)
    cn <- paste("Comp.", 1:ncol(cv), sep = "")
    names(edc$values) <- cn
    dimnames(edc$vectors) <- list(dimnames(x)[[2]], cn)
    scr <- NULL
    if (cor) {
        sdev <- sqrt(edc$values)
        sc <- (apply(z, 2, var)*(N-1)/N)^0.5
        if (scores)
            scr<- (scale(z, center = TRUE, scale = TRUE)
                   %*% edc$vectors)*sqrt(N/(N-1))
    } else {
        sdev <- sqrt(edc$values*(N-1)/N)
        sc <- rep(1, ncol(z))
        if (scores)
            scr<- (scale(z, center = TRUE, scale = FALSE)
                   %*% edc$vectors)
   }
    names(sc) <- dimnames(x)[[2]]
    edc <-list(sdev = sdev, loadings = edc$vectors,
               center = apply(z, 2, mean), scale = sc, n.obs = N,
               scores = scr, call = match.call())
    ## The Splus function also return list elements factor.sdev,
    ## correlations and coef, but these are not documented in the help.
    ## coef seems to equal load.  The Splus function also returns list
    ## element terms which is not supported here.
    class(edc) <- "princomp"
    edc
}

print.princomp <- function(x) {
    cat("Call:\n"); dput(x$call)
    cat("\nStandard deviations:\n")
    print(x$sdev)
    cat("\n", length(x$scale), " variables and ", x$n.obs,
        "observations.\n")
    invisible(x)
}
