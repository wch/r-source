#  File src/library/stats/R/factanal.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team
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

factanal <-
    function (x, factors, data = NULL, covmat = NULL, n.obs = NA,
              subset, na.action, start = NULL,
              scores = c("none", "regression", "Bartlett"),
              rotation = "varimax",
              control = NULL, ...)
{
    sortLoadings <- function(Lambda, rotm = NULL)
    {
        cn <- colnames(Lambda)
        Phi <- attr(Lambda, "covariance")
        io.ssq <- order(- if(is.null(Phi)) colSums(Lambda^2) else diag(Phi %*% crossprod(Lambda)))
        Lambda <- Lambda[, io.ssq, drop = FALSE]
        neg <- colSums(Lambda) < 0
        Lambda[, neg] <- -Lambda[, neg]
        colnames(Lambda) <- cn
        has.rot <- !is.null(rotm)
        has.Ph  <- !is.null(Phi)
        if(has.rot || has.Ph) # used for both
            unit <- c(1, -1)[neg+1L] # = ifelse(neg, -1, 1)
        ## FIXME:  <M>  %*%  diag(unit)  can be made faster (for non-small cases) below
        if(has.rot)
            rotm <- rotm[, io.ssq] %*% diag(unit)
        if (has.Ph) {
            attr(Lambda, "covariance") <-
                diag(unit) %*% Phi[io.ssq, io.ssq] %*% diag(unit)
        }
        list(Lambda = Lambda, rotm = rotm)
    }
    cl <- match.call()
    na.act <- NULL
    if (is.list(covmat)) {
        if (anyNA(match(c("cov", "n.obs"), names(covmat))))
            stop("'covmat' is not a valid covariance list")
        cv <- covmat$cov
        n.obs <- covmat$n.obs
        have.x <- FALSE
    }
    else if (is.matrix(covmat)) {
        cv <- covmat
        have.x <- FALSE
    }
    else if (is.null(covmat)) {
        if(missing(x)) stop("neither 'x' nor 'covmat' supplied")
        have.x <- TRUE
        if(inherits(x, "formula")) {
            ## this is not a `standard' model-fitting function,
            ## so no need to consider contrasts or levels
            mt <- terms(x, data = data)
            if(attr(mt, "response") > 0)
                stop("response not allowed in formula")
            attr(mt, "intercept") <- 0
            mf <- match.call(expand.dots = FALSE)
            names(mf)[names(mf) == "x"] <- "formula"
            mf$factors <- mf$covmat <- mf$scores <- mf$start <-
                mf$rotation <- mf$control <- mf$... <- NULL
            ## need stats:: for non-standard evaluation
            mf[[1L]] <- quote(stats::model.frame)
            mf <- eval.parent(mf)
            na.act <- attr(mf, "na.action")
            if (.check_vars_numeric(mf))
                stop("factor analysis applies only to numerical variables")
            z <- model.matrix(mt, mf)
        } else {
            z <- as.matrix(x)
            if(!is.numeric(z))
                stop("factor analysis applies only to numerical variables")
            if(!missing(subset)) z <- z[subset, , drop = FALSE]
        }
        covmat <- cov.wt(z)
        cv <- covmat$cov
        n.obs <- covmat$n.obs
    }
    else stop("'covmat' is of unknown type")
    scores <- match.arg(scores)
    if(scores != "none" && !have.x)
        stop("requested scores without an 'x' matrix")
    p <- ncol(cv)
    if(p < 3) stop("factor analysis requires at least three variables")
    dof <- 0.5 * ((p - factors)^2 - p - factors)
    if(dof < 0)
        stop(sprintf(ngettext(factors,
                              "%d factor is too many for %d variables",
                              "%d factors are too many for %d variables"),
                     factors, p), domain = NA)
    ## FIXME: cov2cor() shows how to do this faster (for large dim)
    sds <- sqrt(diag(cv))
    cv <- cv/(sds %o% sds)

    cn <- list(nstart = 1, trace = FALSE, lower = 0.005)
    cn[names(control)] <- control
    more <- if(...length())
                Filter(length, # drop NULL components in
                       list(...)[c("nstart", "trace", "lower", "opt", "rotate")])
    if(length(more)) cn[names(more)] <- more

    if(is.null(start)) {
        start <- (1 - 0.5*factors/p)/diag(solve(cv))
        if((ns <- cn$nstart) > 1)
            start <- cbind(start, matrix(runif(ns-1), p, ns-1, byrow=TRUE))
    }
    start <- as.matrix(start)
    if(nrow(start) != p)
        stop(sprintf(ngettext(p,
                       "'start' must have %d row",
                       "'start' must have %d rows"),
                     p), domain = NA)
    nc <- ncol(start)
    if(nc < 1) stop("no starting values supplied")
    best <- Inf
    for (i in 1L:nc) {
        ## factanal.fit.mle(cmat, factors, start=NULL, lower = 0.005, control = NULL, ...)
        nfit <- factanal.fit.mle(cv, factors, start[, i], max(cn$lower, 0), cn$opt)
        if(cn$trace)
            cat("start", i, "value:", format(nfit$criteria[1L]),
                "uniqs:", format(as.vector(round(nfit$uniquenesses, 4))), "\n")
        if(nfit$converged && nfit$criteria[1L] < best) {
            fit <- nfit
            best <- fit$criteria[1L]
        }
    }
    if(best == Inf)
        stop(ngettext(nc,
                      "unable to optimize from this starting value",
                      "unable to optimize from these starting values"),
             domain = NA)
                                        # the following line changed by C. Bernaards, 20 December 2022
    load <- sortLoadings(fit$loadings)$Lambda
    if(is.function(rotation) || rotation != "none") {
        rot <- do.call(rotation, c(list(load), cn$rotate))
        load <- if (is.list(rot)) {
                    fit$rotmat <-
                        if(inherits(rot, "GPArotation")) t(solve(rot$Th))
                        else rot$rotmat
                    rot$loadings
                } else rot
        if(is.list(rot) && !is.null(rot$Phi)) attr(load, "covariance") <- rot$Phi
    }
                                        # the following lines added by C. Bernaards, 20 December 2022
    loadrot <- sortLoadings(load, fit$rotmat)
    fit$loadings <- loadrot$Lambda
    if(!is.null(loadrot$rotm)) fit$rotmat <- loadrot$rotm
                                        # end additions C. Bernaards, 20 December 2022
    class(fit$loadings) <- "loadings"
    fit$na.action <- na.act # book keeping
    if(have.x && scores != "none") {
        Lambda <- fit$loadings
        zz <- scale(z, TRUE, TRUE)
        switch(scores,
               regression = {
                   sc <- zz %*% solve(cv, Lambda)
                   if(!is.null(Phi <- attr(Lambda, "covariance")))
                       sc <- sc %*% Phi
               },
               Bartlett = {
                   d <- 1/fit$uniquenesses
                   tmp <- t(Lambda * d)
                   sc <- t(solve(tmp %*% Lambda, tmp %*% t(zz)))
               })
        rownames(sc) <- rownames(z)
        colnames(sc) <- colnames(Lambda)
        if(!is.null(na.act)) sc <- napredict(na.act, sc)
        fit$scores <- sc
    }
    if(!is.na(n.obs) && dof > 0) {
        fit$STATISTIC <- (n.obs - 1 - (2 * p + 5)/6 -
                     (2 * factors)/3) * fit$criteria["objective"]
        fit$PVAL <- pchisq(fit$STATISTIC, dof, lower.tail = FALSE)
    }
    fit$n.obs <- n.obs
    fit$call <- cl
    fit
}

factanal.fit.mle <-
    function(cmat, factors, start=NULL, lower = 0.005, control = NULL, ...)
{
    FAout <- function(Psi, S, q)
    {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1L:q, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), q)
        diag(sqrt(Psi)) %*% load
    }
    FAfn <- function(Psi, S, q)
    {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE, only.values = TRUE)
        e <- E$values[-(1L:q)]
        e <- sum(log(e) - e) - q + nrow(S)
##        print(round(c(Psi, -e), 5))  # for tracing
        -e
    }
    FAgr <- function(Psi, S, q)
    {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1L:q, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), q)
        load <- diag(sqrt(Psi)) %*% load
        g <- load %*% t(load) + diag(Psi) - S
        diag(g)/Psi^2
    }
    p <- ncol(cmat)
    if(is.null(start))
        start <- (1 - 0.5*factors/p)/diag(solve(cmat))
    res <- optim(start, FAfn, FAgr, method = "L-BFGS-B",
                 lower = lower, upper = 1,
                 control = c(list(fnscale = 1,
                                  parscale = rep(0.01, length(start))), control),
                 q = factors, S = cmat)
    Lambda <- FAout(res$par, cmat, factors)
    dimnames(Lambda) <- list(dimnames(cmat)[[1L]],
                             paste0("Factor", 1L:factors))
    p <- ncol(cmat)
    dof <- 0.5 * ((p - factors)^2 - p - factors)
    un <- setNames(res$par, colnames(cmat))
    class(Lambda) <- "loadings"
    ans <- list(converged = res$convergence == 0,
                loadings = Lambda, uniquenesses = un,
                correlation = cmat,
                criteria = c(objective = res$value, counts = res$counts),
                factors = factors, dof = dof, method = "mle")
    class(ans) <- "factanal"
    ans
}

print.loadings <- function(x, digits = 3L, cutoff = 0.1, sort = FALSE, ...)
{
    Lambda <- unclass(x)
    p <- nrow(Lambda)
    factors <- ncol(Lambda)
    if (sort) {
        mx <- max.col(abs(Lambda))
        ind <- cbind(1L:p, mx)
        mx[abs(Lambda[ind]) < 0.5] <- factors + 1
        Lambda <- Lambda[order(mx, 1L:p), , drop=FALSE]
    }
    cat("\nLoadings:\n")
    fx <- setNames(format(round(Lambda, digits)), NULL) # char matrix
    nc <- nchar(fx[1L], type="c")
    fx[abs(Lambda) < cutoff] <- strrep(" ", nc)
    print(fx, quote = FALSE, ...)
    Phi <- attr(Lambda, "covariance")
    vx <- if(is.null(Phi)) colSums(Lambda^2) else diag(Phi %*% crossprod(Lambda))
    names(vx) <- colnames(Lambda)
    varex <- rbind("SS loadings" = vx)
    if(is.null(attr(x, "covariance"))) {
        varex <- rbind(varex, "Proportion Var" = vx/p)
        if(factors > 1)
            varex <- rbind(varex, "Cumulative Var" = cumsum(vx/p))
    }
    cat("\n")
    print(round(varex, digits))
    invisible(x)
}

print.factanal <- function(x, digits = 3, ...)
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    cat("Uniquenesses:\n")
    print(round(x$uniquenesses, digits), ...)
    print(x$loadings, digits = digits, ...) #-> print.loadings()
                                        # the following lines added by J. Fox, 26 June 2005
    if(!is.null(x$rotmat)) {
        tmat <- solve(x$rotmat)
        R <- tcrossprod(tmat) # t . t'
        factors <- x$factors
        rownames(R) <- colnames(R) <- paste0("Factor", 1:factors)

                                        # the following line changed by Ulrich Keller, 9 Sept 2008
        if (!isTRUE(all.equal(c(R), c(diag(factors))))) {
            cat("\nFactor Correlations:\n")
            print(R, digits=digits, ...)
        }
    }
                                        # end additions J. Fox, 23 June 2005
    if(!is.null(x$STATISTIC)) {
        factors <- x$factors
        cat("\nTest of the hypothesis that", factors, if(factors == 1)
            "factor is" else "factors are", "sufficient.\n")
        cat("The chi square statistic is", round(x$STATISTIC, 2), "on", x$dof,
            if(x$dof == 1) "degree" else "degrees",
            "of freedom.\nThe p-value is", signif(x$PVAL, 3), "\n")
    } else {
        cat(paste("\nThe degrees of freedom for the model is",
                  x$dof, "and the fit was", round(x$criteria["objective"], 4),
                  "\n"))
    }
    invisible(x)
}

varimax <- function(x, normalize = TRUE, eps = 1e-5)
{
    nc <- ncol(x)
    if(nc < 2) return(x)
    if(normalize) {
        sc <- sqrt(drop(apply(x, 1L, function(x) sum(x^2))))
        x <- x/sc
    }
    p <- nrow(x)
    TT <- diag(nc)
    d <- 0
    for(i in 1L:1000L) {
        z <- x %*% TT
        B  <- t(x) %*% (z^3 - z %*% diag(drop(rep(1, p) %*% z^2))/p)
        sB <- La.svd(B)
        TT <- sB$u %*% sB$vt
        dpast <- d
        d <- sum(sB$d)
        if(d < dpast * (1 + eps)) break
    }
    z <- x %*% TT
    if(normalize) z <- z * sc
    dimnames(z) <- dimnames(x)
    class(z) <- "loadings"
    list(loadings = z, rotmat = TT)
}

promax <- function(x, m = 4)
{
    if(ncol(x) < 2) return(x)
    dn <- dimnames(x)
    xx <- varimax(x)
    x <- xx$loadings
    Q <- x * abs(x)^(m-1)
    U <- lm.fit(x, Q)$coefficients
    d <- diag(solve(t(U) %*% U))
    U <- U %*% diag(sqrt(d))
    dimnames(U) <- NULL
    z <- x %*% U
    U <- xx$rotmat %*% U
    dimnames(z) <- dn
    class(z) <- "loadings"
    list(loadings = z, rotmat = U)
}
