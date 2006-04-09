## Hmm, MM thinks diag(.) needs checking { diag(vec) when length(vec)==1 !}
## However, MM does not understand that factor analysis
##   is a *multi*variate technique!
factanal <-
    function (x, factors, data = NULL, covmat = NULL, n.obs = NA,
              subset, na.action, start = NULL,
              scores = c("none", "regression", "Bartlett"),
              rotation = "varimax",
              control = NULL, ...)
{
    sortLoadings <- function(Lambda)
    {
        cn <- colnames(Lambda)
        Phi <- attr(Lambda, "covariance")
        ssq <- apply(Lambda, 2, function(x) -sum(x^2))
        Lambda <- Lambda[, order(ssq), drop = FALSE]
        colnames(Lambda) <- cn
        neg <- colSums(Lambda) < 0
        Lambda[, neg] <- -Lambda[, neg]
        if(!is.null(Phi)) {
            unit <- ifelse(neg, -1, 1)
            attr(Lambda, "covariance") <-
                unit %*% Phi[order(ssq), order(ssq)] %*% unit
        }
        Lambda
    }
    cl <- match.call()
    na.act <- NULL
    if (is.list(covmat)) {
        if (any(is.na(match(c("cov", "n.obs"), names(covmat)))))
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
            mf[[1]] <- as.name("model.frame")
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
        stop(gettextf("%d factors is too many for %d variables", factors, p),
             domain = NA)
    sds <- sqrt(diag(cv))
    cv <- cv/(sds %o% sds)

    cn <- list(nstart = 1, trace = FALSE, lower = 0.005)
    cn[names(control)] <- control
    more <- list(...)[c("nstart", "trace", "lower", "opt", "rotate")]
    if(length(more)) cn[names(more)] <- more

    if(is.null(start)) {
        start <- (1 - 0.5*factors/p)/diag(solve(cv))
        if((ns <- cn$nstart) > 1)
            start <- cbind(start, matrix(runif(ns-1), p, ns-1, byrow=TRUE))
    }
    start <- as.matrix(start)
    if(nrow(start) != p)
        stop(gettextf("'start' must have %d rows", p), domain = NA)
    nc <- ncol(start)
    if(nc < 1) stop("no starting values supplied")

    best <- Inf
    for (i in 1:nc) {
        nfit <- factanal.fit.mle(cv, factors, start[, i],
                                 max(cn$lower, 0), cn$opt)
        if(cn$trace)
            cat("start", i, "value:", format(nfit$criteria[1]),
                "uniqs:", format(as.vector(round(nfit$uniquenesses, 4))), "\n")
        if(nfit$converged && nfit$criteria[1] < best) {
            fit <- nfit
            best <- fit$criteria[1]
        }
    }
    if(best == Inf) stop("unable to optimize from these starting value(s)")
    load <- fit$loadings
    if(rotation != "none") {
        rot <- do.call(rotation, c(list(load), cn$rotate))
        load <- if(is.list(rot)) rot$loadings else rot
    }
    fit$loadings <- sortLoadings(load)
    class(fit$loadings) <- "loadings"
    fit$na.action <- na.act # not used currently
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
        L <- E$vectors[, 1:q, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1:q] - 1, 0)), q)
        diag(sqrt(Psi)) %*% load
    }
    FAfn <- function(Psi, S, q)
    {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE, only.values = TRUE)
        e <- E$values[-(1:q)]
        e <- sum(log(e) - e) - q + nrow(S)
##        print(round(c(Psi, -e), 5))  # for tracing
        -e
    }
    FAgr <- function(Psi, S, q)
    {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1:q, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1:q] - 1, 0)), q)
        load <- diag(sqrt(Psi)) %*% load
        g <- load %*% t(load) + diag(Psi) - S
        diag(g)/Psi^2
    }
    p <- ncol(cmat)
    if(is.null(start))
        start <- (1 - 0.5*factors/p)/diag(solve(cmat))
    res <- optim(start, FAfn, FAgr, method = "L-BFGS-B",
                 lower = lower, upper = 1,
                 control = c(list(fnscale=1,
                 parscale = rep(0.01, length(start))), control),
                 q = factors, S = cmat)
    Lambda <- FAout(res$par, cmat, factors)
    dimnames(Lambda) <- list(dimnames(cmat)[[1]],
                             paste("Factor", 1:factors, sep = ""))
    p <- ncol(cmat)
    dof <- 0.5 * ((p - factors)^2 - p - factors)
    un <- res$par
    names(un) <- colnames(cmat)
    class(Lambda) <- "loadings"
    ans <- list(converged = res$convergence == 0,
                loadings = Lambda, uniquenesses = un,
                correlation = cmat,
                criteria = c(objective = res$value, counts = res$counts),
                factors = factors, dof = dof, method = "mle")
    class(ans) <- "factanal"
    ans
}

print.loadings <- function(x, digits = 3, cutoff = 0.1, sort = FALSE, ...)
{
    Lambda <- unclass(x)
    p <- nrow(Lambda)
    factors <- ncol(Lambda)
    if (sort) {
        mx <- max.col(abs(Lambda))
        ind <- cbind(1:p, mx)
        mx[abs(Lambda[ind]) < 0.5] <- factors + 1
        Lambda <- Lambda[order(mx, 1:p),]
    }
    cat("\nLoadings:\n")
    fx <- format(round(Lambda, digits))
    names(fx) <- NULL
    nc <- nchar(fx[1], type="c")
    fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
    print(fx, quote = FALSE, ...)
    vx <- colSums(x^2)
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
    print(x$loadings, digits = digits, ...)
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
        sc <- sqrt(drop(apply(x, 1, function(x) sum(x^2))))
        x <- x/sc
    }
    p <- nrow(x)
    TT <- diag(nc)
    d <- 0
    for(i in 1:1000) {
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
