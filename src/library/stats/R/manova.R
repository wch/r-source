manova <- function(...)
{
    Call <- fcall <- match.call()
    fcall[[1]] <- as.name("aov")
    result <- eval(fcall, parent.frame())
    if(inherits(result, "aovlist")) {
        for(i in seq(along=result)) {
            if(!inherits(result[[i]], "maov")) stop("need multiple response")
            class(result[[i]]) <- c("manova", oldClass(result[[i]]))
        }
        attr(result, "call") <- Call
    } else {
        if(!inherits(result, "maov")) stop("need multiple response")
        class(result) <- c("manova", oldClass(result))
        result$call <- Call
    }
    result
}

summary.manova <-
    function(object,
             test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
             intercept = FALSE, ...)
{
    Pillai <- function(eig, q, df.res)
    {
        test <- sum(eig/(1 + eig))
        p <- length(eig)
        s <- min(p, q)
        n <- 0.5 * (df.res - p - 1)
        m <- 0.5 * (abs(p - q) - 1)
        tmp1 <- 2 * m + s + 1
        tmp2 <- 2 * n + s + 1
        c(test, (tmp2/tmp1 * test)/(s - test), s*tmp1, s*tmp2)
    }

    Wilks <- function(eig, q, df.res)
    {
        test <- prod(1/(1 + eig))
        p <- length(eig)
        tmp1 <- df.res - 0.5 * (p - q + 1)
        tmp2 <- (p * q - 2)/4
        tmp3 <- p^2 + q^2 - 5
        tmp3 <-  if(tmp3 > 0) sqrt(((p*q)^2 - 4)/tmp3) else 1
        c(test, ((test^(-1/tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2))/p/q,
          p * q, tmp1 * tmp3 - 2 * tmp2)
    }

    HL <- function(eig, q, df.res)
    {
        test <- sum(eig)
        p <- length(eig)
        m <- 0.5 * (abs(p - q) - 1)
        n <- 0.5 * (df.res - p - 1)
        s <- min(p, q)
        tmp1 <- 2 * m + s + 1
        tmp2 <- 2 * (s * n + 1)
        c(test, (tmp2 * test)/s/s/tmp1, s * tmp1, tmp2)
    }

    Roy <- function(eig, q, df.res)
    {
        p <- length(eig)
        test <- max(eig)
        tmp1 <- max(p, q)
        tmp2 <- df.res - tmp1 + q
        c(test, (tmp2 * test)/tmp1, tmp1, tmp2)
    }

    if(!inherits(object, "maov"))
        stop("object must be of class \"manova\" or \"maov\"")
    test <- match.arg(test)

    asgn <- object$assign[object$qr$pivot[1:object$rank]]
    uasgn <- unique(asgn)
    nterms <- length(uasgn)
    effects <- object$effects
    if (!is.null(effects))
        effects <- as.matrix(effects)[seq(along = asgn), , drop = FALSE]
    rdf <- object$df.resid
    nmeffect <- c("(Intercept)", attr(object$terms, "term.labels"))
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if (!is.null(wt)) resid <- resid * wt^0.5
    nresp <- NCOL(resid)
    if(nresp <= 1) stop("need multiple response")

    if (is.null(effects)) {
        df <- nterms <- 0
        ss <- list(0)
        nmrows <- character(0)
    } else {
        df <- numeric(nterms)
        ss <- list(nterms)
        nmrows <- character(nterms)
        for (i in seq(nterms)) {
            ai <- (asgn == uasgn[i])
            nmrows[i] <- nmeffect[1 + uasgn[i]]
            df[i] <- sum(ai)
            ss[[i]] <- crossprod(effects[ai, , drop=FALSE])
        }
    }
    pm <- pmatch("(Intercept)", nmrows, 0)
    if (!intercept && pm > 0) {
        nterms <- nterms - 1
        df <- df[-pm]
        nmrows <- nmrows[-pm]
        ss <- ss[-pm]
    }
    names(ss) <- nmrows

    nt <- nterms
    if (rdf > 0) {
        nt <- nterms + 1
        df[nt] <- rdf
        ss[[nt]] <- crossprod(resid)
        names(ss)[nt] <- nmrows[nt] <- "Residuals"
        ok <- df[-nt] > 0
        eigs <- array(NA, c(nterms, nresp))
        dimnames(eigs) <- list(nmrows[-nt], NULL)
        stats <- matrix(NA, nt, 5)
        dimnames(stats) <-  list(nmrows,
                                 c(test, "approx F", "num Df", "den Df",
                                   "Pr(>F)"))
        rss.qr <- qr(ss[[nt]])
        if(rss.qr$rank < ncol(resid))
            stop("residuals have rank ", rss.qr$rank," < ", ncol(resid))
        if(!is.null(rss.qr))
            for(i in seq(len=nterms)[ok]) {
                eigs[i, ] <- Re(eigen(qr.coef(rss.qr, ss[[i]]),
                                       symmetric = FALSE)$values)
                stats[i, 1:4] <-
                    switch(test,
                           "Pillai" = Pillai(eigs[i,  ], df[i], df[nt]),
                           "Wilks" = Wilks(eigs[i,  ], df[i], df[nt]),
                           "Hotelling-Lawley" = HL(eigs[i,  ], df[i], df[nt]),
                           "Roy" = Roy(eigs[i,  ], df[i], df[nt]))
                ok <- stats[, 2] >= 0 & stats[, 3] > 0 & stats[, 4] > 0
                ok <- !is.na(ok) & ok
                stats[ok, 5] <- pf(stats[ok, 2], stats[ok, 3], stats[ok, 4],
                                   lower.tail = FALSE)

            }
        x <- list(row.names = nmrows, SS = ss,
                  Eigenvalues = eigs, stats = cbind(Df=df, stats=stats))
    } else x <- list(row.names = nmrows, SS = ss, Df = df)
    class(x) <- "summary.manova"
    x
}

print.summary.manova <- function(x, digits = getOption("digits"), ...)
{
    if(length(stats <- x$stats)) {
        print.anova(stats)
    } else {
        cat("No error degrees of freedom\n\n")
        print(data.frame(Df = x$Df, row.names = x$row.names))
    }
    invisible(x)
}
