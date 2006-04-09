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
            stop(gettextf("residuals have rank %d < %d",
                          rss.qr$rank, ncol(resid)), domain = NA)
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
