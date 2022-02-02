#  File src/library/stats/R/manova.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

manova <- function(...)
{
    Call <- fcall <- match.call()
    fcall[[1L]] <- quote(stats::aov)
    result <- eval(fcall, parent.frame())
    if(inherits(result, "aovlist")) {
        for(i in seq_along(result)) {
            if(!inherits(result[[i]], "maov")) stop("need multiple responses")
            class(result[[i]]) <- c("manova", oldClass(result[[i]]))
        }
        attr(result, "call") <- Call
    } else {
        if(!inherits(result, "maov")) stop("need multiple responses")
        class(result) <- c("manova", oldClass(result))
        result$call <- Call
    }
    result
}

summary.manova <-
    function(object,
             test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
             intercept = FALSE, tol = 1e-7, ...)
{
    if(!inherits(object, "maov"))
        stop(gettextf("object must be of class %s or %s",
                      dQuote("manova"), dQuote("maov")),
             domain = NA)
    test <- match.arg(test)

    asgn <- object$assign[object$qr$pivot[1L:object$rank]]
    uasgn <- unique(asgn)
    nterms <- length(uasgn)
    effects <- object$effects
    if (!is.null(effects))
        effects <- as.matrix(effects)[seq_along(asgn), , drop = FALSE]
    rdf <- object$df.residual
    nmeffect <- c("(Intercept)", attr(object$terms, "term.labels"))
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if (!is.null(wt)) resid <- resid * sqrt(wt)
    nresp <- NCOL(resid)
    if(nresp <= 1) stop("need multiple responses")

    if (is.null(effects)) {
        df <- nterms <- 0
        ss <- list(0)
        nmrows <- character()
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
    pm <- pmatch("(Intercept)", nmrows, 0L)
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
        eigs <- array(NA, c(nterms, nresp), dimnames =
                          list(nmrows[-nt], NULL))
        stats <- matrix(NA, nt, 5, dimnames = list(nmrows, c(test,
                                       "approx F", "num Df", "den Df", "Pr(>F)")))
        sc <- sqrt(sss <- diag(ss[[nt]]))
        ## Let us try to distinguish bad scaling and near-perfect fit
        for(i in seq_len(nterms)[ok]) sss <- sss + diag(ss[[i]])
        sc[sc < sqrt(sss)*1e-6] <- 1
        D <- diag(1/sc)
        rss.qr <- qr(D %*% ss[[nt]] %*% D, tol=tol)
        if(rss.qr$rank < ncol(resid))
            stop(gettextf("residuals have rank %d < %d",
                          rss.qr$rank, ncol(resid)), domain = NA)
        if(!is.null(rss.qr))
            for(i in seq_len(nterms)[ok]) {
                A1 <- qr.coef(rss.qr, D %*% ss[[i]] %*% D)
                eigs[i, ] <- Re(eigen(A1, symmetric = FALSE, only.values = TRUE)$values)
                stats[i, 1L:4L] <-
                    switch(test,
			   "Pillai" = 		Pillai(eigs[i, ], df[i], df[nt]),
			   "Wilks" = 		Wilks (eigs[i, ], df[i], df[nt]),
			   "Hotelling-Lawley" = HL    (eigs[i, ], df[i], df[nt]),
			   "Roy" =		Roy   (eigs[i, ], df[i], df[nt]))
                ok <- stats[, 2L] >= 0 & stats[, 3L] > 0 & stats[, 4L] > 0
                ok <- !is.na(ok) & ok
                stats[ok, 5L] <- pf(stats[ok, 2L], stats[ok, 3L], stats[ok, 4L],
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
