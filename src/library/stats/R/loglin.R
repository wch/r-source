#  File src/library/stats/R/loglin.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

loglin <- function(table, margin, start = rep(1, length(table)), fit =
                   FALSE, eps = 0.1, iter = 20L, param = FALSE, print =
                   TRUE) {
    rfit <- fit

    dtab <- dim(table)
    nvar <- length(dtab)

    ncon <- length(margin)
    conf <- matrix(0L, nrow = nvar, ncol = ncon)
    nmar <- 0
    varnames <- names(dimnames(table))
    for (k in seq_along(margin)) {
        tmp <- margin[[k]]
        if (is.character(tmp)) {
            ## Rewrite margin names to numbers
            tmp <- match(tmp, varnames)
            margin[[k]] <- tmp
        }
        if (!is.numeric(tmp) || any(is.na(tmp) | tmp <= 0))
            stop("'margin' must contain names or numbers corresponding to 'table'")
        conf[seq_along(tmp), k] <- tmp
        nmar <- nmar + prod(dtab[tmp])
    }

    ntab <- length(table)
    if (length(start) != ntab ) stop("'start' and 'table' must be same length")

    z <- .Call(C_LogLin, dtab, conf, table, start, nmar, eps, iter)

    if (print)
        cat(z$nlast, "iterations: deviation", z$dev[z$nlast], "\n")

    fit <- z$fit
    attributes(fit) <- attributes(table)

    ## Pearson chi-sq test statistic
    observed <- as.vector(table[start > 0])
    expected <- as.vector(fit[start > 0])
    pearson <- sum((observed - expected)^2 / expected)

    ## Likelihood Ratio Test statistic
    observed <- as.vector(table[table * fit > 0])
    expected <- as.vector(fit[table * fit > 0])
    lrt <- 2 * sum(observed * log(observed / expected))

    ## Compute degrees of freedom.
    ## Use a dyadic-style representation for the (possible) subsets B.
    ## Let u_i(B) = 1 if i is contained in B and 0 otherwise.  Then B
    ## <-> u(B) = (u_1(B),...,u_N(B)) <-> \sum_{i=1}^N u_i(B) 2^{i-1}.
    ## See also the code for 'dyadic' below which computes the u_i(B).
    subsets <- function(x) {
        y <- list(vector(mode(x), length = 0))
        for (i in seq_along(x)) {
            y <- c(y, lapply(y, "c", x[i]))
        }
        y[-1L]
    }
    df <- rep.int(0, 2^nvar)
    for (k in seq_along(margin)) {
        terms <- subsets(margin[[k]])
        for (j in seq_along(terms))
            df[sum(2 ^ (terms[[j]] - 1))] <- prod(dtab[terms[[j]]] - 1)
    }

    ## Rewrite margin numbers to names if possible
    if (!is.null(varnames) && all(nzchar(varnames))) {
        for (k in seq_along(margin))
            margin[[k]] <- varnames[margin[[k]]]
    } else {
        varnames <- as.character(1 : ntab)
    }

    y <- list(lrt = lrt,
              pearson = pearson,
              df = ntab - sum(df) - 1,
              margin = margin)

    if (rfit)
        y$fit <- fit

    if (param) {
        fit <- log(fit)
        terms <- seq_along(df)[df > 0]

        parlen <- length(terms) + 1
        parval <- list(parlen)
        parnam <- character(parlen)

        parval[[1L]] <- mean(fit)
        parnam[1L] <- "(Intercept)"
        fit <- fit - parval[[1L]]

        ## Get the u_i(B) in the rows of 'dyadic', see above.
        dyadic <- NULL
        while(any(terms > 0)) {
            dyadic <- cbind(dyadic, terms %% 2)
            terms <- terms %/% 2
        }
        dyadic <- dyadic[order(rowSums(dyadic)), , drop = FALSE]

        for (i in 2 : parlen) {
            vars <- which(dyadic[i - 1, ] > 0)
            parval[[i]] <- apply(fit, vars, mean)
            parnam[i] <- paste(varnames[vars], collapse = ".")
            fit <- sweep(fit, vars, parval[[i]], check.margin=FALSE)
        }

        names(parval) <- parnam
        y$param <- parval
    }

    return(y)
}
