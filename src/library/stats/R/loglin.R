loglin <- function(table, margin, start = rep(1, length(table)), fit =
                   FALSE, eps = 0.1, iter = 20, param = FALSE, print =
                   TRUE) {
    rfit <- fit

    dtab <- dim(table)
    nvar <- length(dtab)

    ncon <- length(margin)
    conf <- matrix(0, nrow = nvar, ncol = ncon)
    nmar <- 0
    varnames <- names(dimnames(table))
    for (k in seq(along = margin)) {
        tmp <- margin[[k]]
        if (is.character(tmp)) {
            ## Rewrite margin names to numbers
            tmp <- match(tmp, varnames)
            margin[[k]] <- tmp
        }
        conf[1:length(tmp), k] <- tmp
        nmar <- nmar + prod(dtab[tmp])
    }

    ntab <- length(table)
    if (length(start) != ntab ) stop("'start' and 'table' must be same length")

    storage.mode(conf) <- "integer"
    ## NOTE: We make no use of the arguments locmar, nmar, marg, nu, and
    ## u.  It might make sense to eliminate them and simplify the underlying C
    ## code accordingly.
    z <- .C("loglin",
            as.integer(nvar),
            as.integer(dtab),
            as.integer(ncon),
            conf,
            as.integer(ntab),
            as.double(table),
            fit = as.double(start),
            locmar = integer(ncon),
            as.integer(nmar),
            marginals = double(nmar),
            as.integer(ntab),
            u = double(ntab),
            as.double(eps),
            as.integer(iter),
            dev = double(iter),
            nlast = integer(1),
            ifault = integer(1),
            PACKAGE = "base")
    switch(z$ifault,
           stop("this should not happen"),
           stop("this should not happen"),
           warning("algorithm did not converge"),
           stop("incorrect specification of 'table' or 'start'")
           )

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
        for (i in seq(along = x)) {
            y <- c(y, lapply(y, "c", x[i]))
        }
        y[-1]
    }
    df <- rep.int(0, 2^nvar)
    for (k in seq(along = margin)) {
        terms <- subsets(margin[[k]])
        for (j in seq(along = terms))
            df[sum(2 ^ (terms[[j]] - 1))] <- prod(dtab[terms[[j]]] - 1)
    }

    ## Rewrite margin numbers to names if possible
    if (!is.null(varnames) && all(nchar(varnames) > 0)) {
        for (k in seq(along = margin))
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
        terms <- seq(length(df))[df > 0]

        parlen <- length(terms) + 1
        parval <- list(parlen)
        parnam <- character(parlen)

        parval[[1]] <- mean(fit)
        parnam[1] <- "(Intercept)"
        fit <- fit - parval[[1]]

        ## Get the u_i(B) in the rows of 'dyadic', see above.
        dyadic <- NULL
        while(any(terms > 0)) {
            dyadic <- cbind(dyadic, terms %% 2)
            terms <- terms %/% 2
        }
        dyadic <- dyadic[order(rowSums(dyadic)), ]

        for (i in 2 : parlen) {
            vars <- which(dyadic[i - 1, ] > 0)
            parval[[i]] <- apply(fit, vars, mean)
            parnam[i] <- paste(varnames[vars], collapse = ".")
            fit <- sweep(fit, vars, parval[[i]])
        }

        names(parval) <- parnam
        y$param <- parval
    }

    return(y)
}
