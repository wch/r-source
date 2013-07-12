#  File src/library/stats/R/aov.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
#  Copyright (C) 1998 B. D. Ripley
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
#  http://www.r-project.org/Licenses/

aov <- function(formula, data = NULL, projections = FALSE, qr = TRUE,
                contrasts = NULL, ...)
{
    Terms <- if(missing(data)) terms(formula, "Error")
    else terms(formula, "Error", data = data)
    indError <- attr(Terms, "specials")$Error
    ## NB: this is only used for n > 1, so singular form makes no sense
    ## in English.  But some languages have multiple plurals.
    if(length(indError) > 1L)
        stop(sprintf(ngettext(length(indError),
                              "there are %d Error terms: only 1 is allowed",
                              "there are %d Error terms: only 1 is allowed"),
                     length(indError)), domain = NA)
    lmcall <- Call <- match.call()
    lmcall[[1L]] <- quote(stats::lm)
    lmcall$singular.ok <- TRUE
    if(projections) qr <- lmcall$qr <- TRUE
    lmcall$projections <- NULL
    if(is.null(indError)) {
        ## no Error term
        fit <- eval(lmcall, parent.frame())
        if(projections) fit$projections <- proj(fit)
        class(fit) <- if(inherits(fit, "mlm"))
            c("maov", "aov", oldClass(fit)) else c("aov", oldClass(fit))
        fit$call <- Call
        return(fit)
    } else {
        if(pmatch("weights", names(match.call()), 0L))
            stop("weights are not supported in a multistratum aov() fit")
        ##  Helmert contrasts can be helpful: do we want to force them?
        ##  this version does for the Error model.
        opcons <- options("contrasts")
        options(contrasts = c("contr.helmert", "contr.poly"))
        on.exit(options(opcons))
        allTerms <- Terms
        errorterm <-  attr(Terms, "variables")[[1 + indError]]
        eTerm <- deparse(errorterm[[2L]], width.cutoff = 500L, backtick = TRUE)
        intercept <- attr(Terms, "intercept")
        ecall <- lmcall
        ecall$formula <-
            as.formula(paste(deparse(formula[[2L]], width.cutoff = 500L,
                                     backtick = TRUE), "~", eTerm,
                             if(!intercept) "- 1"),
                       env=environment(formula))

        ecall$method <- "qr"
        ecall$qr <- TRUE
        ecall$contrasts <- NULL
        er.fit <- eval(ecall, parent.frame())
        options(opcons)
        nmstrata <- attr(terms(er.fit), "term.labels")
        ## remove backticks from simple labels for strata (only)
        nmstrata <- sub("^`(.*)`$", "\\1", nmstrata)
        nmstrata <- c("(Intercept)", nmstrata)
        qr.e <- er.fit$qr
        rank.e <- er.fit$rank
        if(rank.e < NROW(er.fit$coefficients))
            warning("Error() model is singular")
        qty <- er.fit$residuals
        maov <- is.matrix(qty)
        asgn.e <- er.fit$assign[qr.e$pivot[1L:rank.e]]
        ## we want this to label the rows of qtx, not cols of x.
        maxasgn <- length(nmstrata) - 1L
        nobs <- NROW(qty)
	len <- if(nobs > rank.e) {
	    asgn.e[(rank.e+1):nobs] <- maxasgn + 1L
	    nmstrata <- c(nmstrata, "Within")
	    maxasgn + 2L
	} else maxasgn + 1L
        result <- setNames(vector("list", len), nmstrata)
        lmcall$formula <- form <-
            update(formula, paste(". ~ .-", deparse(errorterm, width.cutoff = 500L, backtick = TRUE)))
        Terms <- terms(form)
        lmcall$method <- "model.frame"
        mf <- eval(lmcall, parent.frame())
        xlev <- .getXlevels(Terms, mf)
        resp <- model.response(mf)
        qtx <- model.matrix(Terms, mf, contrasts)
        cons <- attr(qtx, "contrasts")
        dnx <- colnames(qtx)
        asgn.t <- attr(qtx, "assign")
        if(length(wts <- model.weights(mf))) {
            wts <- sqrt(wts)
            resp <- resp * wts
            qtx <- qtx * wts
        }
        qty <- as.matrix(qr.qty(qr.e, resp))
        if((nc <- ncol(qty)) > 1) {
            dny <- colnames(resp)
            if(is.null(dny)) dny <- paste0("Y", 1L:nc)
            dimnames(qty) <- list(seq(nrow(qty)), dny)
        } else dimnames(qty) <- list(seq(nrow(qty)), NULL)
        qtx <- qr.qty(qr.e, qtx)
        dimnames(qtx) <- list(seq(nrow(qtx)) , dnx)
        for(i in seq_along(nmstrata)) {
            select <- asgn.e==(i-1)
            ni <- sum(select)
            if(!ni) next
            ## helpful to drop constant columns.
            xi <- qtx[select, , drop = FALSE]
            cols <- colSums(xi^2) > 1e-5
            if(any(cols)) {
                xi <- xi[, cols, drop = FALSE]
                attr(xi, "assign") <- asgn.t[cols]
                fiti <- lm.fit(xi, qty[select,,drop=FALSE])
                fiti$terms <- Terms
            } else {
                y <- qty[select,,drop=FALSE]
                fiti <- list(coefficients = numeric(), residuals = y,
                             fitted.values = 0 * y, weights = wts, rank = 0L,
                             df.residual = NROW(y))
            }
            if(projections) fiti$projections <- proj(fiti)
            class(fiti) <- c(if(maov) "maov", "aov", oldClass(er.fit))
            result[[i]] <- fiti
        }
        ## drop empty strata
        result <- result[!sapply(result, is.null)]
        class(result) <- c("aovlist", "listof")
        if(qr) attr(result, "error.qr") <- qr.e
        attr(result, "call") <- Call
        if(length(wts)) attr(result, "weights") <- wts
        attr(result, "terms") <- allTerms
        attr(result, "contrasts") <- cons
        attr(result, "xlevels") <- xlev
        result
    }
}

print.aov <-
function(x, intercept = FALSE, tol = .Machine$double.eps^0.5, ...)
{
    if(!is.null(cl <- x$call)) {
        cat("Call:\n   ")
        dput(cl, control = NULL)
    }
    qrx <- if(x$rank) qr(x)
    asgn <- x$assign[qrx$pivot[1L:x$rank]]
    effects <- x$effects
    if(!is.null(effects))
        effects <- as.matrix(effects)[seq_along(asgn),,drop=FALSE]
    rdf <- x$df.residual
    resid <- as.matrix(x$residuals)
    wt <- x$weights
    if(!is.null(wt)) resid <- resid * wt^0.5
    RSS <- colSums(resid^2)
    uasgn <- unique(asgn)
    nmeffect <- c("(Intercept)", attr(x$terms, "term.labels"))[1+uasgn]
    nterms <- length(uasgn)
    nresp <- NCOL(effects)
    df <- numeric(nterms)
    ss <- matrix(NA, nterms, nresp)
    if(nterms) {
        for(i in seq(nterms)) {
            ai <- asgn==uasgn[i]
            df[i] <- sum(ai)
            ef <- effects[ai,, drop=FALSE]
            ss[i,] <- if(sum(ai) > 1) colSums(ef^2) else ef^2
        }
        keep <- df > 0
        if(!intercept && uasgn[1L] == 0) keep[1L] <- FALSE
        nmeffect <- nmeffect[keep]
        df <- df[keep]
        ss <- ss[keep,,drop=FALSE]
        nterms <- length(df)
    }
    cat("\nTerms:\n")
    if(nterms == 0) {
        ## empty model
        if(rdf > 0) {
            ss <- RSS
            ssp <- sapply(ss, format)
            if(!is.matrix(ssp)) ssp <- t(ssp)
            tmp <- as.matrix(c(ssp, format(rdf)))
            if(length(ss) > 1L) {
                rn <- colnames(x$fitted.values)
                if(is.null(rn)) rn <- paste("resp", seq_along(ss))
            } else rn <- "Sum of Squares"
            dimnames(tmp) <- list(c(rn, "Deg. of Freedom"), "Residuals")
            print(tmp, quote = FALSE, right = TRUE)
            cat("\n")
            cat("Residual standard error: ", sapply(sqrt(ss/rdf), format),
                "\n", sep = "")
        } else
        print(matrix(0, 2L, 1L, dimnames=
                     list(c("Sum of Squares", "Deg. of Freedom"), "<empty>")))
    } else {
        if(rdf > 0L) {
            nterms <- nterms + 1L
            df <- c(df, rdf)
            ss <- rbind(ss, RSS)
            nmeffect <- c(nmeffect, "Residuals")
        }
        ssp <- apply(zapsmall(ss), 2L, format)
        tmp <- t(cbind(ssp, format(df)))
        if(ncol(effects) > 1L) {
            rn <- colnames(x$coeffficients)
            if(is.null(rn)) rn <- paste("resp", seq(ncol(effects)))
        } else rn <- "Sum of Squares"
        dimnames(tmp) <- list(c(rn, "Deg. of Freedom"), nmeffect)
        print(tmp, quote = FALSE, right = TRUE)
        rank <- x$rank
#        int <- attr(x$terms, "intercept")
#        nobs <- NROW(x$residuals) - !(is.null(int) || int == 0)
        cat("\n")
        if(rdf > 0) {
            rs <- sqrt(RSS/rdf)
            cat("Residual standard error: ", sapply(rs, format), "\n", sep = "")
        }
        coef <- as.matrix(x$coefficients)[, 1L]
        R <- qrx$qr
        R <- R[1L:min(dim(R)), ,drop=FALSE]
        R[lower.tri(R)] <- 0
        if(rank < (nc <- length(coef))) {
            cat(paste(nc - rank, "out of", nc, "effects not estimable\n"))
            R <- R[, 1L:rank, drop = FALSE]
        }
        d2 <- sum(abs(diag(R)))
        diag(R) <- 0
        if(sum(abs(R))/d2 > tol)
            cat("Estimated effects may be unbalanced\n")
        else cat("Estimated effects are balanced\n")
        if(nzchar(mess <- naprint(x$na.action))) cat(mess, "\n", sep = "")
    }
    invisible(x)
}

summary.aov <- function(object, intercept = FALSE, split,
                        expand.split = TRUE, keep.zero.df = TRUE, ...)
{
    splitInteractions <- function(split, factors, names, asgn, df.names)
    {
        ns <- names(split)
        for(i in unique(asgn)) {
            if(i == 0 || names[i+1L] %in% ns) next
            f <- rownames(factors)[factors[, i] > 0]
            sp <- f %in% ns
            if(any(sp)) {              # some marginal terms are split
                if(sum(sp) > 1) {
                    old <- split[ f[sp] ]
		    nn <- setNames(nm = f[sp])
                    marg <- lapply(nn, function(x)
                                   df.names[asgn == (match(x, names) - 1L)])
                    term.coefs <- strsplit(df.names[asgn == i], ":", fixed=TRUE)
                    ttc <- sapply(term.coefs, function(x) x[sp])
                    rownames(ttc) <- nn
		    splitnames <-
			setNames(nm = apply(expand.grid(lapply(old, names)), 1L,
				 function(x) paste(x, collapse=".")))
                    tmp <- sapply(nn, function(i)
                                  names(old[[i]])[match(ttc[i, ], marg[[i]])] )
                    tmp <- apply(tmp, 1L, function(x) paste(x, collapse="."))
                    new <- lapply(splitnames, function(x) match(x, tmp))
                    split[[ names[i+1L] ]] <-
                        new[sapply(new, function(x) length(x) > 0L)]
                } else {
                    old <- split[[ f[sp] ]]
                    marg.coefs <- df.names[asgn == (match(f[sp], names) - 1L)]
                    term.coefs <- strsplit(df.names[asgn == i], ":", fixed=TRUE)
                    ttc <- sapply(term.coefs, function(x) x[sp])
                    new <- lapply(old, function(x)
                                  seq_along(ttc)[ttc %in% marg.coefs[x]])
                    split[[ names[i+1L] ]] <- new
                }
            }
        }
        split
    }

    asgn <- object$assign[object$qr$pivot[1L:object$rank]]
    uasgn <- unique(asgn)
    nterms <- length(uasgn)
    effects <- object$effects
    if(!is.null(effects))
        effects <- as.matrix(effects)[seq_along(asgn),,drop=FALSE]
    rdf <- object$df.residual
    nmeffect <- c("(Intercept)", attr(object$terms, "term.labels"))
    coef <- as.matrix(object$coefficients)
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if(!is.null(wt)) resid <- resid * wt^0.5
    nresp <- NCOL(resid)
    ans <- vector("list", nresp)
    if(nresp > 1) {
        names(ans) <- character(nresp)
        for (y in 1L:nresp) {
            cn <- colnames(resid)[y]
            if(is.null(cn) || cn == "") cn <- y
            names(ans)[y] <- paste(" Response", cn)
        }
    }

    if(!is.null(effects) && !missing(split)) {
        ns <- names(split)
        if(!is.null(Terms <- object$terms)) {
            if(!is.list(split))
                stop("the 'split' argument must be a list")
            if(!all(ns %in% nmeffect)) {
                na <- sum(!ns %in% nmeffect)
                stop(sprintf(ngettext(na,
                                      "unknown name %s in the 'split' list",
                                      "unknown names %s in the 'split' list"),
                             paste(sQuote(ns[na]), collapse = ", ")),
                     domain = NA)
            }
        }
        if(expand.split) {
            df.names <- names(coef(object))
            split <- splitInteractions(split, attr(Terms, "factors"),
                                       nmeffect, asgn, df.names)
            ns <- names(split)
        }
    }

    for (y in 1L:nresp) {
        if(is.null(effects)) {
            nterms <- 0
            df <- ss <- ms <- numeric()
            nmrows <- character()
        } else {
            df <- ss <- numeric()
            nmrows <- character()
            for(i in seq(nterms)) {
                ai <- (asgn == uasgn[i])
                df <- c(df, sum(ai))
                ss <- c(ss, sum(effects[ai, y]^2))
                nmi <- nmeffect[1 + uasgn[i]]
                nmrows <- c(nmrows, nmi)
                if(!missing(split) && !is.na(int <- match(nmi, ns))) {
                    df <- c(df, unlist(lapply(split[[int]], length)))
                    if(is.null(nms <- names(split[[int]])))
                        nms <- paste0("C", seq_along(split[[int]]))
                    ss <- c(ss, unlist(lapply(split[[int]],
                                              function(i, e)
                                              sum(e[i]^2), effects[ai, y])))
                    nmrows <- c(nmrows, paste0("  ", nmi, ": ", nms))
                }
            }
        }
        if(rdf > 0) {
            df <- c(df, rdf)
            ss <- c(ss, sum(resid[, y]^2))
            nmrows <- c(nmrows,  "Residuals")
        }
        nt <- length(df)
        ms <- ifelse(df > 0L, ss/df, NA)
        x <- list(Df = df, "Sum Sq" = ss, "Mean Sq" = ms)
        if(rdf > 0) {
            TT <- ms/ms[nt]
            TP <- pf(TT, df, rdf, lower.tail = FALSE)
            TT[nt] <- TP[nt] <- NA
            x$"F value" <- TT
            x$"Pr(>F)" <- TP
            ## 'nterms' ~= 'Residuals' have no P-value
        }
        class(x) <- c("anova", "data.frame")
        attr(x, "row.names") <- format(nmrows)
        if(!keep.zero.df) x <- x[df > 0, ]
        pm <- pmatch("(Intercept)", row.names(x), 0L)
        if(!intercept && pm > 0) x <- x[-pm ,]
        ans[[y]] <- x
    }
    class(ans) <- c("summary.aov", "listof")
    attr(ans, "na.action") <- object$na.action
    ans
}

print.summary.aov <-
    function(x, digits = max(3L, getOption("digits") - 3L),
             symbolic.cor = FALSE,
             signif.stars = getOption("show.signif.stars"), ...)
{
    if (length(x) == 1L)
        print(x[[1L]], digits = digits, symbolic.cor = symbolic.cor,
              signif.stars = signif.stars)
    else NextMethod()
    if(nzchar(mess <- naprint(attr(x, "na.action")))) cat(mess, "\n", sep = "")
    invisible(x)
}

coef.aov <- function(object, ...)
{
    z <- object$coefficients
    z[!is.na(z)]
}

alias <- function(object, ...) UseMethod("alias")

alias.formula <- function(object, data, ...)
{
    lm.obj <- if(missing(data)) aov(object) else aov(object, data)
    alias(lm.obj, ...)
}

alias.lm <- function(object, complete = TRUE, partial = FALSE,
                     partial.pattern = FALSE, ...)
{
    CompPatt <- function(x, ...) {
        x[abs(x) < 1e-6] <- 0
        MASS::fractions(x)
    }
    PartPatt <- function(x) {
        z <- zapsmall(x) != 0
        if(any(z)) {
            xx <- abs(signif(x[z], 2))
            ll <- length(unique(xx))
            if(ll > 10) xx <- cut(xx, 9) else if(ll == 1) x[] <- 1
            x[z] <- paste0(ifelse(x[z] > 0, " ", "-"), xx)
        }
        x[!z] <- ""
        collabs <- colnames(x)
        collabs <- if(length(collabs)) abbreviate(sub("\\.", "", collabs), 3)
        else 1L:ncol(x)
        colnames(x) <- collabs
        class(x) <- "mtable"
        x
    }
    Model <- object$terms
    attributes(Model) <- NULL
    value <- list(Model = Model)
    R <- qr(object)$qr
    R <- R[1L:min(dim(R)), , drop=FALSE]
    R[lower.tri(R)] <- 0
    d <- dim(R)
    rank <- object$rank
    p <- d[2L]
    if(complete) {                      # full rank, no aliasing
        value$Complete <-
            if(is.null(p) || rank == p) NULL else {
                p1 <- 1L:rank
                X <- R[p1, p1]
                Y <-  R[p1, -p1, drop = FALSE]
                beta12 <- as.matrix(qr.coef(qr(X), Y))
                # dimnames(beta12) <- list(dn[p1], dn[ -p1])
                CompPatt(t(beta12))
            }
    }
    if(partial) {
        tmp <- summary.lm(object)$cov.unscaled
        ses <- sqrt(diag(tmp))
        beta11 <- tmp /outer(ses, ses)
        beta11[row(beta11) >= col(beta11)] <- 0
        beta11[abs(beta11) < 1e-6] <- 0
        if(all(beta11 == 0)) beta11 <- NULL
        else if(partial.pattern) beta11 <- PartPatt(beta11)
        value$Partial <- beta11
    }
    class(value) <- "listof"
    value
}

print.aovlist <- function(x, ...)
{
    cl <- attr(x, "call")
    if(!is.null(cl)) {
        cat("\nCall:\n")
        dput(cl)
    }
    if(!is.null(attr(x, "weights")))
        cat("Note: The results below are on the weighted scale\n")
    nx <- names(x)
    if(nx[1L] == "(Intercept)") {
        mn <- x[[1L]]$coefficients
        if(is.matrix(mn)) {
            cat("\nGrand Means:\n")
            print(format(mn[1,]), quote = FALSE)
        } else cat("\nGrand Mean: ", format(mn[1L]), "\n", sep = "")
        nx <- nx[-1L]
    }
    for(ii in seq_along(nx)) {
        i <- nx[ii]
        cat("\nStratum ", ii, ": ", i, "\n", sep = "")
        xi <- x[[i]]
        print(xi, ...)
    }
    invisible(x)
}

summary.aovlist <- function(object, ...)
{
    if(!is.null(attr(object, "weights")))
        cat("Note: The results below are on the weighted scale\n")
    dots <- list(...)
    strata <- names(object)
    if(strata[1L] == "(Intercept)") {
        strata <- strata[-1L]
        object <- object[-1L]
    }
    x <- setNames(vector(length = length(strata), mode = "list"),
		  paste("Error:", strata))
    for(i in seq_along(strata))
        x[[i]] <- do.call("summary", c(list(object = object[[i]]), dots))
    class(x) <- "summary.aovlist"
    x
}

print.summary.aovlist <- function(x, ...)
{
    nn <- names(x)
    for (i in nn) {
        cat("\n", i, "\n", sep = "")
        print(x[[i]], ...)
    }
    invisible(x)
}

coef.listof <- function(object, ...)
{
    val <- setNames(vector("list", length(object)), names(object))
    for(i in seq_along(object)) val[[i]] <- coef(object[[i]])
    class(val) <- "listof"
    val
}

se.contrast <- function(object, ...) UseMethod("se.contrast")

se.contrast.aov <-
    function(object, contrast.obj,
             coef = contr.helmert(ncol(contrast))[, 1L],
             data = NULL, ...)
{
    contrast.weight.aov <- function(object, contrast)
    {
        qro <- qr(object)
        asgn <- object$assign[qro$pivot[1L:object$rank]]
        uasgn <- unique(asgn)
        nterms <- length(uasgn)
        nmeffect <- c("(Intercept)",
                      attr(object$terms, "term.labels"))[1L + uasgn]
        effects <- as.matrix(qr.qty(qro, contrast))
        res <- matrix(0, nrow = nterms, ncol = ncol(effects),
                      dimnames = list(nmeffect, colnames(contrast)))
        for(i in seq(nterms)) {
            select <- (asgn == uasgn[i])
            res[i,] <- colSums(effects[seq_along(asgn)[select], , drop = FALSE]^2)
        }
        res
    }
    if(is.null(data)) contrast.obj <- eval(contrast.obj)
    else contrast.obj <- eval(substitute(contrast.obj), data, parent.frame())
    if(!is.matrix(contrast.obj)) { # so a list
        if(!missing(coef)) {
            if(sum(coef) != 0)
                stop("'coef' must define a contrast, i.e., sum to 0")
            if(length(coef) != length(contrast.obj))
                stop("'coef' must have same length as 'contrast.obj'")
        }
        contrast <-
            sapply(contrast.obj, function(x)
               {
                   if(!is.logical(x))
                       stop(gettextf("each element of '%s' must be logical",
                                     substitute(contrasts.list)),
                            domain = NA)
                   x/sum(x)
               })
        if(!length(contrast) || all(is.na(contrast)))
            stop("the contrast defined is empty (has no TRUE elements)")
        contrast <- contrast %*% coef
    } else {
        contrast <- contrast.obj
        if(any(abs(colSums(contrast)) > 1e-8))
            stop("columns of 'contrast.obj' must define a contrast (sum to zero)")
        if(!length(colnames(contrast)))
            colnames(contrast) <- paste("Contrast", seq(ncol(contrast)))
    }
    weights <- contrast.weight.aov(object, contrast)
    rdf <- object$df.residual
    if(rdf == 0) stop("no degrees of freedom for residuals")
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if(!is.null(wt)) resid <- resid * wt^0.5
    rse <- sum(resid^2)/rdf
    if(!is.matrix(contrast.obj)) sqrt(sum(weights) * rse)
    else sqrt(rse * colSums(weights))
}

se.contrast.aovlist <-
    function(object, contrast.obj,
             coef = contr.helmert(ncol(contrast))[, 1L],
             data = NULL, ...)
{
    contrast.weight.aovlist <- function(object, contrast)
    {
        e.qr <- attr(object, "error.qr")
        if(!is.qr(e.qr))
            stop("'object' does not include an error 'qr' component")
        c.qr <- qr.qty(e.qr, contrast)
        e.assign <- attr(e.qr$qr, "assign")
        n.object <- length(object)
        e.assign <- c(e.assign,
                      rep.int(n.object - 1L, nrow(c.qr) - length(e.assign)))
	res <- setNames(vector("list", n.object), names(object))
        for(j in seq_along(names(object))) {
            strata <- object[[j]]
            if(is.qr(strata$qr)) {
                scontrast <- c.qr[e.assign == (j - 1L), , drop = FALSE]
                effects <- as.matrix(qr.qty(strata$qr, scontrast))
                asgn <- strata$assign[strata$qr$pivot[1L:strata$rank]]
                uasgn <- unique(asgn)
                nm <- c("(Intercept)", attr(strata$terms, "term.labels"))
                res.i <-
                    matrix(0, length(uasgn), ncol(effects),
                           dimnames = list(nm[1L + uasgn], colnames(contrast)))
                for(i in seq_along(uasgn)) {
                    select <- (asgn == uasgn[i])
                    res.i[i, ] <-
                        colSums(effects[seq_along(asgn)[select], , drop = FALSE]^2)
                }
                res[[j]] <- res.i
            }
        }
        res
    }
    SS <- function(aov.object)
    {
        rdf <- aov.object$df.residual
        if(is.null(rdf)) {
            nobs <- length(aov.object$residuals)
            rank <- aov.object$rank
            rdf <- nobs - rank
        }
        if(rdf == 0) return(NA)
        resid <- as.matrix(aov.object$residuals)
        wt <- aov.object$weights
        if(!is.null(wt)) resid <- resid * wt^0.5
        sum(resid^2)/rdf
    }
    if(is.null(attr(object, "error.qr"))) {
        message("Refitting model to allow projection")
        object <- update(object, qr = TRUE)
    }
    contrast.obj <-
        if(is.null(data)) eval(contrast.obj)
        else eval(substitute(contrast.obj), data, parent.frame())
    if(!is.matrix(contrast.obj)) {
        if(!missing(coef)) {
            if(sum(coef) != 0)
                stop("'coef' must define a contrast, i.e., sum to 0")
            if(length(coef) != length(contrast.obj))
                stop("'coef' must have same length as 'contrast.obj'")
        }
        contrast <-
            sapply(contrast.obj,
                   function(x) {
                       if(!is.logical(x))
                           stop(gettextf("each element of '%s' must be logical",
                                         substitute(contrasts.obj)),
                                domain = NA)
                       x/sum(x)
                   })
        if(!length(contrast) || all(is.na(contrast)))
            stop("the contrast defined is empty (has no TRUE elements)")
        contrast <- contrast %*% coef
    }
    else {
        contrast <- contrast.obj
        if(any(abs(colSums(contrast)) > 1e-8))
            stop("columns of 'contrast.obj' must define a contrast(sum to zero)")
        if(!length(colnames(contrast)))
            colnames(contrast) <- paste("Contrast", seq(ncol(contrast)))
    }
    weights <- contrast.weight.aovlist(object, contrast)
    weights <- weights[-match("(Intercept)", names(weights))]
    effic <- eff.aovlist(object)
    ## Need to identify the lowest stratum where each nonzero term appears
    eff.used <- apply(effic, 2L,
                      function(x, ind = seq_along(x)) {
                          temp <- (x > 0)
                          if(sum(temp) == 1) temp
                          else max(ind[temp]) == ind
                      })
    if(is.matrix(eff.used)) {
        strata.nms <- rownames(effic)[row(eff.used)[eff.used]]
        var.nms <- colnames(effic)[col(eff.used)[eff.used]]
    } else {
        strata.nms <- rownames(effic)
        var.nms <- colnames(effic)
    }
    rse.list <- sapply(object[unique(strata.nms)], SS)
    wgt <- matrix(0, nrow = length(var.nms), ncol = ncol(contrast),
                  dimnames = list(var.nms, colnames(contrast)))
    for(i in seq_along(var.nms))
        wgt[i, ] <- weights[[strata.nms[i]]][var.nms[i], , drop = FALSE]
    rse <- rse.list[strata.nms]
    eff <- effic[eff.used]
    drop(sqrt((rse/eff^2) %*% wgt))
}
