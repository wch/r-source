aov <- function(formula, data = NULL, projections = FALSE, qr = TRUE,
                contrasts = NULL, ...)
{
    Terms <- if(missing(data)) terms(formula, "Error")
    else terms(formula, "Error", data = data)
    indError <- attr(Terms, "specials")$Error
    if(length(indError) > 1)
        stop("there are ", length(indError), "Error terms: only 1 is allowed")
    lmcall <- Call <- match.call()
    lmcall[[1]] <- as.name("lm")
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
        ##  helmert contrasts can be helpful: do we want to force them?
        ##  this version does for the Error model.
        opcons <- options("contrasts")
        options(contrasts=c("contr.helmert", "contr.poly"))
        on.exit(options(opcons))
        allTerms <- Terms
        errorterm <-  attr(Terms, "variables")[[1 + indError]]
        eTerm <- deparse(errorterm[[2]], width = 500, backtick = TRUE)
        intercept <- attr(Terms, "intercept")
        ecall <- lmcall
        ecall$formula <-
            as.formula(paste(deparse(formula[[2]], width = 500,
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
        if(rank.e < length(er.fit$coef))
            warning("Error model is singular")
        qty <- er.fit$resid
        maov <- is.matrix(qty)
        asgn.e <- er.fit$assign[qr.e$piv[1:rank.e]]
        ## we want this to label the rows of qtx, not cols of x.
        maxasgn <- length(nmstrata)-1
        nobs <- NROW(qty)
        if(nobs > rank.e) {
            result <- vector("list", maxasgn + 2)
            asgn.e[(rank.e+1):nobs] <- maxasgn + 1
            nmstrata <- c(nmstrata, "Within")
        } else result <- vector("list", maxasgn + 1)
        names(result) <- nmstrata
        lmcall$formula <- form <-
            update(formula, paste(". ~ .-", deparse(errorterm, width = 500,
                                                    backtick = TRUE)))
        Terms <- terms(form)
        lmcall$method <- "model.frame"
        mf <- eval(lmcall, parent.frame())
        xvars <- as.character(attr(Terms, "variables"))[-1]
        if ((yvar <- attr(Terms, "response")) > 0)
            xvars <- xvars[-yvar]
        if (length(xvars) > 0) {
            xlev <- lapply(mf[xvars], levels)
            xlev <- xlev[!sapply(xlev, is.null)]
        } else xlev <- NULL
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
            if(is.null(dny)) dny <- paste("Y", 1:nc, sep="")
            dimnames(qty) <- list(seq(nrow(qty)), dny)
        } else dimnames(qty) <- list(seq(nrow(qty)), NULL)
        qtx <- qr.qty(qr.e, qtx)
        dimnames(qtx) <- list(seq(nrow(qtx)) , dnx)
        for(i in seq(along=nmstrata)) {
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
                fiti <- list(coefficients = numeric(0), residuals = y,
                             fitted.values = 0 * y, weights = wts, rank = 0,
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
        dput(cl)
    }
    asgn <- x$assign[x$qr$pivot[1:x$rank]]
    effects <- x$effects
    if(!is.null(effects))
        effects <- as.matrix(effects)[seq(along=asgn),,drop=FALSE]
    rdf <- x$df.resid
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
        if(!intercept && uasgn[1] == 0) keep[1] <- FALSE
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
            if(length(ss) > 1) {
                rn <- colnames(x$fitted)
                if(is.null(rn)) rn <- paste("resp", 1:length(ss))
            } else rn <- "Sum of Squares"
            dimnames(tmp) <- list(c(rn, "Deg. of Freedom"), "Residuals")
            print(tmp, quote = FALSE, right = TRUE)
            cat("\n")
            cat("Residual standard error:", sapply(sqrt(ss/rdf), format), "\n")
        } else
        print(matrix(0, 2, 1, dimnames=
                     list(c("Sum of Squares", "Deg. of Freedom"), "<empty>")))
    } else {
        if(rdf > 0) {
            nterms <- nterms + 1
            df <- c(df, rdf)
            ss <- rbind(ss, RSS)
            nmeffect <- c(nmeffect, "Residuals")
        }
        ssp <- apply(zapsmall(ss), 2, format)
        tmp <- t(cbind(ssp, format(df)))
        if(ncol(effects) > 1) {
            rn <- colnames(x$coef)
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
            cat("Residual standard error:", sapply(rs, format), "\n")
        }
        coef <- as.matrix(x$coef)[,1]
        R <- x$qr$qr
        R <- R[1:min(dim(R)), ,drop=FALSE]
        R[lower.tri(R)] <- 0
        if(rank < (nc <- length(coef))) {
            cat(paste(nc - rank, "out of", nc, "effects not estimable\n"))
            R <- R[, 1:rank, drop = FALSE]
        }
        d2 <- sum(abs(diag(R)))
        diag(R) <- 0
        if(sum(abs(R))/d2 > tol)
            cat("Estimated effects may be unbalanced\n")
        else cat("Estimated effects are balanced\n")
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
            if(i == 0 || names[i+1] %in% ns) next
            f <- rownames(factors)[factors[, i] > 0]
            sp <- f %in% ns
            if(any(sp)) {              # some marginal terms are split
                if(sum(sp) > 1) {
                    old <- split[ f[sp] ]
                    nn <- f[sp]
                    names(nn) <- nn
                    marg <- lapply(nn, function(x)
                                   df.names[asgn == (match(x, names) - 1)])
                    term.coefs <- strsplit(df.names[asgn == i], ":", fixed=TRUE)
                    ttc <- sapply(term.coefs, function(x) x[sp])
                    rownames(ttc) <- nn
                    splitnames <- apply(expand.grid(lapply(old, names)), 1,
                                        function(x) paste(x, collapse="."))
                    names(splitnames) <- splitnames
                    tmp <- sapply(nn, function(i)
                                  names(old[[i]])[match(ttc[i, ], marg[[i]])] )
                    tmp <- apply(tmp, 1, function(x) paste(x, collapse="."))
                    new <- lapply(splitnames, function(x) match(x, tmp))
                    split[[ names[i+1] ]] <-
                        new[sapply(new, function(x) length(x) > 0)]
                } else {
                    old <- split[[ f[sp] ]]
                    marg.coefs <- df.names[asgn == (match(f[sp], names) - 1)]
                    term.coefs <- strsplit(df.names[asgn == i], ":", fixed=TRUE)
                    ttc <- sapply(term.coefs, function(x) x[sp])
                    new <- lapply(old, function(x)
                                  seq(along=ttc)[ttc %in% marg.coefs[x]])
                    split[[ names[i+1] ]] <- new
                }
            }
        }
        split
    }

    asgn <- object$assign[object$qr$pivot[1:object$rank]]
    uasgn <- unique(asgn)
    nterms <- length(uasgn)
    effects <- object$effects
    if(!is.null(effects))
        effects <- as.matrix(effects)[seq(along=asgn),,drop=FALSE]
    rdf <- object$df.resid
    nmeffect <- c("(Intercept)", attr(object$terms, "term.labels"))
    coef <- as.matrix(object$coef)
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if(!is.null(wt)) resid <- resid * wt^0.5
    nresp <- NCOL(resid)
    ans <- vector("list", nresp)
    if(nresp > 1) {
        names(ans) <- character(nresp)
        for (y in 1:nresp) {
            cn <- colnames(resid)[y]
            if(is.null(cn) || cn == "") cn <- y
            names(ans)[y] <- paste(" Response", cn)
        }
    }

    if(!is.null(effects) && !missing(split)) {
        ns <- names(split)
        if(!is.null(Terms <- object$terms)) {
            if(!is.list(split))
                stop("the split argument must be a list")
            if(!all(ns %in% nmeffect))
                stop("unknown name(s) in the split list")
        }
        if(expand.split) {
            df.names <- names(coef(object))
            split <- splitInteractions(split, attr(Terms, "factors"),
                                       nmeffect, asgn, df.names)
            ns <- names(split)
        }
    }

    for (y in 1:nresp) {
        if(is.null(effects)) {
            nterms <- 0
            df <- ss <- ms <- numeric(0)
            nmrows <- character(0)
        } else {
            df <- ss <- numeric(0)
            nmrows <- character(0)
            for(i in seq(nterms)) {
                ai <- (asgn == uasgn[i])
                df <- c(df, sum(ai))
                ss <- c(ss, sum(effects[ai, y]^2))
                nmi <- nmeffect[1 + uasgn[i]]
                nmrows <- c(nmrows, nmi)
                if(!missing(split) && !is.na(int <- match(nmi, ns))) {
                    df <- c(df, unlist(lapply(split[[int]], length)))
                    if(is.null(nms <- names(split[[int]])))
                        nms <- paste("C", seq(along = split[[int]]), sep = "")
                    ss <- c(ss, unlist(lapply(split[[int]],
                                              function(i, e)
                                              sum(e[i]^2), effects[ai, y])))
                    nmrows <- c(nmrows, paste("  ", nmi, ": ", nms, sep = ""))
                }
            }
        }
        if(rdf > 0) {
            df <- c(df, rdf)
            ss <- c(ss, sum(resid[, y]^2))
            nmrows <- c(nmrows,  "Residuals")
        }
        nt <- length(df)
        ms <- ifelse(df > 0, ss/df, NA)
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
        row.names(x) <- format(nmrows)
        if(!keep.zero.df) x <- x[df > 0, ]
        pm <- pmatch("(Intercept)", row.names(x), 0)
        if(!intercept && pm > 0) x <- x[-pm ,]
        ans[[y]] <- x
    }
    class(ans) <- c("summary.aov", "listof")
    ans
}

print.summary.aov <-
    function(x, digits = max(3, getOption("digits") - 3), symbolic.cor = FALSE,
             signif.stars= getOption("show.signif.stars"),	...)
{
    if (length(x) == 1)  print(x[[1]], ...)
    else NextMethod()
    invisible(x)
}

coef.aov <- function(object, ...)
{
    z <- object$coef
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
        if(exists("fractions", mode="function")) fractions(x)
        else {
            class(x) <- "mtable"
            x[abs(x) < 1e-6] <- NA
            x
        }
    }
    PartPatt <- function(x) {
        z <- zapsmall(x) != 0
        if(any(z)) {
            xx <- abs(signif(x[z], 2))
            ll <- length(unique(xx))
            if(ll > 10) xx <- cut(xx, 9) else if(ll == 1) x[] <- 1
            x[z] <- paste(ifelse(x[z] > 0, " ", "-"), xx, sep = "")
        }
        x[!z] <- ""
        collabs <- colnames(x)
        if(length(collabs)) {
            collabs <- abbreviate(sub("\\.", "", collabs), 3)
        } else  collabs <-1:ncol(x)
        colnames(x) <- collabs
        class(x) <- "mtable"
        x
    }
    Model <- object$terms
    attributes(Model) <- NULL
    value <- list(Model = Model)
    R <- object$qr$qr
    R <- R[1:min(dim(R)),, drop=FALSE]
    R[lower.tri(R)] <- 0
    d <- dim(R)
    rank <- object$rank
    p <- d[2]
    if(complete) {                      # full rank, no aliasing
        value$Complete <-
            if(is.null(p) || rank == p) NULL else {
                p1 <- 1:rank
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
    if(nx[1] == "(Intercept)") {
        mn <- x[[1]]$coef
        if(is.matrix(mn)) {
            cat("\nGrand Means:\n")
            print(format(mn[1,]), quote=FALSE)
        } else cat("\nGrand Mean:", format(mn[1]), "\n")
        nx <- nx[-1]
    }
    for(ii in seq(along = nx)) {
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
    if(strata[1] == "(Intercept)") {
        strata <- strata[-1]
        object <- object[-1]
    }
    x <- vector(length = length(strata), mode = "list")
    names(x) <- paste("Error:", strata)
    for(i in seq(along = strata))
        x[[i]] <- do.call("summary", c(list(object = object[[i]]), dots))
    class(x) <- "summary.aovlist"
    x
}

print.summary.aovlist <- function(x, ...)
{
    nn <- names(x)
    for (i in nn) {
        cat("\n", i, "\n", sep="")
        print(x[[i]], ...)
    }
    invisible(x)
}

coef.listof <- function(object, ...)
{
    val <- vector("list", length(object))
    names(val) <- names(object)
    for(i in seq(along=object)) val[[i]] <- coef(object[[i]])
    class(val) <- "listof"
    val
}

se.contrast <- function(object, ...) UseMethod("se.contrast")

se.contrast.aov <-
    function(object, contrast.obj, coef = contr.helmert(ncol(contrast))[, 1],
             data = NULL, ...)
{
    contrast.weight.aov <- function(object, contrast)
    {
        asgn <- object$assign[object$qr$pivot[1:object$rank]]
        uasgn <- unique(asgn)
        nterms <- length(uasgn)
        nmeffect <- c("(Intercept)",
                      attr(object$terms, "term.labels"))[1 + uasgn]
        effects <- as.matrix(qr.qty(object$qr, contrast))
        res <- matrix(0, nrow = nterms, ncol = ncol(effects),
                      dimnames = list(nmeffect, colnames(contrast)))
        for(i in seq(nterms)) {
            select <- (asgn == uasgn[i])
            res[i,] <- colSums(effects[seq(along=asgn)[select], , drop = FALSE]^2)
        }
        res
    }
    if(is.null(data)) contrast.obj <- eval(contrast.obj)
    else contrast.obj <- eval(substitute(contrast.obj), data, parent.frame())
    if(!is.matrix(contrast.obj)) { # so a list
        if(!missing(coef)) {
            if(sum(coef) != 0)
                stop("coef must define a contrast, i.e., sum to 0")
            if(length(coef) != length(contrast.obj))
                stop("coef must have same length as contrast.obj")
        }
        contrast <-
            sapply(contrast.obj, function(x)
               {
                   if(!is.logical(x))
                       stop("each element of ", substitute(contrasts.list),
                            " must be logical")
                   x/sum(x)
               })
        contrast <- contrast %*% coef
        if(!any(contrast) || all(is.na(contrast)))
            stop("the contrast defined is empty (has no TRUE elements)")
    } else {
        contrast <- contrast.obj
        if(any(abs(colSums(contrast)) > 1e-8))
            stop("columns of contrast.obj must define a contrast (sum to zero)")
        if(length(colnames(contrast)) == 0)
            colnames(contrast) <- paste("Contrast", seq(ncol(contrast)))
    }
    weights <- contrast.weight.aov(object, contrast)
    rdf <- object$df.resid
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if(!is.null(wt)) resid <- resid * wt^0.5
    rse <- sum(resid^2)/rdf
    if(!is.matrix(contrast.obj)) sqrt(sum(weights) * rse)
    else sqrt(rse * colSums(weights))
}

se.contrast.aovlist <-
    function(object, contrast.obj, coef = contr.helmert(ncol(contrast))[, 1],
             data = NULL, ...)
{
    contrast.weight.aovlist <- function(object, contrast)
    {
        e.qr <- attr(object, "error.qr")
        if(!is.qr(e.qr))
            stop("argument does not include an error 'qr' component")
        c.qr <- qr.qty(e.qr, contrast)
        e.assign <- attr(e.qr$qr, "assign")
        n.object <- length(object)
        e.assign <- c(e.assign,
                      rep.int(n.object - 1, nrow(c.qr) - length(e.assign)))
        res <- vector(length = n.object, mode = "list")
        names(res) <- names(object)
        for(j in seq(along=names(object))) {
            strata <- object[[j]]
            if(is.qr(strata$qr)) {
                scontrast <- c.qr[e.assign == (j - 1), , drop = FALSE]
                effects <- as.matrix(qr.qty(strata$qr, scontrast))
                asgn <- strata$assign[strata$qr$pivot[1:strata$rank]]
                uasgn <- unique(asgn)
                nm <- c("(Intercept)", attr(strata$terms, "term.labels"))
                res.i <-
                    matrix(0, length(asgn), ncol(effects),
                           dimnames = list(nm[1 + uasgn], colnames(contrast)))
                for(i in seq(along = asgn)) {
                    select <- (asgn == uasgn[i])
                    res.i[i, ] <-
                        colSums(effects[seq(along=asgn)[select], , drop = FALSE]^2)
                }
                res[[j]] <- res.i
            }
        }
        res
    }
    SS <- function(aov.object)
    {
        rdf <- aov.object$df.resid
        if(is.null(rdf)) {
            nobs <- length(aov.object$residuals)
            rank <- aov.object$rank
            rdf <- nobs - rank
        }
        resid <- as.matrix(aov.object$residuals)
        wt <- aov.object$weights
        if(!is.null(wt)) resid <- resid * wt^0.5
        sum(resid^2)/rdf
    }
    if(is.null(attr(object, "error.qr"))) {
        cat("Refitting model to allow projection\n")
        object <- update(object, qr = TRUE)
    }
    contrast.obj <-
        if(is.null(data)) eval(contrast.obj)
        else eval(substitute(contrast.obj), data, parent.frame())
    if(!is.matrix(contrast.obj)) {
        if(!missing(coef)) {
            if(sum(coef) != 0)
                stop("coef must define a contrast, i.e., sum to 0")
            if(length(coef) != length(contrast.obj))
                stop("coef must have same length as contrast.obj")
        }
        contrast <-
            sapply(contrast.obj,
                   function(x) {
                       if(!is.logical(x))
                           stop("Each element of ",
                                substitute(contrast.obj),
                                " must be logical")
                       x/sum(x)
                   })
        contrast <- contrast %*% coef
        if(!any(contrast))
            stop("the contrast defined is empty (has no TRUE elements)")
    }
    else {
        contrast <- contrast.obj
        if(any(abs(colSums(contrast)) > 1e-8))
            stop("columns of contrast.obj must define a contrast(sum to zero)")
        if(length(colnames(contrast)) == 0)
            colnames(contrast) <- paste("Contrast", seq(ncol(contrast)))
    }
    weights <- contrast.weight.aovlist(object, contrast)
    weights <- weights[-match("(Intercept)", names(weights))]
    effic <- eff.aovlist(object)
    ## Need to identify the lowest stratum where each nonzero term appears
    eff.used <- apply(effic, 2,
                      function(x, ind = seq(length(x))) {
                          temp <- (x > 0)
                          if(sum(temp) == 1) temp
                          else max(ind[temp]) == ind
                      })
    strata.nms <- rownames(effic)[row(eff.used)[eff.used]]
    var.nms <- colnames(effic)[col(eff.used)[eff.used]]
    rse.list <- sapply(object[unique(strata.nms)], SS)
    wgt <- matrix(0, nrow = length(var.nms), ncol = ncol(contrast),
                  dimnames = list(var.nms, colnames(contrast)))
    for(i in seq(length(var.nms)))
        wgt[i, ] <- weights[[strata.nms[i]]][var.nms[i], , drop = FALSE]
    rse <- rse.list[strata.nms]
    eff <- effic[eff.used]
    drop(sqrt((rse/eff^2) %*% wgt))
}
