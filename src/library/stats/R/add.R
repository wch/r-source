#  File src/library/stats/R/add.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1994-8 W. N. Venables and B. D. Ripley
#  Copyright (C) 1998-2012 The R Core Team
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


## version to return NA for df = 0, as R did before 2.7.0
safe_pchisq <- function(q, df, ...)
{
    df[df <= 0] <- NA
    pchisq(q=q, df=df, ...)
}
## and to avoid a warning
safe_pf <- function(q, df1, ...)
{
    df1[df1 <= 0] <- NA
    pf(q=q, df1=df1, ...)
}

## NB: functions in this file will use the 'stats' S3 generics for
## nobs(), terms() ....

add1 <- function(object, scope, ...) UseMethod("add1")

add1.default <- function(object, scope, scale = 0, test=c("none", "Chisq"),
			 k = 2, trace = FALSE, ...)
{
    if(missing(scope) || is.null(scope)) stop("no terms in scope")
    if(!is.character(scope))
	scope <- add.scope(object, update.formula(object, scope))
    if(!length(scope))
	stop("no terms in scope for adding to object")
#     newform <- update.formula(object,
#                               paste(". ~ . +", paste(scope, collapse="+")))
#     data <- model.frame(update(object, newform)) # remove NAs
#     object <- update(object, data = data)
    ns <- length(scope)
    ans <- matrix(nrow = ns + 1L, ncol = 2L,
                  dimnames = list(c("<none>", scope), c("df", "AIC")))
    ans[1L,  ] <- extractAIC(object, scale, k = k, ...)
    n0 <- nobs(object, use.fallback = TRUE)
    env <- environment(formula(object))
    for(i in seq_len(ns)) {
	tt <- scope[i]
	if(trace > 1) {
	    cat("trying +", tt, "\n", sep = "")
	    flush.console()
	}
	nfit <- update(object, as.formula(paste("~ . +", tt)),
                       evaluate = FALSE)
	nfit <- eval(nfit, envir=env) # was  eval.parent(nfit)
	ans[i+1L, ] <- extractAIC(nfit, scale, k = k, ...)
        nnew <- nobs(nfit, use.fallback = TRUE)
        if(all(is.finite(c(n0, nnew))) && nnew != n0)
            stop("number of rows in use has changed: remove missing values?")
    }
    dfs <- ans[, 1L] - ans[1L, 1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, AIC = ans[, 2L])
    test <- match.arg(test)
    if(test == "Chisq") {
	dev <- ans[, 2L] - k*ans[, 1L]
	dev <- dev[1L] - dev; dev[1L] <- NA
	nas <- !is.na(dev)
	P <- dev
	P[nas] <- safe_pchisq(dev[nas], dfs[nas], lower.tail=FALSE)
	aod[, c("LRT", "Pr(>Chi)")] <- list(dev, P)
    }
    head <- c("Single term additions", "\nModel:", deparse(formula(object)),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

##' @title Check for exact fit
##' @param object an lm object (hence using "$" instead of methods)
##' @return (unused / nothing explicitly)
check_exact <- function(object)
{
    w <- object$weights
    if(is.null(w)) {
        mss <- sum(object$fitted.values^2)
        rss <- sum(object$residuals^2)
    } else {
        mss <- sum(w * object$fitted.values^2)
        rss <- sum(w * object$residuals^2)
    }
    if(rss < 1e-10*mss)
	warning("attempting model selection on an essentially perfect fit is nonsense",
		call. = FALSE)
}

add1.lm <- function(object, scope, scale = 0, test=c("none", "Chisq", "F"),
		    x = NULL, k = 2,...)
{
    Fstat <- function(table, RSS, rdf) {
	dev <- table$"Sum of Sq"
	df <- table$Df
	rms <- (RSS - dev)/(rdf - df)
	Fs <- (dev/df)/rms
	Fs[df < .Machine$double.eps] <- NA
	P <- Fs
	nnas <- !is.na(Fs)
	P[nnas] <- safe_pf(Fs[nnas], df[nnas], rdf - df[nnas], lower.tail=FALSE)
	list(Fs=Fs, P=P)
    }

    check_exact(object)
    if(missing(scope) || is.null(scope)) stop("no terms in scope")
    if(!is.character(scope))
	scope <- add.scope(object, update.formula(object, scope))
    if(!length(scope))
	stop("no terms in scope for adding to object")
    oTerms <- attr(object$terms, "term.labels")
    int <- attr(object$terms, "intercept")
    ns <- length(scope)
    y <- object$residuals + object$fitted.values
    ## predict(object) applies na.action where na.exclude results in too long
    dfs <- numeric(ns+1)
    RSS <- numeric(ns+1)
    names(dfs) <- names(RSS) <- c("<none>", scope)
    add.rhs <- paste(scope, collapse = "+")
    add.rhs <- eval(parse(text = paste("~ . +", add.rhs), keep.source = FALSE))
    new.form <- update.formula(object, add.rhs)
    Terms <- terms(new.form)
    if(is.null(x)) {
	fc <- object$call
	fc$formula <- Terms
	## model.frame.lm looks at the terms part for the environment
	fob <- list(call = fc, terms = Terms)
	class(fob) <- oldClass(object)
	m <- model.frame(fob, xlev = object$xlevels)
	x <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
        offset <- model.offset(m)
        wt <- model.weights(m)
        oldn <- length(y)
        y <- model.response(m, "numeric")
        newn <- length(y)
        if(newn < oldn)
            warning(sprintf(ngettext(newn,
                                     "using the %d/%d row from a combined fit",
                                     "using the %d/%d rows from a combined fit"),
                            newn, oldn), domain = NA)
    } else {
        ## need to get offset and weights from somewhere
        wt <- object$weights
        offset <- object$offset
    }
    n <- nrow(x)
    Terms <- attr(Terms, "term.labels")
    asgn <- attr(x, "assign")
    ousex <- match(asgn, match(oTerms, Terms), 0L) > 0L
    if(int) ousex[1L] <- TRUE
    iswt <- !is.null(wt)
    X <- x[, ousex, drop = FALSE]
    z <- if(iswt) lm.wfit(X, y, wt, offset=offset)
    else lm.fit(X, y, offset=offset)
    dfs[1L] <- z$rank
    class(z) <- "lm" # needed as deviance.lm calls generic residuals()
    RSS[1L] <- deviance(z)
    ## workaround for PR#7842. terms.formula may have flipped interactions
    sTerms <- sapply(strsplit(Terms, ":", fixed=TRUE),
                     function(x) paste(sort(x), collapse=":"))
    for(tt in scope) {
        stt <- paste(sort(strsplit(tt, ":")[[1L]]), collapse=":")
	usex <- match(asgn, match(stt, sTerms), 0L) > 0L
	X <- x[, usex|ousex, drop = FALSE]
	z <- if(iswt) lm.wfit(X, y, wt, offset=offset)
        else lm.fit(X, y, offset=offset)
        class(z) <- "lm" # needed as deviance.lm calls generic residuals()
	dfs[tt] <- z$rank
	RSS[tt] <- deviance(z)
    }
    if(scale > 0) aic <- RSS/scale - n + k*dfs
    else aic <- n * log(RSS/n) + k*dfs
    dfs <- dfs - dfs[1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, "Sum of Sq" = c(NA, RSS[1L] - RSS[-1L]),
		      RSS = RSS, AIC = aic,
                      row.names = names(dfs), check.names = FALSE)
    if(scale > 0) names(aod) <- c("Df", "Sum of Sq", "RSS", "Cp")
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- aod$"Sum of Sq"
        if(scale == 0) {
            dev <- n * log(RSS/n)
            dev <- dev[1L] - dev
            dev[1L] <- NA
        } else dev <- dev/scale
        df <- aod$Df
        nas <- !is.na(df)
        dev[nas] <- safe_pchisq(dev[nas], df[nas], lower.tail=FALSE)
        aod[, "Pr(>Chi)"] <- dev
    } else if(test == "F") {
	rdf <- object$df.residual
	aod[, c("F value", "Pr(>F)")] <- Fstat(aod, aod$RSS[1L], rdf)
    }
    head <- c("Single term additions", "\nModel:", deparse(formula(object)),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

add1.glm <- function(object, scope, scale = 0, test=c("none", "Rao", "LRT",
                                                 "Chisq", "F"),
		     x = NULL, k = 2, ...)
{
    Fstat <- function(table, rdf) {
	dev <- table$Deviance
	df <- table$Df
	diff <- pmax(0, (dev[1L] - dev)/df)
	Fs <- diff/(dev/(rdf-df))
	Fs[df < .Machine$double.eps] <- NA
	P <- Fs
	nnas <- !is.na(Fs)
	P[nnas] <- safe_pf(Fs[nnas], df[nnas], rdf - df[nnas], lower.tail=FALSE)
	list(Fs=Fs, P=P)
    }
    test <- match.arg(test)
    if (test=="Chisq") test <- "LRT"

    if(!is.character(scope))
	scope <- add.scope(object, update.formula(object, scope))
    if(!length(scope))
	stop("no terms in scope for adding to object")
    oTerms <- attr(object$terms, "term.labels")
    int <- attr(object$terms, "intercept")
    ns <- length(scope)
    dfs <- dev <- score <- numeric(ns+1)
    names(dfs) <- names(dev) <- names(score) <- c("<none>", scope)
    add.rhs <- paste(scope, collapse = "+")
    add.rhs <- eval(parse(text = paste("~ . +", add.rhs), keep.source = FALSE))
    new.form <- update.formula(object, add.rhs)
    Terms <- terms(new.form)
    y <- object$y
    if(is.null(x)) {
	fc <- object$call
	fc$formula <- Terms
	## model.frame.glm looks at the terms part for the environment
	fob <- list(call = fc, terms = Terms)
	class(fob) <- oldClass(object)
	m <- model.frame(fob, xlev = object$xlevels)
        offset <- model.offset(m)
        wt <- model.weights(m)
	x <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
        oldn <- length(y)
        y <- model.response(m)
        if(!is.factor(y)) storage.mode(y) <- "double"
        ## binomial case has adjusted y and weights
        if(NCOL(y) == 2) {
            n <- y[, 1] + y[, 2]
            y <- ifelse(n == 0, 0, y[, 1]/n)
            if(is.null(wt)) wt <- rep.int(1, length(y))
            wt <- wt * n
        }
        newn <- length(y)
        if(newn < oldn)
            warning(sprintf(ngettext(newn,
                                     "using the %d/%d row from a combined fit",
                                     "using the %d/%d rows from a combined fit"),
                            newn, oldn), domain = NA)

    } else {
        ## need to get offset and weights from somewhere
        wt <- object$prior.weights
        offset <- object$offset
    }
    n <- nrow(x)
    if(is.null(wt)) wt <- rep.int(1, n)
    Terms <- attr(Terms, "term.labels")
    asgn <- attr(x, "assign")
    ousex <- match(asgn, match(oTerms, Terms), 0L) > 0L
    if(int) ousex[1L] <- TRUE
    X <- x[, ousex, drop = FALSE]
    z <-  glm.fit(X, y, wt, offset=offset,
                  family=object$family, control=object$control)
    dfs[1L] <- z$rank
    dev[1L] <- z$deviance
    r <- z$residuals
    w <- z$weights
    ## workaround for PR#7842. terms.formula may have flipped interactions
    sTerms <- sapply(strsplit(Terms, ":", fixed=TRUE),
                     function(x) paste(sort(x), collapse=":"))
    for(tt in scope) {
        stt <- paste(sort(strsplit(tt, ":")[[1L]]), collapse=":")
	usex <- match(asgn, match(stt, sTerms), 0L) > 0L
	X <- x[, usex|ousex, drop = FALSE]
	z <-  glm.fit(X, y, wt, offset=offset,
		      family=object$family, control=object$control)
	dfs[tt] <- z$rank
	dev[tt] <- z$deviance
        if (test=="Rao") {
          ## WLS for score test (comes out as model SS)
          zz <- glm.fit(X, r, w, offset=offset)
          score[tt] <- zz$null.deviance - zz$deviance
        }
    }
    if (scale == 0)
	dispersion <- summary(object, dispersion = NULL)$dispersion
    else dispersion <- scale
    fam <- object$family$family
    if(fam == "gaussian") {
	if(scale > 0) loglik <- dev/scale - n
	else loglik <- n * log(dev/n)
    } else loglik <- dev/dispersion
    aic <- loglik + k * dfs
    aic <- aic + (extractAIC(object, k = k)[2L] - aic[1L])
    dfs <- dfs - dfs[1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic,
		      row.names = names(dfs), check.names = FALSE)
    if(all(is.na(aic))) aod <- aod[, -3]
    test <- match.arg(test)
    if(test == "LRT") {
        dev <- pmax(0, loglik[1L] - loglik)
        dev[1L] <- NA
        LRT <- if(dispersion == 1) "LRT" else "scaled dev."
        aod[, LRT] <- dev
        nas <- !is.na(dev)
        dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail=FALSE)
        aod[, "Pr(>Chi)"] <- dev
    } else if(test == "Rao") {
        dev <- pmax(0, score) # roundoff guard
        dev[1L] <- NA
        nas <- !is.na(dev)
        SC <- if(dispersion == 1) "Rao score" else "scaled Rao sc."
        dev <- dev/dispersion
        aod[, SC] <- dev
        dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail=FALSE)
        aod[, "Pr(>Chi)"] <- dev
    } else if(test == "F") {
        if(fam == "binomial" || fam == "poisson")
            warning(gettextf("F test assumes quasi%s family", fam),
                    domain = NA)
	rdf <- object$df.residual
	aod[, c("F value", "Pr(>F)")] <- Fstat(aod, rdf)
    }
    head <- c("Single term additions", "\nModel:", deparse(formula(object)),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

add1.mlm <- function(object, scope, ...)
    stop("no 'add1' method implemented for \"mlm\" models")

drop1 <- function(object, scope, ...) UseMethod("drop1")

drop1.default <- function(object, scope, scale = 0, test=c("none", "Chisq"),
			  k = 2, trace = FALSE, ...)
{
    tl <- attr(terms(object), "term.labels")
    if(missing(scope)) scope <- drop.scope(object)
    else {
	if(!is.character(scope))
	    scope <- attr(terms(update.formula(object, scope)), "term.labels")
	if(!all(match(scope, tl, 0L) > 0L))
	    stop("scope is not a subset of term labels")
    }
    ns <- length(scope)
    ans <- matrix(nrow = ns + 1L, ncol = 2L,
                  dimnames =  list(c("<none>", scope), c("df", "AIC")))
    ans[1, ] <- extractAIC(object, scale, k = k, ...)
    n0 <- nobs(object, use.fallback = TRUE)
    env <- environment(formula(object))
    for(i in seq_len(ns)) {
	tt <- scope[i]
	if(trace > 1) {
	    cat("trying -", tt, "\n", sep = "")
	    flush.console()
        }
        nfit <- update(object, as.formula(paste("~ . -", tt)),
                       evaluate = FALSE)
	nfit <- eval(nfit, envir=env) # was  eval.parent(nfit)
	ans[i+1, ] <- extractAIC(nfit, scale, k = k, ...)
        nnew <- nobs(nfit, use.fallback = TRUE)
        if(all(is.finite(c(n0, nnew))) && nnew != n0)
            stop("number of rows in use has changed: remove missing values?")
    }
    dfs <- ans[1L , 1L] - ans[, 1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, AIC = ans[,2])
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- ans[, 2L] - k*ans[, 1L]
        dev <- dev - dev[1L] ; dev[1L] <- NA
        nas <- !is.na(dev)
        P <- dev
        P[nas] <- safe_pchisq(dev[nas], dfs[nas], lower.tail = FALSE)
        aod[, c("LRT", "Pr(>Chi)")] <- list(dev, P)
    }
    head <- c("Single term deletions", "\nModel:", deparse(formula(object)),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

drop1.lm <- function(object, scope, scale = 0, all.cols = TRUE,
		     test=c("none", "Chisq", "F"), k = 2, ...)
{
    check_exact(object)
    x <- model.matrix(object)
    offset <- model.offset(model.frame(object))
    iswt <- !is.null(wt <- object$weights)
    n <- nrow(x)
    asgn <- attr(x, "assign")
    tl <- attr(object$terms, "term.labels")
    if(missing(scope)) scope <- drop.scope(object)
    else {
	if(!is.character(scope))
	    scope <- attr(terms(update.formula(object, scope)), "term.labels")
	if(!all(match(scope, tl, 0L) > 0L))
	    stop("scope is not a subset of term labels")
    }
    ndrop <- match(scope, tl)
    ns <- length(scope)
    rdf <- object$df.residual
    chisq <- deviance.lm(object)
    dfs <- numeric(ns)
    RSS <- numeric(ns)
    y <- object$residuals + object$fitted.values
    ## predict(object) applies na.action where na.exclude results in too long
    na.coef <- seq_along(object$coefficients)[!is.na(object$coefficients)]
    for(i in seq_len(ns)) {
	ii <- seq_along(asgn)[asgn == ndrop[i]]
	jj <- setdiff(if(all.cols) seq(ncol(x)) else na.coef, ii)
	z <- if(iswt) lm.wfit(x[, jj, drop = FALSE], y, wt, offset=offset)
	else lm.fit(x[, jj, drop = FALSE], y, offset=offset)
	dfs[i] <- z$rank
        oldClass(z) <- "lm" # needed as deviance.lm calls residuals.lm
	RSS[i] <- deviance(z)
    }
    scope <- c("<none>", scope)
    dfs <- c(object$rank, dfs)
    RSS <- c(chisq, RSS)
    if(scale > 0) aic <- RSS/scale - n + k*dfs
    else aic <- n * log(RSS/n) + k*dfs
    dfs <- dfs[1L] - dfs
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, "Sum of Sq" = c(NA, RSS[-1L] - RSS[1L]),
		      RSS = RSS, AIC = aic,
                      row.names = scope, check.names = FALSE)
    if(scale > 0) names(aod) <- c("Df", "Sum of Sq", "RSS", "Cp")
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- aod$"Sum of Sq"
        if(scale == 0) {
            dev <- n * log(RSS/n)
            dev <- dev - dev[1L]
            dev[1L] <- NA
        } else dev <- dev/scale
        df <- aod$Df
        nas <- !is.na(df)
        dev[nas] <- safe_pchisq(dev[nas], df[nas], lower.tail=FALSE)
        aod[, "Pr(>Chi)"] <- dev
    } else if(test == "F") {
	dev <- aod$"Sum of Sq"
	dfs <- aod$Df
	rdf <- object$df.residual
	rms <- aod$RSS[1L]/rdf
	Fs <- (dev/dfs)/rms
	Fs[dfs < 1e-4] <- NA
	P <- Fs
	nas <- !is.na(Fs)
	P[nas] <- safe_pf(Fs[nas], dfs[nas], rdf, lower.tail=FALSE)
	aod[, c("F value", "Pr(>F)")] <- list(Fs, P)
    }
    head <- c("Single term deletions", "\nModel:", deparse(formula(object)),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

drop1.mlm <- function(object, scope, ...)
    stop("no 'drop1' method for \"mlm\" models")

drop1.glm <- function(object, scope, scale = 0, test=c("none", "Rao", "LRT", "Chisq", "F"),
		      k = 2, ...)
{
    test <- match.arg(test)
    if (test=="Chisq") test <- "LRT"
    x <- model.matrix(object)
#    iswt <- !is.null(wt <- object$weights)
    n <- nrow(x)
    asgn <- attr(x, "assign")
    tl <- attr(object$terms, "term.labels")
    if(missing(scope)) scope <- drop.scope(object)
    else {
	if(!is.character(scope))
	    scope <- attr(terms(update.formula(object, scope)), "term.labels")
	if(!all(match(scope, tl, 0L) > 0L))
	    stop("scope is not a subset of term labels")
    }
    ndrop <- match(scope, tl)
    ns <- length(scope)
    rdf <- object$df.residual
    chisq <- object$deviance
    dfs <- numeric(ns)
    dev <- numeric(ns)
    score <- numeric(ns)
    y <- object$y
    if(is.null(y)) {
        y <- model.response(model.frame(object))
        if(!is.factor(y)) storage.mode(y) <- "double"
    }
#    na.coef <- seq_along(object$coefficients)[!is.na(object$coefficients)]
    wt <- object$prior.weights
    if(is.null(wt)) wt <- rep.int(1, n)
    for(i in seq_len(ns)) {
	ii <- seq_along(asgn)[asgn == ndrop[i]]
	jj <- setdiff(seq(ncol(x)), ii)
	z <-  glm.fit(x[, jj, drop = FALSE], y, wt, offset=object$offset,
		      family=object$family, control=object$control)
	dfs[i] <- z$rank
	dev[i] <- z$deviance

        if (test=="Rao"){
            r <- z$residuals
            w <- z$weights
            ## Approximative refit of full model to residuals using WLS
            ## Score statistic comes out as (weighted) model SS
            zz <- glm.fit(x, r, w, offset=object$offset)
            score[i] <- zz$null.deviance - zz$deviance
        }
    }
    scope <- c("<none>", scope)
    dfs <- c(object$rank, dfs)
    dev <- c(chisq, dev)
    if (test=="Rao") {
      score <- c(NA, score)
    }
    dispersion <- if (is.null(scale) || scale == 0)
	summary(object, dispersion = NULL)$dispersion
    else scale
    fam <- object$family$family
    loglik <-
        if(fam == "gaussian") {
            if(scale > 0) dev/scale - n else n * log(dev/n)
        } else dev/dispersion
    aic <- loglik + k * dfs
    dfs <- dfs[1L] - dfs
    dfs[1L] <- NA
    aic <- aic + (extractAIC(object, k = k)[2L] - aic[1L])
    aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic,
		      row.names = scope, check.names = FALSE)
    if(all(is.na(aic))) aod <- aod[, -3]
    if(test == "LRT") {
        dev <- pmax(0, loglik - loglik[1L])
        dev[1L] <- NA
        nas <- !is.na(dev)
        LRT <- if(dispersion == 1) "LRT" else "scaled dev."
        aod[, LRT] <- dev
        dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail=FALSE)
        aod[, "Pr(>Chi)"] <- dev
    } else if(test == "Rao") {
        dev <- pmax(0, score) # roundoff guard
        nas <- !is.na(dev)
        SC <- if(dispersion == 1) "Rao score" else "scaled Rao sc."
        dev <- dev/dispersion
        aod[, SC] <- dev
        dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail=FALSE)
        aod[, "Pr(>Chi)"] <- dev
    } else if(test == "F") {
        if(fam == "binomial" || fam == "poisson")
            warning(gettextf("F test assumes 'quasi%s' family", fam),
                    domain = NA)
	dev <- aod$Deviance
	rms <- dev[1L]/rdf
        dev <- pmax(0, dev - dev[1L])
	dfs <- aod$Df
	rdf <- object$df.residual
	Fs <- (dev/dfs)/rms
	Fs[dfs < 1e-4] <- NA
	P <- Fs
	nas <- !is.na(Fs)
	P[nas] <- safe_pf(Fs[nas], dfs[nas], rdf, lower.tail=FALSE)
	aod[, c("F value", "Pr(>F)")] <- list(Fs, P)
    }
    head <- c("Single term deletions", "\nModel:", deparse(formula(object)),
	      if(!is.null(scale) && scale > 0)
	      paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

add.scope <- function(terms1, terms2)
{
    terms1 <- terms(terms1)
    terms2 <- terms(terms2)
    factor.scope(attr(terms1, "factors"),
		 list(add = attr(terms2, "factors")))$add
}

drop.scope <- function(terms1, terms2)
{
    terms1 <- terms(terms1)
    f2 <- if(missing(terms2)) numeric()
    else attr(terms(terms2), "factors")
    factor.scope(attr(terms1, "factors"), list(drop = f2))$drop
}

factor.scope <- function(factor, scope)
{
    drop <- scope$drop
    add <- scope$add

    if(length(factor) && !is.null(drop)) {# have base model
	nmdrop <- colnames(drop)
	facs <- factor
	if(length(drop)) {
	    nmfac <- colnames(factor)
            ## workaround as in PR#7842.
            ## terms.formula may have flipped interactions
            nmfac0 <- sapply(strsplit(nmfac, ":", fixed=TRUE),
                             function(x) paste(sort(x), collapse=":"))
            nmdrop0 <- sapply(strsplit(nmdrop, ":", fixed=TRUE),
                             function(x) paste(sort(x), collapse=":"))
	    where <- match(nmdrop0, nmfac0, 0L)
	    if(any(!where))
                stop(sprintf(ngettext(sum(where==0),
                                      "lower scope has term %s not included in model",
                                      "lower scope has terms %s not included in model"),
                             paste(sQuote(nmdrop[where==0]), collapse=", ")),
                     domain = NA)
	    facs <- factor[, -where, drop = FALSE]
	    nmdrop <- nmfac[-where]
	} else nmdrop <- colnames(factor)
	if(ncol(facs) > 1) {
            ## check no interactions will be left without margins.
	    keep <- rep.int(TRUE, ncol(facs))
	    f <- crossprod(facs > 0)
	    for(i in seq(keep)) keep[i] <- max(f[i, - i]) != f[i, i]
	    nmdrop <- nmdrop[keep]
	}
    } else nmdrop <- character()

    if(!length(add)) nmadd <- character()
    else {
	nmfac <- colnames(factor)
	nmadd <- colnames(add)
	if(!is.null(nmfac)) {
            ## workaround as in PR#7842.
            ## terms.formula may have flipped interactions
            nmfac0 <- sapply(strsplit(nmfac, ":", fixed=TRUE),
                             function(x) paste(sort(x), collapse=":"))
            nmadd0 <- sapply(strsplit(nmadd, ":", fixed=TRUE),
                             function(x) paste(sort(x), collapse=":"))
	    where <- match(nmfac0, nmadd0, 0L)
	    if(any(!where))
                stop(sprintf(ngettext(sum(where==0),
                                      "upper scope has term %s not included in model",
                                      "upper scope has terms %s not included in model"),
                             paste(sQuote(nmdrop[where==0]), collapse=", ")),
                     domain = NA)
	    nmadd <- nmadd[-where]
	    add <- add[, -where, drop = FALSE]
	}
	if(ncol(add) > 1) {             # check marginality:
	    keep <- rep.int(TRUE, ncol(add))
	    f <- crossprod(add > 0)
	    for(i in seq(keep)) keep[-i] <- keep[-i] & (f[i, -i] < f[i, i])
	    nmadd <- nmadd[keep]
	}
    }
    list(drop = nmdrop, add = nmadd)
}


## a slightly simplified version of stepAIC().
step <- function(object, scope, scale = 0,
		 direction = c("both", "backward", "forward"),
		 trace = 1, keep = NULL, steps = 1000, k = 2, ...)
{
    mydeviance <- function(x, ...)
    {
        dev <- deviance(x)
        if(!is.null(dev)) dev else extractAIC(x, k=0)[2L]
    }

    cut.string <- function(string)
    {
	if(length(string) > 1L)
	    string[-1L] <- paste0("\n", string[-1L])
	string
    }
    re.arrange <- function(keep)
    {
	namr <- names(k1 <- keep[[1L]])
	namc <- names(keep)
	nc <- length(keep)
	nr <- length(k1)
	array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, namc))
    }

    step.results <- function(models, fit, object, usingCp=FALSE)
    {
	change <- sapply(models, "[[", "change")
	rd <- sapply(models, "[[", "deviance")
        dd <- c(NA, abs(diff(rd)))
	rdf <- sapply(models, "[[", "df.resid")
	ddf <- c(NA, diff(rdf))
	AIC <- sapply(models, "[[", "AIC")
	heading <- c("Stepwise Model Path \nAnalysis of Deviance Table",
		     "\nInitial Model:", deparse(formula(object)),
		     "\nFinal Model:", deparse(formula(fit)),
		     "\n")
	aod <- data.frame(Step = I(change), Df = ddf, Deviance = dd,
                          "Resid. Df" = rdf, "Resid. Dev" = rd, AIC = AIC,
                          check.names = FALSE)
        if(usingCp) {
            cn <- colnames(aod)
            cn[cn == "AIC"] <- "Cp"
            colnames(aod) <- cn
        }
	attr(aod, "heading") <- heading
        ##stop gap attr(aod, "class") <- c("anova", "data.frame")
	fit$anova <- aod
	fit
    }

    Terms <- terms(object)
    object$call$formula <- object$formula <- Terms
    md <- missing(direction)
    direction <- match.arg(direction)
    backward <- direction == "both" | direction == "backward"
    forward  <- direction == "both" | direction == "forward"
    if(missing(scope)) {
	fdrop <- numeric()
        fadd <- attr(Terms, "factors")
        if(md) forward <- FALSE
    }
    else {
	if(is.list(scope)) {
	    fdrop <- if(!is.null(fdrop <- scope$lower))
		attr(terms(update.formula(object, fdrop)), "factors")
	    else numeric()
	    fadd <- if(!is.null(fadd <- scope$upper))
		attr(terms(update.formula(object, fadd)), "factors")
	}
        else {
	    fadd <- if(!is.null(fadd <- scope))
		attr(terms(update.formula(object, scope)), "factors")
	    fdrop <- numeric()
	}
    }
    models <- vector("list", steps)
    if(!is.null(keep)) keep.list <- vector("list", steps)
    n <- nobs(object, use.fallback = TRUE)  # might be NA
    fit <- object
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1L]
    bAIC <- bAIC[2L]
    if(is.na(bAIC))
        stop("AIC is not defined for this model, so 'step' cannot proceed")
    if(bAIC == -Inf)
        stop("AIC is -infinity for this model, so 'step' cannot proceed")
    nm <- 1
    ## Terms <- fit$terms
    if(trace) {
	cat("Start:  AIC=", format(round(bAIC, 2)), "\n",
	    cut.string(deparse(formula(fit))), "\n\n", sep = "")
        flush.console()
    }

    ## FIXME think about df.residual() here
    models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - edf,
			 change = "", AIC = bAIC)
    if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
    usingCp <- FALSE
    while(steps > 0) {
	steps <- steps - 1
	AIC <- bAIC
	ffac <- attr(Terms, "factors")
	scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
	aod <- NULL
	change <- NULL
	if(backward && length(scope$drop)) {
	    aod <- drop1(fit, scope$drop, scale = scale,
                         trace = trace, k = k, ...)
	    rn <- row.names(aod)
	    row.names(aod) <- c(rn[1L], paste("-", rn[-1L], sep=" "))
            ## drop zero df terms first: one at time since they
            ## may mask each other
	    if(any(aod$Df == 0, na.rm=TRUE)) {
		zdf <- aod$Df == 0 & !is.na(aod$Df)
		change <- rev(rownames(aod)[zdf])[1L]
	    }
	}
	if(is.null(change)) {
	    if(forward && length(scope$add)) {
		aodf <- add1(fit, scope$add, scale = scale,
                             trace = trace, k = k, ...)
		rn <- row.names(aodf)
		row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], sep=" "))
		aod <-
                    if(is.null(aod)) aodf
                    else rbind(aod, aodf[-1, , drop = FALSE])
	    }
	    attr(aod, "heading") <- NULL
	    ## need to remove any terms with zero df from consideration
	    nzdf <- if(!is.null(aod$Df))
		aod$Df != 0 | is.na(aod$Df)
	    aod <- aod[nzdf, ]
	    if(is.null(aod) || ncol(aod) == 0) break
	    nc <- match(c("Cp", "AIC"), names(aod))
	    nc <- nc[!is.na(nc)][1L]
	    o <- order(aod[, nc])
	    if(trace) print(aod[o, ])
	    if(o[1L] == 1) break
	    change <- rownames(aod)[o[1L]]
	}
	usingCp <- match("Cp", names(aod), 0L) > 0L
        ## may need to look for a `data' argument in parent
	fit <- update(fit, paste("~ .", change), evaluate = FALSE)
        fit <- eval.parent(fit)
        nnew <- nobs(fit, use.fallback = TRUE)
        if(all(is.finite(c(n, nnew))) && nnew != n)
            stop("number of rows in use has changed: remove missing values?")
        Terms <- terms(fit)
	bAIC <- extractAIC(fit, scale, k = k, ...)
	edf <- bAIC[1L]
	bAIC <- bAIC[2L]
	if(trace) {
	    cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n",
		cut.string(deparse(formula(fit))), "\n\n", sep = "")
            flush.console()
        }
        ## add a tolerance as dropping 0-df terms might increase AIC slightly
	if(bAIC >= AIC + 1e-7) break
	nm <- nm + 1
        ## FIXME: think about using df.residual() here.
	models[[nm]] <-
	    list(deviance = mydeviance(fit), df.resid = n - edf,
		 change = change, AIC = bAIC)
	if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
    }
    if(!is.null(keep)) fit$keep <- re.arrange(keep.list[seq(nm)])
    step.results(models = models[seq(nm)], fit, object, usingCp)
}

extractAIC <- function(fit, scale, k = 2, ...) UseMethod("extractAIC")

extractAIC.coxph <- function(fit, scale, k = 2, ...)
{
    ## fit$coefficients gives NAs for aliased terms
    edf <- sum(!is.na(fit$coefficients))
    ## seems that coxph sometimes gives one and sometimes gives two values
    ## for loglik: the latter is what is documented.
    loglik <- fit$loglik[length(fit$loglik)]
    c(edf, -2 * loglik + k * edf)
}

extractAIC.survreg <- function(fit, scale, k = 2, ...)
{
    edf <- sum(fit$df)
    c(edf, -2 * fit$loglik[2L] + k * edf)
}

extractAIC.glm <- function(fit, scale = 0, k = 2, ...)
{
    n <- length(fit$residuals)
    edf <- n  - fit$df.residual # assumes dispersion is known
    aic <- fit$aic
    c(edf, aic + (k-2) * edf)
}

extractAIC.lm <- function(fit, scale = 0, k = 2, ...)
{
    n <- length(fit$residuals)
    edf <- n  - fit$df.residual # maybe -1 if sigma^2 is estimated
    RSS <- deviance.lm(fit)
    dev <- if(scale > 0) RSS/scale - n else n * log(RSS/n)
    c(edf, dev + k * edf)
}
extractAIC.aov <- extractAIC.lm

extractAIC.negbin <- function(fit, scale, k = 2, ...)
{
    n <- length(fit$residuals)
    edf <- n - fit$df.residual # may -1 if theta is estimated
    c(edf, -fit$twologlik + k * edf)
}
