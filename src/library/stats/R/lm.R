#  File src/library/stats/R/lm.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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


lm <- function (formula, data, subset, weights, na.action,
		method = "qr", model = TRUE, x = FALSE, y = FALSE,
		qr = TRUE, singular.ok = TRUE, contrasts = NULL,
		offset, ...)
{
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"),
               names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (method == "model.frame")
	return(mf)
    else if (method != "qr")
	warning(gettextf("method = '%s' is not supported. Using 'qr'", method),
                domain = NA)
    mt <- attr(mf, "terms") # allow model.frame to update it
    y <- model.response(mf, "numeric")
    ## avoid any problems with 1D or nx1 arrays by as.vector.
    w <- as.vector(model.weights(mf))
    if(!is.null(w) && !is.numeric(w))
        stop("'weights' must be a numeric vector")
    offset <- as.vector(model.offset(mf))
    if(!is.null(offset)) {
        if(length(offset) != NROW(y))
            stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
                          length(offset), NROW(y)), domain = NA)
    }

    if (is.empty.model(mt)) {
	x <- NULL
	z <- list(coefficients = if (is.matrix(y))
                  matrix(,0,3) else numeric(), residuals = y,
		  fitted.values = 0 * y, weights = w, rank = 0L,
		  df.residual = if(!is.null(w)) sum(w != 0) else
                  if (is.matrix(y)) nrow(y) else length(y))
        if(!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    }
    else {
	x <- model.matrix(mt, mf, contrasts)
	z <- if(is.null(w)) lm.fit(x, y, offset = offset,
                                   singular.ok=singular.ok, ...)
	else lm.wfit(x, y, w, offset = offset, singular.ok=singular.ok, ...)
    }
    class(z) <- c(if(is.matrix(y)) "mlm", "lm")
    z$na.action <- attr(mf, "na.action")
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model)
	z$model <- mf
    if (ret.x)
	z$x <- x
    if (ret.y)
	z$y <- y
    if (!qr) z$qr <- NULL
    z
}

## lm.fit() and lm.wfit() have *MUCH* in common  [say ``code re-use !'']
lm.fit <- function (x, y, offset = NULL, method = "qr", tol = 1e-07,
                    singular.ok = TRUE, ...)
{
    if (is.null(n <- nrow(x))) stop("'x' must be a matrix")
    if(n == 0L) stop("0 (non-NA) cases")
    p <- ncol(x)
    if (p == 0L) {
        ## oops, null model
        return(list(coefficients = numeric(), residuals = y,
                    fitted.values = 0 * y, rank = 0,
                    df.residual = length(y)))
    }
    ny <- NCOL(y)
    ## treat one-col matrix as vector
    if(is.matrix(y) && ny == 1)
        y <- drop(y)
    if(!is.null(offset))
        y <- y - offset
    if (NROW(y) != n)
	stop("incompatible dimensions")
    if(method != "qr")
	warning(gettextf("method = '%s' is not supported. Using 'qr'", method),
                domain = NA)
    chkDots(...)
    z <- .Call(C_Cdqrls, x, y, tol, FALSE)
    if(!singular.ok && z$rank < p) stop("singular fit encountered")
    coef <- z$coefficients
    pivot <- z$pivot
    ## careful here: the rank might be 0
    r1 <- seq_len(z$rank)
    dn <- colnames(x); if(is.null(dn)) dn <- paste0("x", 1L:p)
    nmeffects <- c(dn[pivot[r1]], rep.int("", n - z$rank))
    r2 <- if(z$rank < p) (z$rank+1L):p else integer()
    if (is.matrix(y)) {
	coef[r2, ] <- NA
	if(z$pivoted) coef[pivot, ] <- coef
	dimnames(coef) <- list(dn, colnames(y))
	dimnames(z$effects) <- list(nmeffects, colnames(y))
    } else {
	coef[r2] <- NA
        ## avoid copy
	if(z$pivoted) coef[pivot] <- coef
	names(coef) <- dn
	names(z$effects) <- nmeffects
    }
    z$coefficients <- coef
    r1 <- y - z$residuals ; if(!is.null(offset)) r1 <- r1 + offset
    ## avoid unnecessary copy
    if(z$pivoted) colnames(z$qr) <- colnames(x)[z$pivot]
    qr <- z[c("qr", "qraux", "pivot", "tol", "rank")]
    c(z[c("coefficients", "residuals", "effects", "rank")],
      list(fitted.values = r1, assign = attr(x, "assign"),
	   qr = structure(qr, class="qr"),
	   df.residual = n - z$rank))
}

.lm.fit <- function(x, y, tol = 1e-07) .Call(C_Cdqrls, x, y, tol, check=TRUE)

lm.wfit <- function (x, y, w, offset = NULL, method = "qr", tol = 1e-7,
                     singular.ok = TRUE, ...)
{
    if(is.null(n <- nrow(x))) stop("'x' must be a matrix")
    if(n == 0) stop("0 (non-NA) cases")
    ny <- NCOL(y)
    ## treat one-col matrix as vector
    if(is.matrix(y) && ny == 1L)
        y <- drop(y)
    if(!is.null(offset))
        y <- y - offset
    if (NROW(y) != n | length(w) != n)
	stop("incompatible dimensions")
    if (any(w < 0 | is.na(w)))
	stop("missing or negative weights not allowed")
    if(method != "qr")
	warning(gettextf("method = '%s' is not supported. Using 'qr'", method),
                domain = NA)
    chkDots(...)
    x.asgn <- attr(x, "assign")# save
    zero.weights <- any(w == 0)
    if (zero.weights) {
	save.r <- y
	save.f <- y
	save.w <- w
	ok <- w != 0
	nok <- !ok
	w <- w[ok]
	x0 <- x[!ok, , drop = FALSE]
	x <- x[ok,  , drop = FALSE]
	n <- nrow(x)
	y0 <- if (ny > 1L) y[!ok, , drop = FALSE] else y[!ok]
	y  <- if (ny > 1L) y[ ok, , drop = FALSE] else y[ok]
    }
    p <- ncol(x)
    if (p == 0) {
        ## oops, null model
        return(list(coefficients = numeric(), residuals = y,
                    fitted.values = 0 * y, weights = w, rank = 0L,
                    df.residual = length(y)))
    }
    if (n == 0) { # all cases have weight zero
        return(list(coefficients = rep(NA_real_, p), residuals = y,
                    fitted.values = 0 * y, weights = w, rank = 0L,
                    df.residual = 0L))
    }
    wts <- sqrt(w)
    z <- .Call(C_Cdqrls, x * wts, y * wts, tol, FALSE)
    if(!singular.ok && z$rank < p) stop("singular fit encountered")
    coef <- z$coefficients
    pivot <- z$pivot
    r1 <- seq_len(z$rank)
    dn <- colnames(x); if(is.null(dn)) dn <- paste0("x", 1L:p)
    nmeffects <- c(dn[pivot[r1]], rep.int("", n - z$rank))
    r2 <- if(z$rank < p) (z$rank+1L):p else integer()
    if (is.matrix(y)) {
	coef[r2, ] <- NA
	if(z$pivoted) coef[pivot, ] <- coef
	dimnames(coef) <- list(dn, colnames(y))
	dimnames(z$effects) <- list(nmeffects,colnames(y))
    } else {
	coef[r2] <- NA
	if(z$pivoted) coef[pivot] <- coef
	names(coef) <- dn
	names(z$effects) <- nmeffects
    }
    z$coefficients <- coef
    z$residuals <- z$residuals/wts
    z$fitted.values <- y - z$residuals
    z$weights <- w
    if (zero.weights) {
	coef[is.na(coef)] <- 0
	f0 <- x0 %*% coef
	if (ny > 1) {
	    save.r[ok, ] <- z$residuals
	    save.r[nok, ] <- y0 - f0
	    save.f[ok, ] <- z$fitted.values
	    save.f[nok, ] <- f0
	}
	else {
	    save.r[ok] <- z$residuals
	    save.r[nok] <- y0 - f0
	    save.f[ok] <- z$fitted.values
	    save.f[nok] <- f0
	}
	z$residuals <- save.r
	z$fitted.values <- save.f
	z$weights <- save.w
    }
    if(!is.null(offset))
        z$fitted.values <- z$fitted.values + offset
    if(z$pivoted) colnames(z$qr) <- colnames(x)[z$pivot]
    qr <- z[c("qr", "qraux", "pivot", "tol", "rank")]
    c(z[c("coefficients", "residuals", "fitted.values", "effects",
	  "weights", "rank")],
      list(assign = x.asgn,
	   qr = structure(qr, class="qr"),
	   df.residual = n - z$rank))
}

print.lm <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    cat("\nCall:\n",
	paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
    if(length(coef(x))) {
        cat("Coefficients:\n")
        print.default(format(coef(x), digits = digits),
                      print.gap = 2L, quote = FALSE)
    } else cat("No coefficients\n")
    cat("\n")
    invisible(x)
}

summary.lm <- function (object, correlation = FALSE, symbolic.cor = FALSE, ...)
{
    z <- object
    p <- z$rank
    rdf <- z$df.residual
    if (p == 0) {
        r <- z$residuals
        n <- length(r)
        w <- z$weights
        if (is.null(w)) {
            rss <- sum(r^2)
        } else {
            rss <- sum(w * r^2)
            r <- sqrt(w) * r
        }
        resvar <- rss/rdf
        ans <- z[c("call", "terms", if(!is.null(z$weights)) "weights")]
        class(ans) <- "summary.lm"
        ans$aliased <- is.na(coef(object))  # used in print method
        ans$residuals <- r
        ans$df <- c(0L, n, length(ans$aliased))
        ans$coefficients <- matrix(NA, 0L, 4L)
        dimnames(ans$coefficients) <-
            list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
        ans$sigma <- sqrt(resvar)
        ans$r.squared <- ans$adj.r.squared <- 0
        return(ans)
    }
    if (is.null(z$terms))
	stop("invalid 'lm' object:  no 'terms' component")
    if(!inherits(object, "lm"))
	warning("calling summary.lm(<fake-lm-object>) ...")
    Qr <- qr.lm(object)
    n <- NROW(Qr$qr)
    if(is.na(z$df.residual) || n - p != z$df.residual)
        warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
    ## do not want missing values substituted here
    r <- z$residuals
    f <- z$fitted.values
    w <- z$weights
    if (is.null(w)) {
        mss <- if (attr(z$terms, "intercept"))
            sum((f - mean(f))^2) else sum(f^2)
        rss <- sum(r^2)
    } else {
        mss <- if (attr(z$terms, "intercept")) {
            m <- sum(w * f /sum(w))
            sum(w * (f - m)^2)
        } else sum(w * f^2)
        rss <- sum(w * r^2)
        r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    ## see thread at https://stat.ethz.ch/pipermail/r-help/2014-March/367585.html
    if (is.finite(resvar) &&
        resvar < (mean(f)^2 + var(f)) * 1e-30)  # a few times .Machine$double.eps^2
        warning("essentially perfect fit: summary may be unreliable")
    p1 <- 1L:p
    R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    se <- sqrt(diag(R) * resvar)
    est <- z$coefficients[Qr$pivot[p1]]
    tval <- est/se
    ans <- z[c("call", "terms", if(!is.null(z$weights)) "weights")]
    ans$residuals <- r
    ans$coefficients <-
	cbind(est, se, tval, 2*pt(abs(tval), rdf, lower.tail = FALSE))
    dimnames(ans$coefficients) <-
	list(names(z$coefficients)[Qr$pivot[p1]],
	     c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    ans$aliased <- is.na(coef(object))  # used in print method
    ans$sigma <- sqrt(resvar)
    ans$df <- c(p, rdf, NCOL(Qr$qr))
    if (p != attr(z$terms, "intercept")) {
	df.int <- if (attr(z$terms, "intercept")) 1L else 0L
	ans$r.squared <- mss/(mss + rss)
	ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - df.int)/rdf)
	ans$fstatistic <- c(value = (mss/(p - df.int))/resvar,
			    numdf = p - df.int, dendf = rdf)
    } else ans$r.squared <- ans$adj.r.squared <- 0
    ans$cov.unscaled <- R
    dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1,1)]
    if (correlation) {
	ans$correlation <- (R * resvar)/outer(se, se)
	dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
        ans$symbolic.cor <- symbolic.cor
    }
    if(!is.null(z$na.action)) ans$na.action <- z$na.action
    class(ans) <- "summary.lm"
    ans
}

print.summary.lm <-
    function (x, digits = max(3L, getOption("digits") - 3L),
              symbolic.cor = x$symbolic.cor,
	      signif.stars = getOption("show.signif.stars"),	...)
{
    cat("\nCall:\n", # S has ' ' instead of '\n'
	paste(deparse(x$call), sep="\n", collapse = "\n"), "\n\n", sep = "")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    cat(if(!is.null(x$weights) && diff(range(x$weights))) "Weighted ",
        "Residuals:\n", sep = "")
    if (rdf > 5L) {
	nam <- c("Min", "1Q", "Median", "3Q", "Max")
	rq <- if (length(dim(resid)) == 2L)
	    structure(apply(t(resid), 1L, quantile),
		      dimnames = list(nam, dimnames(resid)[[2L]]))
	else  {
            zz <- zapsmall(quantile(resid), digits + 1L)
            structure(zz, names = nam)
        }
	print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
	print(resid, digits = digits, ...)
    } else { # rdf == 0 : perfect fit!
	cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
        cat("\n")
    }
    if (length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    } else {
        if (nsingular <- df[3L] - df[1L])
            cat("\nCoefficients: (", nsingular,
                " not defined because of singularities)\n", sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if(!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4, dimnames=list(cn, colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }

        printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                     na.print = "NA", ...)
    }
    ##
    cat("\nResidual standard error:",
	format(signif(x$sigma, digits)), "on", rdf, "degrees of freedom")
    cat("\n")
    if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
    if (!is.null(x$fstatistic)) {
	cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
	cat(",\tAdjusted R-squared: ",formatC(x$adj.r.squared, digits = digits),
	    "\nF-statistic:", formatC(x$fstatistic[1L], digits = digits),
	    "on", x$fstatistic[2L], "and",
	    x$fstatistic[3L], "DF,  p-value:",
	    format.pval(pf(x$fstatistic[1L], x$fstatistic[2L],
                           x$fstatistic[3L], lower.tail = FALSE),
                        digits = digits))
        cat("\n")
    }
    correl <- x$correlation
    if (!is.null(correl)) {
	p <- NCOL(correl)
	if (p > 1L) {
	    cat("\nCorrelation of Coefficients:\n")
	    if(is.logical(symbolic.cor) && symbolic.cor) {# NULL < 1.7.0 objects
		print(symnum(correl, abbr.colnames = NULL))
	    } else {
                correl <- format(round(correl, 2), nsmall = 2, digits = digits)
                correl[!lower.tri(correl)] <- ""
                print(correl[-1, -p, drop=FALSE], quote = FALSE)
            }
	}
    }
    cat("\n")#- not in S
    invisible(x)
}

residuals.lm <-
    function(object,
             type = c("working","response", "deviance","pearson", "partial"),
             ...)
{
    type <- match.arg(type)
    r <- object$residuals
    res <- switch(type,
                  working =, response = r,
                  deviance=, pearson =
                  if(is.null(object$weights)) r else r * sqrt(object$weights),
                  partial = r
           )
    res <- naresid(object$na.action, res)
    if (type=="partial") ## predict already does naresid
      res <- res + predict(object,type="terms")
    res
}

## using qr(<lm>)  as interface to  <lm>$qr :
qr.lm <- function(x, ...) {
      if(is.null(r <- x$qr))
        stop("lm object does not have a proper 'qr' component.
 Rank zero or should not have used lm(.., qr=FALSE).")
      r
}

## The lm method includes objects of class "glm"
simulate.lm <- function(object, nsim = 1, seed = NULL, ...)
{
    if(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        runif(1)                     # initialize the RNG if necessary
    if(is.null(seed))
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
	set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
    ftd <- fitted(object)             # == napredict(*, object$fitted)
    nm <- names(ftd)
    n <- length(ftd)
    ntot <- n * nsim
    fam <- if(inherits(object, "glm")) object$family$family else "gaussian"
    val <- switch(fam,
                  "gaussian" = {
                      vars <- deviance(object)/ df.residual(object)
                      if (!is.null(object$weights)) vars <- vars/object$weights
                      ftd + rnorm(ntot, sd = sqrt(vars))
                  },
                  if(!is.null(object$family$simulate))
                      object$family$simulate(object, nsim)
                  else stop(gettextf("family '%s' not implemented", fam),
                            domain = NA)
                  )

    if(!is.list(val)) {
        dim(val) <- c(n, nsim)
        val <- as.data.frame(val)
    } else
        class(val) <- "data.frame"
    names(val) <- paste("sim", seq_len(nsim), sep="_")
    if (!is.null(nm)) row.names(val) <- nm
    attr(val, "seed") <- RNGstate
    val
}

deviance.lm <- function(object, ...)
    sum(weighted.residuals(object)^2, na.rm=TRUE)

formula.lm <- function(x, ...)
{
    form <- x$formula
    if( !is.null(form) ) {
        form <- formula(x$terms) # has . expanded
        environment(form) <- environment(x$formula)
        form
    } else formula(x$terms)
}

family.lm <- function(object, ...) { gaussian() }

model.frame.lm <- function(formula, ...)
{
    dots <- list(...)
    nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]
    if (length(nargs) || is.null(formula$model)) {
        ## mimic lm(method = "model.frame")
        fcall <- formula$call
        m <- match(c("formula", "data", "subset", "weights", "na.action",
                     "offset"), names(fcall), 0L)
        fcall <- fcall[c(1L, m)]
        fcall$drop.unused.levels <- TRUE
        fcall[[1L]] <- quote(stats::model.frame)
        fcall$xlev <- formula$xlevels
        ## We want to copy over attributes here, especially predvars.
        fcall$formula <- terms(formula)
        fcall[names(nargs)] <- nargs
        env <- environment(formula$terms)
	if (is.null(env)) env <- parent.frame()
        eval(fcall, env) # 2-arg form as env is an environment
    }
    else formula$model
}

variable.names.lm <- function(object, full = FALSE, ...)
{
    if(full) dimnames(qr.lm(object)$qr)[[2L]]
    else if(object$rank) dimnames(qr.lm(object)$qr)[[2L]][seq_len(object$rank)]
    else character()
}

case.names.lm <- function(object, full = FALSE, ...)
{
    w <- weights(object)
    dn <- names(residuals(object))
    if(full || is.null(w)) dn else dn[w!=0]
}

anova.lm <- function(object, ...)
{
    ## Do not copy this: anova.lmlist is not an exported object.
    ## See anova.glm for further comments.
    if(length(list(object, ...)) > 1L) return(anova.lmlist(object, ...))

    if(!inherits(object, "lm"))
	warning("calling anova.lm(<fake-lm-object>) ...")
    w <- object$weights
    ssr <- sum(if(is.null(w)) object$residuals^2 else w*object$residuals^2)
    mss <- sum(if(is.null(w)) object$fitted.values^2 else w*object$fitted.values^2)
    if(ssr < 1e-10*mss)
        warning("ANOVA F-tests on an essentially perfect fit are unreliable")
    dfr <- df.residual(object)
    p <- object$rank
    if(p > 0L) {
        p1 <- 1L:p
        comp <- object$effects[p1]
        asgn <- object$assign[qr.lm(object)$pivot][p1]
        nmeffects <- c("(Intercept)", attr(object$terms, "term.labels"))
        tlabels <- nmeffects[1 + unique(asgn)]
        ss <- c(unlist(lapply(split(comp^2,asgn), sum)), ssr)
        df <- c(lengths(split(asgn,  asgn)), dfr)
    } else {
        ss <- ssr
        df <- dfr
        tlabels <- character()
    }
    ms <- ss/df
    f <- ms/(ssr/dfr)
    P <- pf(f, df, dfr, lower.tail = FALSE)
    table <- data.frame(df, ss, ms, f, P)
    table[length(P), 4:5] <- NA
    dimnames(table) <- list(c(tlabels, "Residuals"),
                            c("Df","Sum Sq", "Mean Sq", "F value", "Pr(>F)"))
    if(attr(object$terms,"intercept")) table <- table[-1, ]
    structure(table, heading = c("Analysis of Variance Table\n",
		     paste("Response:", deparse(formula(object)[[2L]]))),
	      class = c("anova", "data.frame"))# was "tabular"
}

anova.lmlist <- function (object, ..., scale = 0, test = "F")
{
    objects <- list(object, ...)
    responses <- as.character(lapply(objects,
				     function(x) deparse(x$terms[[2L]])))
    sameresp <- responses == responses[1L]
    if (!all(sameresp)) {
	objects <- objects[sameresp]
        warning(gettextf("models with response %s removed because response differs from model 1",
                         sQuote(deparse(responses[!sameresp]))),
                domain = NA)
    }

    ns <- sapply(objects, function(x) length(x$residuals))
    if(any(ns != ns[1L]))
        stop("models were not all fitted to the same size of dataset")

    ## calculate the number of models
    nmodels <- length(objects)
    if (nmodels == 1)
	return(anova.lm(object))

    ## extract statistics

    resdf  <- as.numeric(lapply(objects, df.residual))
    resdev <- as.numeric(lapply(objects, deviance))

    ## construct table and title

    table <- data.frame(resdf, resdev, c(NA, -diff(resdf)),
                        c(NA, -diff(resdev)) )
    variables <- lapply(objects, function(x)
                        paste(deparse(formula(x)), collapse="\n") )
    dimnames(table) <- list(1L:nmodels,
                            c("Res.Df", "RSS", "Df", "Sum of Sq"))

    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1L:nmodels),": ",
		     variables, sep = "", collapse = "\n")

    ## calculate test statistic if needed

    if(!is.null(test)) {
	bigmodel <- order(resdf)[1L]
        scale <- if(scale > 0) scale else resdev[bigmodel]/resdf[bigmodel]
	table <- stat.anova(table = table, test = test,
			    scale = scale,
                            df.scale = resdf[bigmodel],
			    n = length(objects[[bigmodel]]$residuals))
    }
    structure(table, heading = c(title, topnote),
              class = c("anova", "data.frame"))
}

## code originally from John Maindonald 26Jul2000
predict.lm <-
    function(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
	     interval = c("none", "confidence", "prediction"),
	     level = .95,  type = c("response", "terms"),
	     terms = NULL, na.action = na.pass, pred.var = res.var/weights,
             weights = 1, ...)
{
    tt <- terms(object)
    if(!inherits(object, "lm"))
	warning("calling predict.lm(<fake-lm-object>) ...")
    if(missing(newdata) || is.null(newdata)) {
	mm <- X <- model.matrix(object)
	mmDone <- TRUE
	offset <- object$offset
    }
    else {
        Terms <- delete.response(tt)
        m <- model.frame(Terms, newdata, na.action = na.action,
                         xlev = object$xlevels)
        if(!is.null(cl <- attr(Terms, "dataClasses"))) .checkMFClasses(cl, m)
        X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
        offset <- rep(0, nrow(X))
        if (!is.null(off.num <- attr(tt, "offset")))
            for(i in off.num)
                offset <- offset + eval(attr(tt, "variables")[[i+1]], newdata)
	if (!is.null(object$call$offset))
	    offset <- offset + eval(object$call$offset, newdata)
	mmDone <- FALSE
    }
    n <- length(object$residuals) # NROW(qr(object)$qr)
    p <- object$rank
    p1 <- seq_len(p)
    piv <- if(p) qr.lm(object)$pivot[p1]
    if(p < ncol(X) && !(missing(newdata) || is.null(newdata)))
	warning("prediction from a rank-deficient fit may be misleading")
### NB: Q[p1,] %*% X[,piv] = R[p1,p1]
    beta <- object$coefficients
    predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
    if (!is.null(offset))
	predictor <- predictor + offset

    interval <- match.arg(interval)
    if (interval == "prediction") {
        if (missing(newdata))
            warning("predictions on current data refer to _future_ responses\n")
        if (missing(newdata) && missing(weights)) {
            w <-  weights.default(object)
            if (!is.null(w)) {
                weights <- w
                warning("assuming prediction variance inversely proportional to weights used for fitting\n")
            }
        }
        if (!missing(newdata) && missing(weights) && !is.null(object$weights) && missing(pred.var))
            warning("Assuming constant prediction variance even though model fit is weighted\n")
        if (inherits(weights, "formula")){
            if (length(weights) != 2L)
                stop("'weights' as formula should be one-sided")
            d <- if(missing(newdata) || is.null(newdata))
                model.frame(object)
            else
                newdata
            weights <- eval(weights[[2L]], d, environment(weights))
        }
    }

    type <- match.arg(type)
    if(se.fit || interval != "none") {
        ## w is needed for interval = "confidence"
        w <- object$weights
	res.var <-
	    if (is.null(scale)) {
		r <- object$residuals
		rss <- sum(if(is.null(w)) r^2 else r^2 * w)
		df <- object$df.residual
		rss/df
	    } else scale^2
	if(type != "terms") {
            if(p > 0) {
                XRinv <-
                    if(missing(newdata) && is.null(w))
                        qr.Q(qr.lm(object))[, p1, drop = FALSE]
                    else
                        X[, piv] %*% qr.solve(qr.R(qr.lm(object))[p1, p1])
#	NB:
#	 qr.Q(qr.lm(object))[, p1, drop = FALSE] / sqrt(w)
#	looks faster than the above, but it's slower, and doesn't handle zero
#	weights properly
#
                ip <- drop(XRinv^2 %*% rep(res.var, p))
            } else ip <- rep(0, n)
	}
    }

    if (type == "terms") { ## type == "terms" ------------
	if(!mmDone) {
            mm <- model.matrix(object)
            mmDone <- TRUE
        }
	aa <- attr(mm, "assign")
	ll <- attr(tt, "term.labels")
	hasintercept <- attr(tt, "intercept") > 0L
	if (hasintercept) ll <- c("(Intercept)", ll)
	aaa <- factor(aa, labels = ll)
	asgn <- split(order(aa), aaa)
	if (hasintercept) {
	    asgn$"(Intercept)" <- NULL
	    avx <- colMeans(mm)
	    termsconst <- sum(avx[piv] * beta[piv])
	}
	nterms <- length(asgn)
        if(nterms > 0) {
            predictor <- matrix(ncol = nterms, nrow = NROW(X))
            dimnames(predictor) <- list(rownames(X), names(asgn))

            if (se.fit || interval != "none") {
                ip <- matrix(ncol = nterms, nrow = NROW(X))
                dimnames(ip) <- list(rownames(X), names(asgn))
                Rinv <- qr.solve(qr.R(qr.lm(object))[p1, p1])
            }
            if(hasintercept)
                X <- sweep(X, 2L, avx, check.margin=FALSE)
            unpiv <- rep.int(0L, NCOL(X))
            unpiv[piv] <- p1
            ## Predicted values will be set to 0 for any term that
            ## corresponds to columns of the X-matrix that are
            ## completely aliased with earlier columns.
            for (i in seq.int(1L, nterms, length.out = nterms)) {
                iipiv <- asgn[[i]]      # Columns of X, ith term
                ii <- unpiv[iipiv]      # Corresponding rows of Rinv
                iipiv[ii == 0L] <- 0L
                predictor[, i] <-
                    if(any(iipiv > 0L)) X[, iipiv, drop = FALSE] %*% beta[iipiv]
                    else 0
                if (se.fit || interval != "none")
                    ip[, i] <-
                        if(any(iipiv > 0L))
                            as.matrix(X[, iipiv, drop = FALSE] %*%
                                      Rinv[ii, , drop = FALSE])^2 %*% rep.int(res.var, p)
                        else 0
            }
            if (!is.null(terms)) {
                predictor <- predictor[, terms, drop = FALSE]
                if (se.fit)
                    ip <- ip[, terms, drop = FALSE]
            }
        } else {                        # no terms
            predictor <- ip <- matrix(0, n, 0L)
        }
	attr(predictor, 'constant') <- if (hasintercept) termsconst else 0
    }

### Now construct elements of the list that will be returned

    if(interval != "none") {
	tfrac <- qt((1 - level)/2, df)
	hwid <- tfrac * switch(interval,
			       confidence = sqrt(ip),
			       prediction = sqrt(ip+pred.var)
			       )
	if(type != "terms") {
	    predictor <- cbind(predictor, predictor + hwid %o% c(1, -1))
	    colnames(predictor) <- c("fit", "lwr", "upr")
	} else {
            if (!is.null(terms)) hwid <- hwid[, terms, drop = FALSE]
	    lwr <- predictor + hwid
	    upr <- predictor - hwid
	}
    }
    if(se.fit || interval != "none") {
        se <- sqrt(ip)
	if(type == "terms" && !is.null(terms) && !se.fit)
	    se <- se[, terms, drop = FALSE]
    }
    if(missing(newdata) && !is.null(na.act <- object$na.action)) {
	predictor <- napredict(na.act, predictor)
	if(se.fit) se <- napredict(na.act, se)
    }
    if(type == "terms" && interval != "none") {
	if(missing(newdata) && !is.null(na.act)) {
	    lwr <- napredict(na.act, lwr)
	    upr <- napredict(na.act, upr)
	}
	list(fit = predictor, se.fit = se, lwr = lwr, upr = upr,
	     df = df, residual.scale = sqrt(res.var))
    } else if (se.fit)
	list(fit = predictor, se.fit = se,
	     df = df, residual.scale = sqrt(res.var))
    else predictor
}

effects.lm <- function(object, set.sign = FALSE, ...)
{
    eff <- object$effects
    if(is.null(eff)) stop("'object' has no 'effects' component")
    if(set.sign) {
	dd <- coef(object)
	if(is.matrix(eff)) {
	    r <- 1L:dim(dd)[1L]
	    eff[r,  ] <- sign(dd) * abs(eff[r,	])
	} else {
	    r <- seq_along(dd)
	    eff[r] <- sign(dd) * abs(eff[r])
	}
    }
    structure(eff, assign = object$assign, class = "coef")
}

## plot.lm --> now in ./plot.lm.R

model.matrix.lm <- function(object, ...)
{
    if(n_match <- match("x", names(object), 0L)) object[[n_match]]
    else {
        data <- model.frame(object, xlev = object$xlevels, ...)
        NextMethod("model.matrix", data = data,
                   contrasts.arg = object$contrasts)
    }
}

##---> SEE ./mlm.R  for more methods, etc. !!
predict.mlm <-
    function(object, newdata, se.fit = FALSE, na.action = na.pass, ...)
{
    if(missing(newdata)) return(object$fitted.values)
    if(se.fit)
	stop("the 'se.fit' argument is not yet implemented for \"mlm\" objects")
    if(missing(newdata)) {
        X <- model.matrix(object)
        offset <- object$offset
    }
    else {
        tt <- terms(object)
        Terms <- delete.response(tt)
        m <- model.frame(Terms, newdata, na.action = na.action,
                         xlev = object$xlevels)
        if(!is.null(cl <- attr(Terms, "dataClasses"))) .checkMFClasses(cl, m)
        X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
	offset <- if (!is.null(off.num <- attr(tt, "offset")))
	    eval(attr(tt, "variables")[[off.num+1]], newdata)
	else if (!is.null(object$offset))
	    eval(object$call$offset, newdata)
    }
    piv <- qr.lm(object)$pivot[seq(object$rank)]
    pred <- X[, piv, drop = FALSE] %*% object$coefficients[piv,]
    if ( !is.null(offset) ) pred <- pred + offset
    if(inherits(object, "mlm")) pred else pred[, 1L]
}

## from base/R/labels.R
labels.lm <- function(object, ...)
{
    tl <- attr(object$terms, "term.labels")
    asgn <- object$assign[qr.lm(object)$pivot[1L:object$rank]]
    tl[unique(asgn)]
}
