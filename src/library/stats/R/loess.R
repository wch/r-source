#  File src/library/stats/R/loess.R
#  Part of the R package, http://www.R-project.org
#
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

loess <-
function(formula, data, weights, subset, na.action, model = FALSE,
	 span = 0.75, enp.target, degree = 2L, parametric = FALSE,
	 drop.square = FALSE, normalize = TRUE,
	 family = c("gaussian", "symmetric"),
	 method = c("loess", "model.frame"),
	 control = loess.control(...), ...)
{
    family <- match.arg(family)
    method <- match.arg(method)
    mf <- match.call(expand.dots=FALSE)
    mf$model <- mf$span <- mf$enp.target <- mf$degree <-
	mf$parametric <- mf$drop.square <- mf$normalize <- mf$family <-
	    mf$method <- mf$control <- mf$... <- NULL
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (match.arg(method) == "model.frame") return(mf)
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- model.weights(mf)
    if(is.null(w)) w <- rep(1, length(y))
    nmx <- as.character(attr(mt, "variables"))[-(1L:2)]
    x <- mf[, nmx, drop=FALSE]
    if(any(sapply(x, is.factor))) stop("predictors must all be numeric")
    x <- as.matrix(x)
    D <- ncol(x)
    nmx <- setNames(nm = colnames(x))
    drop.square <- match(nmx, nmx[drop.square], 0L) > 0L
    parametric <- match(nmx, nmx[parametric], 0L) > 0L
    if(!match(degree, 0L:2L, 0L)) stop("'degree' must be 0, 1 or 2")
    iterations <- if(family == "gaussian") 1 else control$iterations
    if(!missing(enp.target))
	if(!missing(span))
	    warning("both 'span' and 'enp.target' specified: 'span' will be used")
	else {				# White book p.321
	    tau <- switch(degree+1L, 1, D+1, (D+1)*(D+2)/2) - sum(drop.square)
	    span <- 1.2 * tau/enp.target
	}
    ## Let's add sanity checks on control
    if(!is.list(control) || !is.character(control$surface) ||
       !is.character(control$statistics) || !is.character(control$trace.hat) ||
       !is.numeric(control$cell) || !is.numeric(iterations))
        stop("invalid 'control' argument")
    fit <- simpleLoess(y, x, w, span, degree, parametric, drop.square,
		       normalize, control$statistics, control$surface,
		       control$cell, iterations, control$trace.hat)
    fit$call <- match.call()
    fit$terms <- mt
    fit$xnames <- nmx
    fit$x <- x
    fit$y <- y
    fit$weights <- w
    if(model) fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    fit
}

loess.control <-
  function(surface = c("interpolate", "direct"),
	   statistics = c("approximate", "exact"),
	   trace.hat = c("exact", "approximate"),
	   cell = 0.2, iterations = 4L, ...)
{
    list(surface=match.arg(surface),
	 statistics=match.arg(statistics),
	 trace.hat=match.arg(trace.hat),
	 cell=cell, iterations=iterations)
}


simpleLoess <-
  function(y, x, weights, span = 0.75, degree = 2L, parametric = FALSE,
	   drop.square = FALSE, normalize = TRUE,
	   statistics = "approximate", surface = "interpolate",
	   cell = 0.2, iterations = 1L, trace.hat = "exact")
{
    ## loess_ translated to R.

    D <- as.integer(NCOL(x))
    if (is.na(D)) stop("invalid NCOL(X)")
    if(D > 4) stop("only 1-4 predictors are allowed")
    N <- as.integer(NROW(x))
    if (is.na(N)) stop("invalid NCOL(X)")
    if(!N || !D)	stop("invalid 'x'")
    if(!length(y))	stop("invalid 'y'")
    x <- as.matrix(x)
    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    storage.mode(weights) <- "double"
    max.kd <-  max(N, 200)
    robust <- rep(1, N)
    divisor <- rep(1, D)
    if(normalize && D > 1L) {
	trim <- ceiling(0.1 * N)
	divisor <-
	    sqrt(apply(apply(x, 2L, sort)[seq(trim+1, N-trim), , drop = FALSE],
		       2L, var))
	x <- x/rep(divisor, rep(N, D))
    }
    sum.drop.sqr <- sum(drop.square)
    sum.parametric <- sum(parametric)
    nonparametric <- sum(!parametric)
    order.parametric <- order(parametric)
    x <- x[, order.parametric]
    order.drop.sqr <- (2L - drop.square)[order.parametric]
    if(degree == 1L && sum.drop.sqr)
	stop("specified the square of a factor predictor to be dropped when degree = 1")
    if(D == 1L && sum.drop.sqr)
	stop("specified the square of a predictor to be dropped with only one numeric predictor")
    if(sum.parametric == D) stop("specified parametric for all predictors")

    if(iterations)
    for(j in seq_len(iterations)) {
	robust <- weights * robust
	if(j > 1) statistics <- "none"
	else if(surface == "interpolate" && statistics == "approximate")
	    statistics <- if(trace.hat == "exact") "1.approx"
            else "2.approx" # trace.hat == "approximate"
	surf.stat <- paste(surface, statistics, sep = "/")
        if (length(span) != 1L) stop("invalid argument 'span'")
        if (length(cell) != 1L) stop("invalid argument 'cell'")
        if (length(degree) != 1L) stop("invalid argument 'degree'")
	z <- .C(C_loess_raw, # ../src/loessc.c
		y, x, weights, robust, D, N,
		as.double(span),
		as.integer(degree),
		as.integer(nonparametric),
		as.integer(order.drop.sqr),
		as.integer(sum.drop.sqr),
		as.double(span*cell),
		as.character(surf.stat),
		fitted.values = double(N),
		parameter = integer(7L),
		a = integer(max.kd),
		xi = double(max.kd),
		vert = double(2L*D),
		vval = double((D+1L)*max.kd),
		diagonal = double(N),
		trL = double(1L),
		delta1 = double(1L),
		delta2 = double(1L),
		as.integer(surf.stat == "interpolate/exact"))
	if(j==1) {
	    trace.hat.out <- z$trL
	    one.delta <- z$delta1
	    two.delta <- z$delta2
	}
	fitted.residuals <- y - z$fitted.values
	if(j < iterations)
	    robust <- .Fortran(C_lowesw, fitted.residuals, N,
			       robust = double(N), integer(N))$robust
    }
    if(surface == "interpolate")
    {
	pars <- setNames(z$parameter,
			 c("d", "n", "vc", "nc", "nv", "liv", "lv"))
	enough <- (D + 1L) * pars["nv"]
	fit.kd <- list(parameter=pars, a=z$a[1L:pars[4L]], xi=z$xi[1L:pars[4L]],
		       vert=z$vert, vval=z$vval[1L:enough])
    }
    if(iterations > 1L) {
	pseudovalues <- .Fortran(C_lowesp,
				 N,
				 as.double(y),
				 as.double(z$fitted.values),
				 as.double(weights),
				 as.double(robust),
				 integer(N),
				 pseudovalues = double(N))$pseudovalues
	zz <- .C(C_loess_raw,
		as.double(pseudovalues), # ? needed
		x, weights, weights, D, N,
		as.double(span),
		as.integer(degree),
		as.integer(nonparametric),
		as.integer(order.drop.sqr),
		as.integer(sum.drop.sqr),
		as.double(span*cell),
		as.character(surf.stat),
		temp = double(N),
		parameter = integer(7L),
		a = integer(max.kd),
		xi = double(max.kd),
		vert = double(2L*D),
		vval = double((D+1L)*max.kd),
		diagonal = double(N),
		trL = double(1L),
		delta1 = double(1L),
		delta2 = double(1L),
		0L)
	pseudo.resid <- pseudovalues - zz$temp
    }
    sum.squares <- if(iterations <= 1L) sum(weights * fitted.residuals^2)
    else sum(weights * pseudo.resid^2)
    enp <- one.delta + 2*trace.hat.out - N
    s <- sqrt(sum.squares/one.delta)
    pars <- list(robust = robust, span = span, degree = degree,
                 normalize = normalize,
		 parametric = parametric, drop.square = drop.square,
		 surface = surface, cell = cell,
                 family = if(iterations <= 1L) "gaussian" else "symmetric",
		 iterations = iterations)
    fit <- list(n = N, fitted = z$fitted.values, residuals = fitted.residuals,
		enp = enp, s = s, one.delta = one.delta, two.delta = two.delta,
		trace.hat = trace.hat.out, divisor = divisor)
    fit$pars <- pars
    if(surface == "interpolate") fit$kd <- fit.kd
    class(fit) <- "loess"
    fit
}

predict.loess <-
    function(object, newdata = NULL, se = FALSE, na.action = na.pass, ...)
{
    if(!inherits(object, "loess"))
	stop("first argument must be a \"loess\" object")
    if(is.null(newdata) && !se)
	return(fitted(object))

    newx <- if(is.null(newdata)) object$x
    else if(is.data.frame(newdata))
        as.matrix(model.frame(delete.response(terms(object)), newdata,
                              na.action = na.action))
    else as.matrix(newdata) # this case is undocumented
    res <-
        with(object, predLoess(y, x, newx, s, weights, pars$robust,
                               pars$span, pars$degree, pars$normalize,
                               pars$parametric, pars$drop.square, pars$surface,
                               pars$cell, pars$family, kd, divisor, se = se))
    if(!is.null(out.attrs <- attr(newdata, "out.attrs"))) { # expand.grid used
        if(se) {
            res$fit <- array(res$fit, out.attrs$dim, out.attrs$dimnames)
            res$se.fit <- array(res$se.fit, out.attrs$dim, out.attrs$dimnames)
        } else res <- array(res, out.attrs$dim, out.attrs$dimnames)
    }
    if(se)
	res$df <- object$one.delta^2/object$two.delta
    res
}

predLoess <-
  function(y, x, newx, s, weights, robust, span, degree,
	   normalize, parametric, drop.square, surface, cell, family,
	   kd, divisor, se = FALSE)
{
    ## translation of pred_
    D <- NCOL(x); N <- NROW(x); M <- NROW(newx)
    x <- as.matrix(x); newx <- as.matrix(newx)
    newx <- newx/rep(divisor, rep(M, D))
    x <- x/rep(divisor, rep(N, D))
    sum.drop.sqr <- sum(drop.square)
    nonparametric <- sum(!parametric)
    order.parametric <- order(parametric)
    x <- x[, order.parametric, drop=FALSE]
    x.evaluate <- newx[, order.parametric, drop=FALSE]
    order.drop.sqr <- (2L - drop.square)[order.parametric]
    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    if(surface == "direct") {
        nas <- rowSums(is.na(newx)) > 0L
        fit <- rep(NA_real_, length(nas))
        x.evaluate <- x.evaluate[!nas,, drop = FALSE]
        M <- nrow(x.evaluate)
	if(se) {
            se.fit <- fit
	    z <- .C(C_loess_dfitse,
		    y,
		    x,
		    as.double(x.evaluate),
		    as.double(weights*robust),
		    as.double(robust),
		    as.integer(family =="gaussian"),
		    as.double(span),
		    as.integer(degree),
		    as.integer(nonparametric),
		    as.integer(order.drop.sqr),
		    as.integer(sum.drop.sqr),
		    as.integer(D),
		    as.integer(N),
		    as.integer(M),
		    fit = double(M),
		    L = double(N*M))[c("fit", "L")]
	    fit[!nas] <- z$fit
	    ses <- (matrix(z$L^2, M, N)/rep(weights, rep(M,N))) %*% rep(1,N)
	    se.fit[!nas] <- drop(s * sqrt(ses))
	} else {
	    fit[!nas] <- .C(C_loess_dfit,
                            y,
                            x,
                            as.double(x.evaluate),
                            as.double(weights*robust),
                            as.double(span),
                            as.integer(degree),
                            as.integer(nonparametric),
                            as.integer(order.drop.sqr),
                            as.integer(sum.drop.sqr),
                            as.integer(D),
                            as.integer(N),
                            as.integer(M),
                            fit = double(M))$fit
	}
    }
    else { ## interpolate
	## need to eliminate points outside original range - not in pred_
	inside <- matrix(FALSE, M, ncol = D)
	ranges <- apply(x, 2L, range)
	inside <- (x.evaluate <= rep(ranges[2L,], rep(M, D))) &
	(x.evaluate >= rep(ranges[1L,], rep(M, D)))
	inside <- inside %*% rep(1, D) == D
        inside[is.na(inside)] <- FALSE
	M1 <- sum(inside)
	fit <- rep(NA_real_, M)
	if(any(inside))
	    fit[inside] <- .C(C_loess_ifit,
			      as.integer(kd$parameter),
			      as.integer(kd$a), as.double(kd$xi),
			      as.double(kd$vert), as.double(kd$vval),
			      as.integer(M1),
			      as.double(x.evaluate[inside, ]),
			      fit = double(M1))$fit
	if(se) {
	    se.fit <- rep(NA_real_, M)
	    if(any(inside)) {
		L <- .C(C_loess_ise,
			y,
			x,
			as.double(x.evaluate[inside, ]),
			as.double(weights),
			as.double(span),
			as.integer(degree),
			as.integer(nonparametric),
			as.integer(order.drop.sqr),
			as.integer(sum.drop.sqr),
			as.double(span*cell),
			as.integer(D),
			as.integer(N),
			as.integer(M1),
			double(M1),
			L = double(N*M1)
			)$L
		tmp <- (matrix(L^2, M1, N)/rep(weights, rep(M1,N))) %*% rep(1,N)
		se.fit[inside] <- drop(s * sqrt(tmp))
	    }
	}
    }
    rn <- rownames(newx)
    if(se) {
        if(!is.null(rn)) names(fit) <- names(se.fit) <- rn
        list(fit = fit, se.fit = drop(se.fit), residual.scale = s)
    } else {
        if(!is.null(rn)) names(fit) <- rn
        fit
    }
}

pointwise <- function(results, coverage)
{
    fit <- results$fit
    lim <- qt((1 - coverage)/2, results$df, lower.tail = FALSE) * results$se.fit
    list(fit = fit, lower = fit - lim, upper = fit + lim)
}

print.loess <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl, control=NULL)
    }
    cat("\nNumber of Observations:", x$n, "\n")
    cat("Equivalent Number of Parameters:", format(round(x$enp, 2L)), "\n")
    cat("Residual",
	if(x$pars$family == "gaussian")"Standard Error:" else "Scale Estimate:",
	format(signif(x$s, digits)), "\n")
    invisible(x)
}

summary.loess <- function(object, ...)
{
    class(object) <- "summary.loess"
    object
}

print.summary.loess <-
    function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl, control=NULL)
    }
    cat("\nNumber of Observations:", x$n, "\n")
    cat("Equivalent Number of Parameters:", format(round(x$enp, 2)), "\n")
    if(x$pars$family == "gaussian")
	cat("Residual Standard Error:", format(signif(x$s, digits)), "\n")
    else cat("Residual Scale Estimate:", format(signif(x$s, digits)), "\n")
    cat("Trace of smoother matrix:", format(round(x$trace.hat, 2L)), "\n")
    cat("\nControl settings:\n")
    cat("  normalize: ", x$pars$normalize, "\n")
    cat("  span	    : ", format(x$pars$span), "\n")
    cat("  degree   : ", x$pars$degree, "\n")
    cat("  family   : ", x$pars$family)
    if(x$pars$family != "gaussian")
	cat("	    iterations =", x$pars$iterations)
    cat("\n  surface  : ", x$pars$surface)
    if(x$pars$surface == "interpolate")
	cat("	  cell =", format(x$pars$cell))
    cat("\n")
    invisible(x)
}

scatter.smooth <-
    function(x, y = NULL, span = 2/3, degree = 1,
	     family = c("symmetric", "gaussian"),
	     xlab = NULL, ylab = NULL,
	     ylim = range(y, pred$y, na.rm = TRUE),
             evaluation = 50, ..., lpars = list())
{
    xlabel <- if (!missing(x)) deparse(substitute(x))
    ylabel <- if (!missing(y)) deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel)
    x <- xy$x
    y <- xy$y
    xlab <- if (is.null(xlab)) xy$xlab else xlab
    ylab <- if (is.null(ylab)) xy$ylab else ylab
    pred <- loess.smooth(x, y, span, degree, family, evaluation)
    plot(x, y, ylim = ylim, xlab = xlab, ylab = ylab, ...)
    do.call(lines, c(list(pred), lpars))
    invisible()
}

loess.smooth <-
  function(x, y, span = 2/3, degree = 1, family = c("symmetric", "gaussian"),
	   evaluation = 50, ...)
{
    notna <- !(is.na(x) | is.na(y))
    new.x <- seq.int(min(x[notna]), max(x[notna]), length.out = evaluation)

    control <- loess.control(...)
    x <- x[notna]; y <- y[notna]
    w <- rep(1, length(y))
    family <- match.arg(family)
    iterations <- if(family == "gaussian") 1L else control$iterations
    fit <- simpleLoess(y, x, w, span, degree, FALSE, FALSE,
		       normalize=FALSE, "none", "interpolate",
		       control$cell, iterations, control$trace.hat)
    kd <- fit$kd
    z <- .C(C_loess_ifit,
	    as.integer(kd$parameter),
	    as.integer(kd$a), as.double(kd$xi),
	    as.double(kd$vert), as.double(kd$vval),
	    as.integer(evaluation),
	    as.double(new.x),
	    fit = double(evaluation))$fit
    list(x = new.x, y = z)
}

anova.loess <- function(object, ...)
{
    objects <- list(object, ...)
    responses <- as.character(lapply(objects,
				     function(x) as.character(x$terms[[2L]])))
    sameresp <- responses == responses[1L]
    ## calculate the number of models
    if (!all(sameresp)) {
	objects <- objects[sameresp]
        warning(gettextf("models with response %s removed because response differs from model 1",
                         sQuote(deparse(responses[!sameresp]))),
                domain = NA)
    }
    nmodels <- length(objects)
    if(nmodels <= 1L) stop("no models to compare")
    models <- as.character(lapply(objects, function(x) x$call))
    descr <- paste("Model ", format(1L:nmodels), ": ", models,
		   sep = "", collapse = "\n")
    ## extract statistics
    delta1 <- sapply(objects, function(x) x$one.delta)
    delta2 <- sapply(objects, function(x) x$two.delta)
    s <- sapply(objects, function(x) x$s)
    enp <- sapply(objects, function(x) x$enp)
    rss <- s^2*delta1
    max.enp <- order(enp)[nmodels]
    d1diff <- abs(diff(delta1))
    dfnum <- c(d1diff^2/abs(diff(delta2)))
    dfden <- (delta1^2/delta2)[max.enp]
    Fvalue <- c(NA, (abs(diff(rss))/d1diff)/s[max.enp]^2)
    pr <- pf(Fvalue, dfnum, dfden, lower.tail = FALSE)
    ans <- data.frame(ENP = round(enp,2L), RSS = rss, "F-value" = Fvalue,
		      "Pr(>F)" = pr, check.names = FALSE)
    attr(ans, "heading") <-
	paste0(descr, "\n\n", "Analysis of Variance:   denominator df ",
               format(round(dfden, 2L)), "\n")
    class(ans) <- c("anova", "data.frame")
    ans
}
