### file lqs/R/lqs.R
### copyright (C) 1998-9 B. D. Ripley

lqs <- function(x, ...) UseMethod("lqs")

lqs.formula <-
    function(formula, data, ...,
	     method = c("lts" ,"lqs", "lms", "S", "model.frame"),
	     subset, na.action = na.fail,
	     model = TRUE, x.ret = FALSE, y.ret = FALSE, contrasts = NULL)
{
    method <- match.arg(method)
    mf <- match.call(expand.dots = FALSE)
    mf$method <- mf$contrasts <- mf$model <- mf$x <- mf$y <- mf$... <- NULL
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, sys.frame(sys.parent()))
    if (method == "model.frame") return(mf)
    mt <- attr(mf, "terms")
    y <- model.extract(mf, "response")
    x <- model.matrix(mt, mf, contrasts)
    xvars <- as.character(attr(mt, "variables"))[-1]
    if(yvar <- attr(mt, "response") > 0)
	xvars <- xvars[-yvar]
    xlev <-
	if(length(xvars) > 0) {
	    xlev <- lapply(mf[xvars], levels)
	    xlev[!sapply(xlev, is.null)]
	}
    xint <- match("(Intercept)", colnames(x), nomatch = 0)
    if(xint) x <- x[, -xint, drop = FALSE]
    fit <- lqs.default(x, y, intercept = (xint>0), method = method, ...)
    fit$terms <- mt
    fit$call <- match.call()
    fit$contrasts <- attr(x, "contrasts")
    fit$xlevels <- xlev
    if(model) fit$model <- mf
    if(x.ret) fit$x <- x
    if(y.ret) fit$y <- y
    fit
}

lqs.default <-
    function(x, y, intercept=TRUE, method = c("lts", "lqs", "lms", "S"),
	     quantile, control = lqs.control(...), k0 = 1.548, seed, ...)
{
    lqs.control <- function(psamp = NA, nsamp = "best", adjust = TRUE)
	list(psamp = psamp, nsamp = nsamp, adjust = adjust)

    n <- length(y)
    nmx <- deparse(substitute(x))
    if(is.null(dim(x))) {
	x <- as.matrix(x)
	colnames(x) <- nmx
    } else x <- as.matrix(x)
    p <- ncol(x)
    if(any(is.na(x)) || any(is.na(y)))
	stop("missing values are not allowed")
    nm <- colnames(x)
    if(is.null(nm))
	nm <- if(p > 1) paste("X", 1:p, sep="") else if(p == 1) "X" else NULL
    if(intercept) {
	x <- cbind(1, x)
	nm <- c("(Intercept)", nm)
    }
    p <- ncol(x)
    if(nrow(x) != n) stop("x and y must have the same number of rows")
    method <- match.arg(method)
    lts <- 0; beta <- 0
    if(method == "lqs" && missing(quantile)) quantile <- floor((n+p+1)/2)
    if(method == "lms") quantile <- floor((n+1)/2)
    if(method == "lts") {
	lts <- 1
	if(missing(quantile)) quantile <- floor(n/2) + floor((p+1)/2)
    }
    if(method == "S") {
	lts <- 2
	beta <- 0.5
	quantile <- ceiling(n/2)
	chi <- function(u, k0)
	{ u <- (u/k0)^2; ifelse(u < 1, 3*u - 3*u^2 + u^3, 1) }
    }
    ps <- control$psamp
    if(is.na(ps)) ps <- p
    if(ps < p) {
	ps <- p
	warning("ps must be at least p")
    }
    adj <- control$adjust & intercept
    nsamp <- eval(control$nsamp)
    nexact <- choose(n, ps)
    if(is.character(nsamp) && nsamp == "best")
	nsamp <- if(nexact < 5000) "exact" else "sample"
    if(is.numeric(nsamp) && nsamp > nexact) {
	warning(paste("only", nexact, "sets, so all sets will be tried"))
	nsamp <- "exact"
    }
    if(nsamp == "exact") {
	nsamp <- nexact
	samp <- FALSE
    } else {
	if(nsamp == "sample") nsamp <- min(500*ps, 3000)
	samp <- TRUE
    }
    if(samp && !missing(seed)) {
	if(exists(".Random.seed", envir=.GlobalEnv))  {
	    seed.keep <- .Random.seed
	    on.exit(assign(".Random.seed", seed.keep, envir=.GlobalEnv))
	}
	.Random.seed <<- seed
    }
    z <-  .C("lqs_fitlots",
	     as.double(x), as.double(y), as.integer(n), as.integer(p),
	     as.integer(quantile), as.integer(lts), as.integer(adj),
	     as.integer(samp), as.integer(ps), as.integer(nsamp),
	     crit=double(1), sing=integer(1), bestone=integer(ps),
	     coefficients=double(p), as.integer(k0), as.double(beta),
             PACKAGE="lqs"
	     )[c("crit", "sing", "coefficients", "bestone")]
    z$sing <- paste(z$sing, "singular samples of size", ps, "out of", nsamp)
    z$bestone <- sort(z$bestone)
    names(z$coefficients) <- nm
    fitted <- drop(x %*% z$coefficients)
    z$fitted.values <- fitted
    z$residuals <- y - fitted
    c1 <- 1/qnorm((n + quantile)/(2*n))
    if(lts == 1) {
	s <- sqrt(z$crit/quantile)/sqrt(1 - 2*n*dnorm(1/c1)/(quantile*c1))
    } else if(lts == 0) {
	s <- sqrt(z$crit)*c1
    } else s <- z$crit
    res <- z$residual
    ind <- abs(res) <= 2.5*s
    s2 <- sum(res[ind]^2)/(sum(ind) - p)
    z$scale <- c(s, sqrt(s2))
    if(method == "S") { # IWLS refinement
	psi <- function(u, k0) (1  - pmin(1, abs(u/k0))^2)^2
	coef <- z$coef
	resid <- z$residuals
	scale <- s
	for(i in 1:30) {
	    w <- psi(resid/scale, k0)
	    temp <- lm.wfit(x, y, w, method="qr")
	    resid <- temp$residuals
	    s2 <- scale*sqrt(sum(chi(resid/scale, k0))/((n-p)*beta))
	    if(abs(s2/scale - 1) < 1e-5) break
	    scale <- s2
	}
	z$coef <- temp$coef
	z$fitted.values <- temp$fitted.values
	z$residuals <- resid
	z$scale <- scale
    }
    class(z) <- "lqs"
    z
}

print.lqs <- function (x, digits = max(3, getOption("digits") - 3), ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl)
	cat("\n")
    }
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits), print.gap = 2,
		  quote = FALSE)
    cat("\nScale estimates", format(x$scale, digits = digits) ,"\n\n")
    invisible(x)
}

predict.lqs <- function (object, newdata, ...)
{
    if (missing(newdata)) return(object$fitted.values)
    X <- model.matrix(delete.response(terms(object)), newdata,
		      contrasts = object$contrasts, xlev = object$xlevels)
    drop(X %*% object$coefficients)
}


cov.rob <- function(x, cor = FALSE, quantile.used = floor((n+p+1)/2),
		    method = c("mve", "mcd", "classical"), nsamp = "best", seed)
{
    method <- match.arg(method)
    x <- as.matrix(x)
    if(any(is.na(x)) || any(is.infinite(x)))
	stop("missing or infinite values are not allowed")
    n <- nrow(x); p <- ncol(x)
    if(n < p+1) stop(paste("At least", p+1, "cases are needed"))
    if(method == "classical") {
	ans <- list(center = apply(x, 2, mean), cov = var(x))
    } else {
	if(quantile.used < p+1) stop(paste("quantile must be at least", p+1))
	## re-scale to roughly common scale
	divisor <- apply(x, 2, IQR)
	x <- x /rep(divisor, rep(n,p))
	qn <- quantile.used
	ps <- p + 1
	nexact <- choose(n, ps)
	if(is.character(nsamp) && nsamp == "best")
	    nsamp <- if(nexact < 5000) "exact" else "sample"
	if(is.numeric(nsamp) && nsamp > nexact) {
	    warning(paste("only", nexact, "sets, so all sets will be tried"))
	    nsamp <- "exact"
	}
	samp <- nsamp != "exact"
	if(samp) {
	    if(nsamp == "sample") nsamp <- min(500*ps, 3000)
	} else nsamp <- nexact

	if(samp && !missing(seed)) {
	    if(exists(".Random.seed", envir=.GlobalEnv))  {
		seed.keep <- .Random.seed
		on.exit(assign(".Random.seed", seed.keep, envir=.GlobalEnv))
	    }
	    .Random.seed <<- seed
	}
	z <-  .C("mve_fitlots",
		 as.double(x), as.integer(n), as.integer(p),
		 as.integer(qn), as.integer(method=="mcd"),
		 as.integer(samp), as.integer(ps), as.integer(nsamp),
		 crit=double(1), sing=integer(1), bestone=integer(n),
                 PACKAGE="lqs"
		 )
	z$sing <- paste(z$sing, "singular samples of size", ps, "out of", nsamp)
	crit <- z$crit + 2*sum(log(divisor)) +
	    if(method=="mcd") - p * log(qn - 1) else 0
	best <- seq(n)[z$bestone != 0]
	means <- apply(x[best, , drop = FALSE], 2, mean)
	rcov <- var(x[best, , drop = FALSE]) * (1 + 15/(n - p))^2
	dist <- mahalanobis(x, means, rcov)
	cut <- qchisq(0.975, p) * quantile(dist, qn/n)/qchisq(qn/n, p)
	cov <- divisor * var(x[dist < cut, , drop = FALSE]) *
	    rep(divisor, rep(p, p))
	attr(cov, "names") <- NULL
	ans <- list(center =
		    apply(x[dist < cut, , drop = FALSE], 2, mean) * divisor,
		    cov = cov, msg = z$sing, crit = crit, best = best)
    }
    if(cor) {
	sd <- sqrt(diag(ans$cov))
	ans <- c(ans, list(cor = (ans$cov/sd)/rep(sd, rep(p, p))))
    }
    ans$n.obs <- n
    ans
}

## compatibility functions for R users.

lmsreg <- function(...)
{
    oc <- sys.call()
    oc$method <- "lms"
    oc[[1]] <- as.name("lqs")
    eval(oc, sys.frame(sys.parent()))
}

ltsreg <- function(...)
{
    oc <- sys.call()
    oc$method <- "lts"
    oc[[1]] <- as.name("lqs")
    eval(oc, sys.frame(sys.parent()))
}

cov.mve <- function(...)
{
    oc <- sys.call()
    oc$method <- "mve"
    oc[[1]] <- as.name("cov.rob")
    eval(oc, sys.frame(sys.parent()))
}

cov.mcd <- function(...)
{
    oc <- sys.call()
    oc$method <- "mcd"
    oc[[1]] <- as.name("cov.rob")
    eval(oc, sys.frame(sys.parent()))
}
