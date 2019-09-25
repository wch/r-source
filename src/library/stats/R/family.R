#  File src/library/stats/R/family.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

family <- function(object, ...) UseMethod("family")

print.family <- function(x, ...)
{
    cat("\nFamily:", x$family, "\n")
    cat("Link function:", x$link, "\n\n")
    invisible(x)
}

power <- function(lambda = 1)
{
    if(!is.numeric(lambda) || is.na(lambda))
        stop("invalid argument 'lambda'")
    if(lambda <= 0) return(make.link("log"))
    if(lambda == 1) return(make.link("identity"))
    linkfun <- function(mu) mu^lambda
    linkinv <- function(eta)
        pmax(eta^(1/lambda), .Machine$double.eps)
    mu.eta <- function(eta)
        pmax((1/lambda) * eta^(1/lambda - 1), .Machine$double.eps)
    valideta <- function(eta) all(is.finite(eta)) && all(eta>0)
    link <- paste0("mu^", round(lambda, 3))
    structure(list(linkfun = linkfun, linkinv = linkinv,
                   mu.eta = mu.eta, valideta = valideta, name = link),
              class="link-glm")
}

## Written by Simon Davies Dec 1995
## Modified by Thomas Lumley 26 Apr 97
## added valideta(eta) function..
make.link <- function (link)
{
    switch(link,
           "logit" = {
               linkfun <- function(mu) .Call(C_logit_link, mu)
               linkinv <- function(eta) .Call(C_logit_linkinv, eta)
               mu.eta <- function(eta) .Call(C_logit_mu_eta, eta)
               valideta <- function(eta) TRUE
           },
           "probit" = {
               linkfun <- function(mu) qnorm(mu)
               linkinv <- function(eta) {
                   thresh <- - qnorm(.Machine$double.eps)
                   eta <- pmin(pmax(eta, -thresh), thresh)
                   pnorm(eta)
               }
               mu.eta <- function(eta)
                   pmax(dnorm(eta),.Machine$double.eps)
               valideta <- function(eta) TRUE
           },
           "cauchit" = {
               linkfun <- function(mu) qcauchy(mu)
               linkinv <- function(eta) {
                   thresh <- -qcauchy(.Machine$double.eps)
                   eta <- pmin(pmax(eta, -thresh), thresh)
                   pcauchy(eta)
               }
               mu.eta <- function(eta)
                   pmax(dcauchy(eta), .Machine$double.eps)
               valideta <- function(eta) TRUE
           },
           "cloglog" = {
               linkfun <- function(mu) log(-log(1 - mu))
               linkinv <- function(eta)
                   pmax(pmin(-expm1(-exp(eta)), 1 - .Machine$double.eps),
                        .Machine$double.eps)
               mu.eta <- function(eta) {
                   eta <- pmin(eta, 700)
                   pmax(exp(eta) * exp(-exp(eta)), .Machine$double.eps)
               }
               valideta <- function(eta) TRUE
           },
           "identity" = {
               linkfun <- function(mu) mu
               linkinv <- function(eta) eta
               mu.eta <- function(eta) rep.int(1, length(eta))
               valideta <- function(eta) TRUE
           },
           "log" = {
               linkfun <- function(mu) log(mu)
               linkinv <- function(eta)
                   pmax(exp(eta), .Machine$double.eps)
               mu.eta <- function(eta)
                   pmax(exp(eta), .Machine$double.eps)
               valideta <- function(eta) TRUE
           },
           "sqrt" = {
               linkfun <- function(mu) sqrt(mu)
               linkinv <- function(eta) eta^2
               mu.eta <- function(eta) 2 * eta
               valideta <- function(eta) all(is.finite(eta)) && all(eta>0)
           },
           "1/mu^2" = {
               linkfun <- function(mu) 1/mu^2
               linkinv <- function(eta) 1/sqrt(eta)
               mu.eta <- function(eta) -1/(2 * eta^1.5)
               valideta <- function(eta) all(is.finite(eta)) && all(eta>0)
           },
           "inverse" = {
               linkfun <- function(mu) 1/mu
               linkinv <- function(eta) 1/eta
               mu.eta <- function(eta) -1/(eta^2)
               valideta <- function(eta) all(is.finite(eta)) && all(eta != 0)
           },
           ## else :
           stop(gettextf("%s link not recognised", sQuote(link)),
                domain = NA)
           )# end switch(.)
    environment(linkfun) <- environment(linkinv) <- environment(mu.eta) <-
        environment(valideta) <- asNamespace("stats")
    structure(list(linkfun = linkfun, linkinv = linkinv,
                   mu.eta = mu.eta, valideta = valideta, name = link),
              class="link-glm")
}

poisson <- function (link = "log")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) linktemp <- deparse(linktemp)
    okLinks <- c("log", "identity", "sqrt")
    family <- "poisson"
    if (linktemp %in% okLinks)
	stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
            stop(gettextf('link "%s" not available for %s family; available links are %s',
			  linktemp, family, paste(sQuote(okLinks), collapse =", ")),
		 domain = NA)
        }
    }
    variance <- function(mu) mu
    validmu <- function(mu) all(is.finite(mu)) && all(mu>0)
    dev.resids <- function(y, mu, wt)
    { ## faster than  2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))
	r <- mu*wt
	p <- which(y > 0)
	r[p] <- (wt * (y*log(y/mu) - (y - mu)))[p]
	2*r
    }
    aic <- function(y, n, mu, wt, dev) -2*sum(dpois(y, mu, log=TRUE)*wt)
    initialize <- expression({
	if (any(y < 0))
	    stop("negative values not allowed for the 'Poisson' family")
	n <- rep.int(1, nobs)
	mustart <- y + 0.1
    })
    simfun <- function(object, nsim) {
        ## A Poisson GLM has dispersion fixed at 1, so prior weights
        ## do not have a simple unambiguous interpretation:
        ## they might be frequency weights or indicate averages.
        wts <- object$prior.weights
        if (any(wts != 1)) warning("ignoring prior weights")
        ftd <- fitted(object)
        rpois(nsim*length(ftd), ftd)
    }
    structure(list(family = family,
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta,
                   simulate = simfun),
	      class = "family")
}

quasipoisson <- function (link = "log")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) linktemp <- deparse(linktemp)
    okLinks <- c("log", "identity", "sqrt")
    family <- "quasipoisson"
    if (linktemp %in% okLinks)
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for %s family; available links are %s',
			  linktemp, family, paste(sQuote(okLinks), collapse =", ")),
                 domain = NA)
        }
    }
    variance <- function(mu) mu
    validmu <- function(mu) all(is.finite(mu)) && all(mu>0)
    dev.resids <- function(y, mu, wt)
    { ## faster than  2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))
	r <- mu*wt
	p <- which(y > 0)
	r[p] <- (wt * (y*log(y/mu) - (y - mu)))[p]
	2*r
    }
    aic <- function(y, n, mu, wt, dev) NA
    initialize <- expression({
	if (any(y < 0))
	    stop("negative values not allowed for the 'quasiPoisson' family")
	n <- rep.int(1, nobs)
	mustart <- y + 0.1
    })
    structure(list(family = family,
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta),
	      class = "family")
}

gaussian <- function (link = "identity")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) linktemp <- deparse(linktemp)
    okLinks <- c("inverse", "log", "identity")
    family <- "gaussian"
    if (linktemp %in% okLinks)
	stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for %s family; available links are %s',
			  linktemp, family, paste(sQuote(okLinks), collapse =", ")),
		 domain = NA)
        }
    }
    structure(list(family = family,
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = function(mu) rep.int(1, length(mu)),
		   dev.resids = function(y, mu, wt) wt * ((y - mu)^2),
		   aic =	function(y, n, mu, wt, dev) {
                       nobs <- length(y)
                       nobs*(log(dev/nobs*2*pi)+1)+2 - sum(log(wt))
                   },
		   mu.eta = stats$mu.eta,
		   initialize = expression({
		       n <- rep.int(1, nobs)
                       if(is.null(etastart) && is.null(start) &&
                          is.null(mustart) &&
                          ((family$link == "inverse" && any(y == 0)) ||
                          (family$link == "log" && any(y <= 0))))
                           stop("cannot find valid starting values: please specify some")

		       mustart <- y }),
		   validmu = function(mu) TRUE,
		   valideta = stats$valideta
		   ),
	      class = "family")
}

binomInitialize <- function(family) substitute({ # other "arg"s:  (y, weights, nobs)
    if (NCOL(y) == 1) {
	## allow factors as responses
	## added BDR 29/5/98
	if (is.factor(y)) y <- y != levels(y)[1L]
	n <- rep.int(1, nobs)
	## anything, e.g. NA/NaN, for cases with zero weight is OK.
	y[weights == 0] <- 0
	if (any(y < 0 | y > 1))
	    stop("y values must be 0 <= y <= 1")
	mustart <- (weights * y + 0.5)/(weights + 1)
	m <- weights * y
	if(FAMILY == "binomial" && any(abs(m - round(m)) > 1e-3))
	    warning(gettextf("non-integer #successes in a %s glm!", FAMILY),
		    domain = NA)
    }
    else if (NCOL(y) == 2) {
	if(FAMILY == "binomial" && any(abs(y - round(y)) > 1e-3))
	    warning(gettextf("non-integer counts in a %s glm!", FAMILY),
		    domain = NA)
	n <- (y1 <- y[, 1L]) + y[, 2L]
	y <- ## ifelse(n == 0, 0, y[, 1]/n)
	    y1/n; if(any(n0 <- n == 0)) y[n0] <- 0
	weights <- weights * n
	mustart <- (n * y + 0.5)/(n + 1)
    }
    else stop(gettextf(
      "for the '%s' family, y must be a vector of 0 and 1\'s\nor a 2 column matrix where col 1 is no. successes and col 2 is no. failures",
                       FAMILY),
             domain = NA)
}, list(FAMILY = family))

binomial <- function (link = "logit")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) linktemp <- deparse(linktemp)
    okLinks <- c("logit", "probit", "cloglog", "cauchit", "log")
    family <- "binomial"
    if (linktemp %in% okLinks)
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for %s family; available links are %s',
			  linktemp, family, paste(sQuote(okLinks), collapse =", ")),
                 domain = NA)
        }
    }
    variance <- function(mu) mu * (1 - mu)
    validmu <- function(mu) all(is.finite(mu)) && all(mu>0 &mu<1)
    dev.resids <- function(y, mu, wt) .Call(C_binomial_dev_resids, y, mu, wt)
    aic <- function(y, n, mu, wt, dev) {
        m <- if(any(n > 1)) n else wt
	-2*sum(ifelse(m > 0, (wt/m), 0)*
               dbinom(round(m*y), round(m), mu, log=TRUE))
    }
    simfun <- function(object, nsim) {
        ftd <- fitted(object)
        n <- length(ftd)
        ntot <- n*nsim
        wts <- object$prior.weights
        if (any(wts %% 1 != 0))
            stop("cannot simulate from non-integer prior.weights")
        ## Try to fathom out if the original data were
        ## proportions, a factor or a two-column matrix
        if (!is.null(m <- object$model)) {
            y <- model.response(m)
            if(is.factor(y)) {
                ## ignore weights
                yy <- factor(1+rbinom(ntot, size = 1, prob = ftd),
                             labels = levels(y))
                split(yy, rep(seq_len(nsim), each = n))
            } else if(is.matrix(y) && ncol(y) == 2) {
                yy <- vector("list", nsim)
                for (i in seq_len(nsim)) {
                    Y <- rbinom(n, size = wts, prob = ftd)
                    YY <- cbind(Y, wts - Y)
                    colnames(YY) <- colnames(y)
                    yy[[i]] <- YY
                }
                yy
            } else
               rbinom(ntot, size = wts, prob = ftd)/wts
        } else rbinom(ntot, size = wts, prob = ftd)/wts
    }
    structure(list(family = family,
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = binomInitialize(family),
		   validmu = validmu,
		   valideta = stats$valideta,
                   simulate = simfun),
	      class = "family")
}

quasibinomial <- function (link = "logit")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) linktemp <- deparse(linktemp)
    okLinks <- c("logit", "probit", "cloglog", "cauchit", "log")
    family <- "quasibinomial"
    if (linktemp %in% okLinks)
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for %s family; available links are %s',
			  linktemp, family, paste(sQuote(okLinks), collapse =", ")),
                 domain = NA)
        }
    }
    structure(list(family = family,
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = function(mu) mu * (1 - mu),
		   dev.resids =  function(y, mu, wt)
                       .Call(C_binomial_dev_resids, y, mu, wt),
		   aic = function(y, n, mu, wt, dev) NA,
		   mu.eta = stats$mu.eta,
		   initialize = binomInitialize(family),
		   validmu = function(mu) all(is.finite(mu)) && all(0 < mu & mu < 1),
		   valideta = stats$valideta),
	      class = "family")
}

Gamma <- function (link = "inverse")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) linktemp <- deparse(linktemp)
    okLinks <- c("inverse", "log", "identity")
    family <- "Gamma"
    if (linktemp %in% okLinks)
	stats <- make.link(linktemp)
    else if(is.character(link)) stats <- make.link(link)
    else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for %s family; available links are %s',
			  linktemp, family, paste(sQuote(okLinks), collapse =", ")),
                 domain = NA)
        }
    }
    variance <- function(mu) mu^2
    validmu <- function(mu) all(is.finite(mu)) && all(mu>0)
    dev.resids <- function(y, mu, wt)
	-2 * wt * (log(ifelse(y == 0, 1, y/mu)) - (y - mu)/mu)
    aic <- function(y, n, mu, wt, dev){
	n <- sum(wt)
	disp <- dev/n
	-2*sum(dgamma(y, 1/disp, scale=mu*disp, log=TRUE)*wt) + 2
    }
    initialize <- expression({
	if (any(y <= 0))
	    stop("non-positive values not allowed for the 'Gamma' family")
	n <- rep.int(1, nobs)
	mustart <- y
    })
    simfun <- function(object, nsim) {
        wts <- object$prior.weights
        if (any(wts != 1)) message("using weights as shape parameters")
        ftd <- fitted(object)
        shape <- MASS::gamma.shape(object)$alpha * wts
        rgamma(nsim*length(ftd), shape = shape, rate = shape/ftd)
    }
    structure(list(family = family,
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta,
                   simulate = simfun),
	      class = "family")
}

inverse.gaussian <- function(link = "1/mu^2")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) linktemp <- deparse(linktemp)
    family <- "inverse.gaussian"
    okLinks <- c("inverse", "log", "identity", "1/mu^2")
    if (linktemp %in% okLinks)
	stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for %s family; available links are %s',
			  linktemp, family, paste(sQuote(okLinks), collapse =", ")),
                 domain = NA)
        }
    }
    variance <- function(mu) mu^3
    dev.resids <- function(y, mu, wt)  wt*((y - mu)^2)/(y*mu^2)
    aic <- function(y, n, mu, wt, dev)
	sum(wt)*(1+log(dev/sum(wt)*2*pi)) + 3*sum(log(y)*wt) + 2
    initialize <- expression({
	if(any(y <= 0))
	    stop("positive values only are allowed for the 'inverse.gaussian' family")
	n <- rep.int(1, nobs)
	mustart <- y
    })
    validmu <- function(mu) TRUE
    simfun <- function(object, nsim) {
        if(!requireNamespace("SuppDists", quietly = TRUE))
            stop("need CRAN package 'SuppDists' for simulation from the 'inverse.gaussian' family")
        wts <- object$prior.weights
        if (any(wts != 1)) message("using weights as inverse variances")
        ftd <- fitted(object)
        SuppDists::rinvGauss(nsim * length(ftd), nu = ftd,
                             lambda = wts/summary(object)$dispersion)
    }

    structure(list(family = family,
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = variance,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta,
                   simulate = simfun),
	      class = "family")
}

quasi <- function (link = "identity", variance = "constant")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) linktemp <- deparse(linktemp)
    if (linktemp %in% c("logit", "probit", "cloglog", "identity",
                        "inverse", "log", "1/mu^2", "sqrt"))
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        stats <- link
        linktemp <- if(!is.null(stats$name)) stats$name else deparse(linktemp)
    }
    maybeV <- is.character(vtemp <- substitute(variance)) ||
        (is.symbol(vtemp) && (vtemp == quote(mu) || vtemp == quote(constant))) ||
        (is.call(vtemp) &&
         (length(vtemp) == 2L && vtemp == quote(mu(1-mu))) ||
         (length(vtemp) == 3L && (vtemp == quote(mu^2)     ||
                                  vtemp == quote(mu^3))))
    if(!maybeV && (is.list(variance) && !anyNA(match(c("varfun", "validmu"), names(variance)))))
        variance_nm <- NA
    else {
    if (!is.character(vtemp)) vtemp <- deparse(vtemp)
    variance_nm <- vtemp <- gsub(" ", "", vtemp, fixed=TRUE)
    switch(vtemp,
           "constant" = {
               varfun <- function(mu) rep.int(1, length(mu))
               dev.resids <- function(y, mu, wt) wt * ((y - mu)^2)
                   validmu <- function(mu) TRUE
               initialize <- expression({n <- rep.int(1, nobs); mustart <- y})
           },
           "mu(1-mu)" = {
               varfun <- function(mu) mu * (1 - mu)
               validmu <- function(mu) all(mu>0) && all(mu<1)
               dev.resids <- function(y, mu, wt) .Call(C_binomial_dev_resids, y, mu, wt)
               initialize <- expression({n <- rep.int(1, nobs)
                                         mustart <- pmax(0.001, pmin(0.999, y))})
           },
           "mu" = {
               varfun <- function(mu) mu
               validmu <- function(mu) all(mu>0)
               dev.resids <- function(y, mu, wt)
                   2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))
               ## 0.1 fudge here matches poisson: S has 1/6.
               initialize <- expression({n <- rep.int(1, nobs)
                                         mustart <- y + 0.1 * (y == 0)})
           },
           "mu^2" = {
               varfun <- function(mu) mu^2
               validmu <- function(mu) all(mu>0)
               dev.resids <- function(y, mu, wt)
		   pmax(-2 * wt * (log(ifelse(y == 0, 1, y)/mu) - (y - mu)/mu), 0)
               initialize <- expression({n <- rep.int(1, nobs)
                                         mustart <- y + 0.1 * (y == 0)})
           },
           "mu^3" = {
               varfun <- function(mu) mu^3
               validmu <- function(mu) all(mu>0)
               dev.resids <- function(y, mu, wt)
                   wt * ((y - mu)^2)/(y * mu^2)
               initialize <- expression({n <- rep.int(1, nobs)
                                         mustart <- y + 0.1 * (y == 0)})
           },
           variance_nm <- NA
           )# end switch(.)
    }
    if(is.na(variance_nm)) {
        if(is.character(variance))
            stop(gettextf('\'variance\' "%s" is invalid: possible values are "mu(1-mu)", "mu", "mu^2", "mu^3" and "constant"', vtemp), domain = NA)
        ## so we really meant the object.
        varfun <- variance$varfun
        validmu <- variance$validmu
        dev.resids <- variance$dev.resids
        initialize <- variance$initialize
        variance_nm <- variance$name
    }
    aic <- function(y, n, mu, wt, dev) NA
    structure(list(family = "quasi",
		   link = linktemp,
		   linkfun = stats$linkfun,
		   linkinv = stats$linkinv,
		   variance = varfun,
		   dev.resids = dev.resids,
		   aic = aic,
		   mu.eta = stats$mu.eta,
		   initialize = initialize,
		   validmu = validmu,
		   valideta = stats$valideta,
                   ## character form of the var fun is needed for gee
                   varfun = variance_nm),
	      class = "family")
}
