family <- function(object, ...) UseMethod("family")

print.family <- function(x, ...)
{
    cat("\nFamily:", x$family, "\n")
    cat("Link function:", x$link, "\n\n")
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
    valideta <- function(eta) all(eta>0)
    link <- paste("mu^", round(lambda, 3), sep="")
    structure(list(linkfun = linkfun, linkinv = linkinv,
                   mu.eta = mu.eta, valideta = valideta, name = link),
              class="link-glm")
}

## Written by Simon Davies Dec 1995
## Modified by Thomas Lumley 26 Apr 97
## added valideta(eta) function..
make.link <- function (link)
{
    ## first two deprecated in 2.4.0
    if (is.character(link) && length(grep("^power", link) > 0)) {
        ## this seems intended to catch calls like "power(3)"
        warning('calling make.link("power(z)") is deprecated', domain = NA)
        return(eval(parse(text = link)))
    } else if(!is.character(link) && !is.na(lambda <- as.numeric(link))) {
        warning('calling make.link(number) is deprecated', domain = NA)
        return(power(lambda))
    } else
        switch(link,
               "logit" = {
                   linkfun <- function(mu)
                       .Call("logit_link", mu, PACKAGE="stats")
                   linkinv <- function(eta)
                       .Call("logit_linkinv", eta, PACKAGE="stats")
                   mu.eta <- function(eta)
                       .Call("logit_mu_eta", eta, PACKAGE="stats")
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
                   valideta <- function(eta) all(eta>0)
               },
               "1/mu^2" = {
                   linkfun <- function(mu) 1/mu^2
                   linkinv <- function(eta) 1/sqrt(eta)
                   mu.eta <- function(eta) -1/(2 * eta^1.5)
                   valideta <- function(eta) all(eta>0)
               },
               "inverse" = {
                   linkfun <- function(mu) 1/mu
                   linkinv <- function(eta) 1/eta
                   mu.eta <- function(eta) -1/(eta^2)
                   valideta <- function(eta) all(eta!=0)
               },
               ## else :
               stop(sQuote(link), " link not recognised")
               )# end switch(.)
    structure(list(linkfun = linkfun, linkinv = linkinv,
                   mu.eta = mu.eta, valideta = valideta, name = link),
              class="link-glm")
}

poisson <- function (link = "log")
{
    linktemp <- substitute(link)
    ## idea is that we can specify a character string or a name for
    ## the standard links, and if the name is not one of those, look
    ## for an object of the appropiate class.
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link (undocumented).  Deprecate in 2.4.0.
	if (linktemp == "link") {
            warning("use of poisson(link=link) is deprecated\n", domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }
    okLinks <- c("log", "identity", "sqrt")
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
            stop(gettextf('link "%s" not available for poisson family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
		 domain = NA)
        }
    }
    variance <- function(mu) mu
    validmu <- function(mu) all(mu>0)
    dev.resids <- function(y, mu, wt)
        2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))
    aic <- function(y, n, mu, wt, dev)
#	2*sum((mu-y*log(mu)+lgamma(y+1))*wt)
	-2*sum(dpois(y, mu, log=TRUE)*wt)
    initialize <- expression({
	if (any(y < 0))
	    stop("negative values not allowed for the Poisson family")
	n <- rep.int(1, nobs)
	mustart <- y + 0.1
    })
    structure(list(family = "poisson",
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

quasipoisson <- function (link = "log")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of quasipoisson(link=link) is deprecated\n",
                    domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }

    okLinks <- c("log", "identity", "sqrt")
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
	    stop(gettextf('link "%s" not available for quasipoisson family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
		 domain = NA)
        }
    }
    variance <- function(mu) mu
    validmu <- function(mu) all(mu>0)
    dev.resids <- function(y, mu, wt)
	2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))
    aic <- function(y, n, mu, wt, dev) NA
    initialize <- expression({
	if (any(y < 0))
	    stop("negative values not allowed for the quasiPoisson family")
	n <- rep.int(1, nobs)
	mustart <- y + 0.1
    })
    structure(list(family = "quasipoisson",
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
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of gaussian(link=link) is deprecated\n", domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }
    okLinks <- c("inverse", "log", "identity")
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
	    stop(gettextf('link "%s" not available for gaussian family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
		 domain = NA)
        }
    }
    structure(list(family = "gaussian",
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

binomial <- function (link = "logit")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of binomial(link=link) is deprecated\n", domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }
    okLinks <- c("logit", "probit", "cloglog", "cauchit", "log")
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
	    stop(gettextf('link "%s" not available for binomial family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
	     domain = NA)
        }
    }
    variance <- function(mu) mu * (1 - mu)
    validmu <- function(mu) all(mu>0) && all(mu<1)
    dev.resids <- function(y, mu, wt)
        .Call("binomial_dev_resids", y, mu, wt, PACKAGE="stats")
#	2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) +
#		  (1 - y) * log(ifelse(y == 1, 1, (1 - y)/(1 - mu))))
    aic <- function(y, n, mu, wt, dev) {
#	-2*sum((lchoose(n, n*y) + n*(y*log(mu) + (1-y)*log(1-mu)))*wt/n)
        m <- if(any(n > 1)) n else wt
	-2*sum(ifelse(m > 0, (wt/m), 0)*
               dbinom(round(m*y), round(m), mu, log=TRUE))
    }
    initialize <- expression({
	if (NCOL(y) == 1) {
	    ## allow factors as responses
	    ## added BDR 29/5/98
	    if (is.factor(y)) y <- y != levels(y)[1]
	    n <- rep.int(1, nobs)
	    if (any(y < 0 | y > 1))
		stop("y values must be 0 <= y <= 1")
            mustart <- (weights * y + 0.5)/(weights + 1)
            m <- weights * y
            if(any(abs(m - round(m)) > 1e-3))
                warning("non-integer #successes in a binomial glm!")
	}
	else if (NCOL(y) == 2) {
            if(any(abs(y - round(y)) > 1e-3))
                warning("non-integer counts in a binomial glm!")
	    n <- y[, 1] + y[, 2]
	    y <- ifelse(n == 0, 0, y[, 1]/n)
	    weights <- weights * n
            mustart <- (n * y + 0.5)/(n + 1)
	}
	else stop("for the binomial family, y must be a vector of 0 and 1\'s\n",
                  "or a 2 column matrix where col 1 is no. successes and col 2 is no. failures")
    })
    structure(list(family = "binomial",
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

quasibinomial <- function (link = "logit")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of quasibinomial(link=link) is deprecated\n", domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }

    okLinks <- c("logit", "probit", "cloglog", "cauchit", "log")
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
	    stop(gettextf('link "%s" not available for quasibinomial family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
	     domain = NA)
        }
    }
    variance <- function(mu) mu * (1 - mu)
    validmu <- function(mu) all(mu>0) && all(mu<1)
    dev.resids <- function(y, mu, wt)
	2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) +
		  (1 - y) * log(ifelse(y == 1, 1, (1 - y)/(1 - mu))))
    aic <- function(y, n, mu, wt, dev) NA
    initialize <- expression({
	if (NCOL(y) == 1) {
	    if (is.factor(y)) y <- y != levels(y)[1]
	    n <- rep.int(1, nobs)
	    if (any(y < 0 | y > 1))
		stop("y values must be 0 <= y <= 1")
            mustart <- (weights * y + 0.5)/(weights + 1)
	}
	else if (NCOL(y) == 2) {
	    n <- y[, 1] + y[, 2]
	    y <- ifelse(n == 0, 0, y[, 1]/n)
	    weights <- weights * n
            mustart <- (n * y + 0.5)/(n + 1)
	}
	else stop("for the quasibinomial family, y must be a vector of 0 and 1\'s\n",
                  "or a 2 column matrix where col 1 is no. successes and col 2 is no. failures")
    })
    structure(list(family = "quasibinomial",
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

Gamma <- function (link = "inverse")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of Gamma(link=link) is deprecated\n", domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }
    okLinks <- c("inverse", "log", "identity")
    if (linktemp %in% okLinks)
	stats <- make.link(linktemp)
    else if(is.character(link)) stats <- make.link(link)
    else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else {
	    stop(gettextf('link "%s" not available for gamma family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
	     domain = NA)
        }
    }
    variance <- function(mu) mu^2
    validmu <- function(mu) all(mu>0)
    dev.resids <- function(y, mu, wt)
	-2 * wt * (log(ifelse(y == 0, 1, y/mu)) - (y - mu)/mu)
    aic <- function(y, n, mu, wt, dev){
	n <- sum(wt)
	disp <- dev/n
#	2*((sum(wt*(y/mu+log(mu)-log(y)))+n*log(disp))/disp+
#	   n*lgamma(1/disp)+sum(log(y)*wt)+1)
	-2*sum(dgamma(y, 1/disp, scale=mu*disp, log=TRUE)*wt) + 2
    }
    initialize <- expression({
	if (any(y <= 0))
	    stop("non-positive values not allowed for the gamma family")
	n <- rep.int(1, nobs)
	mustart <- y
    })
    structure(list(family = "Gamma",
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

inverse.gaussian <- function(link = "1/mu^2")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) {
	linktemp <- deparse(linktemp)
        ## the idea here seems to be that we can have a character variable
        ## 'link' naming the link.
	if (linktemp == "link") {
            warning("use of inverse.gaussian(link=link) is deprecated\n",
                    domain = NA)
	    linktemp <- eval(link)
            if(!is.character(linktemp) || length(linktemp) != 1)
                stop("'link' is invalid", domain=NA)
        }
    }
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
	    stop(gettextf('link "%s" not available for inverse.gaussian family; available links are %s',
			  linktemp, paste(sQuote(okLinks), collapse =", ")),
                 domain = NA)
        }
    }
    variance <- function(mu) mu^3
    dev.resids <- function(y, mu, wt)  wt*((y - mu)^2)/(y*mu^2)
    aic <- function(y, n, mu, wt, dev)
	sum(wt)*(log(dev/sum(wt)*2*pi)+1)+3*sum(log(y)*wt)+2
    initialize <- expression({
	if(any(y <= 0))
	    stop("positive values only allowed for the inverse.gaussian family")
	n <- rep.int(1, nobs)
	mustart <- y
    })
    validmu <- function(mu) TRUE

    structure(list(family = "inverse.gaussian",
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
    vtemp <- substitute(variance)
    if (!is.character(vtemp)) vtemp <- deparse(vtemp)
    variance_nm <- vtemp
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
               dev.resids <- function(y, mu, wt)
                   2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) +
                             (1 - y) * log(ifelse(y == 1, 1, (1 - y)/(1 - mu))))
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

    if(is.na(variance_nm)) {
        if(is.character(variance))
            stop(gettextf('\'variance\' "%s" is invalid: possible values are "mu(1-mu)", "mu", "mu^2", "mu^3" and "constant"', variance_nm), domain = NA)
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
