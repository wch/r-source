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
    ans <- matrix(nrow = ns + 1, ncol = 2)
    dimnames(ans) <- list(c("<none>", scope), c("df", "AIC"))
    ans[1, ] <- extractAIC(object, scale, k = k, ...)
    n0 <- length(object$residuals)
    for(i in seq(ns)) {
	tt <- scope[i]
	if(trace > 1) cat("trying +", tt, "\n")
	nfit <- update(object, as.formula(paste("~ . +", tt)),
                       evaluate = FALSE)
        nfit <- eval.parent(nfit)
	ans[i+1, ] <- extractAIC(nfit, scale, k = k, ...)
        if(length(nfit$residuals) != n0)
            stop("number of rows in use has changed: remove missing values?")
    }
    dfs <- ans[,1] - ans[1,1]
    dfs[1] <- NA
    aod <- data.frame(Df = dfs, AIC = ans[,2])
    test <- match.arg(test)
    if(test == "Chisq") {
	dev <- ans[,2] - k*ans[, 1]
	dev <- dev[1] - dev; dev[1] <- NA
	nas <- !is.na(dev)
	P <- dev
	P[nas] <- pchisq(dev[nas], dfs[nas], lower.tail=FALSE)
	aod[, c("LRT", "Pr(Chi)")] <- list(dev, P)
    }
    head <- c("Single term additions", "\nModel:",
	      deparse(as.vector(formula(object))),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
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
	P[nnas] <- pf(Fs[nnas], df[nnas], rdf - df[nnas], lower.tail=FALSE)
	list(Fs=Fs, P=P)
    }

    if(missing(scope) || is.null(scope)) stop("no terms in scope")
    if(!is.character(scope))
	scope <- add.scope(object, update.formula(object, scope))
    if(!length(scope))
	stop("no terms in scope for adding to object")
    oTerms <- attr(object$terms, "term.labels")
    int <- attr(object$terms, "intercept")
    ns <- length(scope)
    y <- object$residuals + predict(object)
    dfs <- numeric(ns+1)
    RSS <- numeric(ns+1)
    names(dfs) <- names(RSS) <- c("<none>", scope)
    add.rhs <- paste(scope, collapse = "+")
    add.rhs <- eval(parse(text = paste("~ . +", add.rhs)))
    new.form <- update.formula(object, add.rhs)
    Terms <- terms(new.form)
    if(is.null(x)) {
	fc <- object$call
	fc$formula <- Terms
	fob <- list(call = fc)
	class(fob) <- oldClass(object)
	m <- model.frame(fob, xlev = object$xlevels)
	x <- model.matrix(Terms, m, contrasts = object$contrasts)
        oldn <- length(y)
        y <- model.response(m, "numeric")
        newn <- length(y)
        if(newn < oldn)
            warning(gettextf("using the %d/%d rows from a combined fit",
                             newn, oldn), domain = NA)
    }
    n <- nrow(x)
    Terms <- attr(Terms, "term.labels")
    asgn <- attr(x, "assign")
    ousex <- match(asgn, match(oTerms, Terms), 0) > 0
    if(int) ousex[1] <- TRUE
    iswt <- !is.null(wt <- object$weights)
    X <- x[, ousex, drop = FALSE]
    z <- if(iswt) lm.wfit(X, y, wt) else lm.fit(X, y)
    dfs[1] <- z$rank
    RSS[1] <- deviance.lm(z)
    for(tt in scope) {
	usex <- match(asgn, match(tt, Terms), 0) > 0
	X <- x[, usex|ousex, drop = FALSE]
	z <- if(iswt) lm.wfit(X, y, wt) else lm.fit(X, y)
	dfs[tt] <- z$rank
	RSS[tt] <- deviance.lm(z)
    }
    if(scale > 0) aic <- RSS/scale - n + k*dfs
    else aic <- n * log(RSS/n) + k*dfs
    dfs <- dfs - dfs[1]
    dfs[1] <- NA
    aod <- data.frame(Df = dfs, "Sum of Sq" = c(NA, RSS[1] - RSS[-1]),
		      RSS = RSS, AIC = aic,
                      row.names = names(dfs), check.names = FALSE)
    if(scale > 0) names(aod) <- c("Df", "Sum of Sq", "RSS", "Cp")
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- aod$"Sum of Sq"
        if(scale == 0) {
            dev <- n * log(RSS/n)
            dev <- dev[1] - dev
            dev[1] <- NA
        } else dev <- dev/scale
        df <- aod$Df
        nas <- !is.na(df)
        dev[nas] <- pchisq(dev[nas], df[nas], lower.tail=FALSE)
        aod[, "Pr(Chi)"] <- dev
    } else if(test == "F") {
	rdf <- object$df.resid
	aod[, c("F value", "Pr(F)")] <- Fstat(aod, aod$RSS[1], rdf)
    }
    head <- c("Single term additions", "\nModel:",
	      deparse(as.vector(formula(object))),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

add1.glm <- function(object, scope, scale = 0, test=c("none", "Chisq", "F"),
		     x = NULL, k = 2, ...)
{
    Fstat <- function(table, rdf) {
	dev <- table$Deviance
	df <- table$Df
	diff <- pmax(0, (dev[1] - dev)/df)
	Fs <- (diff/df)/(dev/(rdf-df))
	Fs[df < .Machine$double.eps] <- NA
	P <- Fs
	nnas <- !is.na(Fs)
	P[nnas] <- pf(Fs[nnas], df[nnas], rdf - df[nnas], lower.tail=FALSE)
	list(Fs=Fs, P=P)
    }
    if(!is.character(scope))
	scope <- add.scope(object, update.formula(object, scope))
    if(!length(scope))
	stop("no terms in scope for adding to object")
    oTerms <- attr(object$terms, "term.labels")
    int <- attr(object$terms, "intercept")
    ns <- length(scope)
    dfs <- dev <- numeric(ns+1)
    names(dfs) <- names(dev) <- c("<none>", scope)
    add.rhs <- paste(scope, collapse = "+")
    add.rhs <- eval(parse(text = paste("~ . +", add.rhs)))
    new.form <- update.formula(object, add.rhs)
    Terms <- terms(new.form)
    y <- object$y
    wt <- object$prior.weights
    if(is.null(x)) {
	fc <- object$call
	fc$formula <- Terms
	fob <- list(call = fc)
	class(fob) <- oldClass(object)
	m <- model.frame(fob, xlev = object$xlevels)
	x <- model.matrix(Terms, m, contrasts = object$contrasts)
        oldn <- length(y)
        y <- model.response(m, "numeric")
        ## binomial case has adjusted y.
        if(NCOL(y) == 2) y <- y[, 1]/(y[, 1] + y[,2])
        newn <- length(y)
        if(newn < oldn)
            warning(gettextf("using the %d/%d rows from a combined fit",
                             newn, oldn), domain = NA)
    }
    n <- nrow(x)
    if(is.null(wt)) wt <- rep.int(1, n)
    Terms <- attr(Terms, "term.labels")
    asgn <- attr(x, "assign")
    ousex <- match(asgn, match(oTerms, Terms), 0) > 0
    if(int) ousex[1] <- TRUE
    X <- x[, ousex, drop = FALSE]
    z <-  glm.fit(X, y, wt, offset=object$offset,
                  family=object$family, control=object$control)
    dfs[1] <- z$rank
    dev[1] <- z$deviance
    for(tt in scope) {
	usex <- match(asgn, match(tt, Terms), 0) > 0
	X <- x[, usex|ousex, drop = FALSE]
	z <-  glm.fit(X, y, wt, offset=object$offset,
		      family=object$family, control=object$control)
	dfs[tt] <- z$rank
	dev[tt] <- z$deviance
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
    aic <- aic + (extractAIC(object, k = k)[2] - aic[1])
    dfs <- dfs - dfs[1]
    dfs[1] <- NA
    aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic,
		      row.names = names(dfs), check.names = FALSE)
    if(all(is.na(aic))) aod <- aod[, -3]
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- pmax(0, loglik[1] - loglik)
        dev[1] <- NA
        LRT <- if(dispersion == 1) "LRT" else "scaled dev."
        aod[, LRT] <- dev
        nas <- !is.na(dev)
        dev[nas] <- pchisq(dev[nas], aod$Df[nas], lower.tail=FALSE)
        aod[, "Pr(Chi)"] <- dev
    } else if(test == "F") {
        if(fam == "binomial" || fam == "poisson")
            warning(gettextf("F test assumes quasi%s family", fam),
                    domain = NA)
	rdf <- object$df.residual
	aod[, c("F value", "Pr(F)")] <- Fstat(aod, rdf)
    }
    head <- c("Single term additions", "\nModel:",
	      deparse(as.vector(formula(object))),
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
    tl <- attr(object$terms, "term.labels")
    if(missing(scope)) scope <- drop.scope(object)
    else {
	if(!is.character(scope))
	    scope <- attr(terms(update.formula(object, scope)), "term.labels")
	if(!all(match(scope, tl, FALSE)))
	    stop("scope is not a subset of term labels")
    }
#    data <- model.frame(object) # remove NAs
#    object <- update(object, data = data)
    ns <- length(scope)
    ans <- matrix(nrow = ns + 1, ncol = 2)
    dimnames(ans) <- list(c("<none>", scope), c("df", "AIC"))
    ans[1, ] <- extractAIC(object, scale, k = k, ...)
    n0 <- length(object$residuals)
    for(i in seq(ns)) {
	tt <- scope[i]
	if(trace > 1) cat("trying -", tt, "\n")
	nfit <- update(object, as.formula(paste("~ . -", tt)),
                       evaluate = FALSE)
        nfit <- eval.parent(nfit)
	ans[i+1, ] <- extractAIC(nfit, scale, k = k, ...)
        if(length(nfit$residuals) != n0)
            stop("number of rows in use has changed: remove missing values?")
    }
    dfs <- ans[1,1] - ans[,1]
    dfs[1] <- NA
    aod <- data.frame(Df = dfs, AIC = ans[,2])
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- ans[, 2] - k*ans[, 1]
        dev <- dev - dev[1] ; dev[1] <- NA
        nas <- !is.na(dev)
        P <- dev
        P[nas] <- pchisq(dev[nas], dfs[nas], lower.tail = FALSE)
        aod[, c("LRT", "Pr(Chi)")] <- list(dev, P)
    }
    head <- c("Single term deletions", "\nModel:",
	      deparse(as.vector(formula(object))),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

drop1.lm <- function(object, scope, scale = 0, all.cols = TRUE,
		     test=c("none", "Chisq", "F"), k = 2, ...)
{
    x <- model.matrix(object)
    iswt <- !is.null(wt <- object$weights)
    n <- nrow(x)
    asgn <- attr(x, "assign")
    tl <- attr(object$terms, "term.labels")
    if(missing(scope)) scope <- drop.scope(object)
    else {
	if(!is.character(scope))
	    scope <- attr(terms(update.formula(object, scope)), "term.labels")
	if(!all(match(scope, tl, FALSE)))
	    stop("scope is not a subset of term labels")
    }
    ndrop <- match(scope, tl)
    ns <- length(scope)
    rdf <- object$df.resid
    chisq <- deviance.lm(object)
    dfs <- numeric(ns)
    RSS <- numeric(ns)
    y <- object$residuals + predict(object)
    na.coef <- (1:length(object$coefficients))[!is.na(object$coefficients)]
    for(i in 1:ns) {
	ii <- seq(along=asgn)[asgn == ndrop[i]]
	if(all.cols) jj <- setdiff(seq(ncol(x)), ii)
	else jj <- setdiff(na.coef, ii)
	z <- if(iswt) lm.wfit(x[, jj, drop = FALSE], y, wt)
	else lm.fit(x[, jj, drop = FALSE], y)
	dfs[i] <- z$rank
	RSS[i] <- deviance.lm(z)
    }
    scope <- c("<none>", scope)
    dfs <- c(object$rank, dfs)
    RSS <- c(chisq, RSS)
    if(scale > 0) aic <- RSS/scale - n + k*dfs
    else aic <- n * log(RSS/n) + k*dfs
    dfs <- dfs[1] - dfs
    dfs[1] <- NA
    aod <- data.frame(Df = dfs, "Sum of Sq" = c(NA, RSS[-1] - RSS[1]),
		      RSS = RSS, AIC = aic,
                      row.names = scope, check.names = FALSE)
    if(scale > 0) names(aod) <- c("Df", "Sum of Sq", "RSS", "Cp")
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- aod$"Sum of Sq"
        if(scale == 0) {
            dev <- n * log(RSS/n)
            dev <- dev - dev[1]
            dev[1] <- NA
        } else dev <- dev/scale
        df <- aod$Df
        nas <- !is.na(df)
        dev[nas] <- pchisq(dev[nas], df[nas], lower.tail=FALSE)
        aod[, "Pr(Chi)"] <- dev
    } else if(test == "F") {
	dev <- aod$"Sum of Sq"
	dfs <- aod$Df
	rdf <- object$df.resid
	rms <- aod$RSS[1]/rdf
	Fs <- (dev/dfs)/rms
	Fs[dfs < 1e-4] <- NA
	P <- Fs
	nas <- !is.na(Fs)
	P[nas] <- pf(Fs[nas], dfs[nas], rdf, lower.tail=FALSE)
	aod[, c("F value", "Pr(F)")] <- list(Fs, P)
    }
    head <- c("Single term deletions", "\nModel:",
	      deparse(as.vector(formula(object))),
	      if(scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

drop1.mlm <- function(object, scope, ...)
    stop("no 'drop1' method for \"mlm\" models")

drop1.glm <- function(object, scope, scale = 0, test=c("none", "Chisq", "F"),
		      k = 2, ...)
{
    x <- model.matrix(object)
#    iswt <- !is.null(wt <- object$weights)
    n <- nrow(x)
    asgn <- attr(x, "assign")
    tl <- attr(object$terms, "term.labels")
    if(missing(scope)) scope <- drop.scope(object)
    else {
	if(!is.character(scope))
	    scope <- attr(terms(update.formula(object, scope)), "term.labels")
	if(!all(match(scope, tl, FALSE)))
	    stop("scope is not a subset of term labels")
    }
    ndrop <- match(scope, tl)
    ns <- length(scope)
    rdf <- object$df.resid
    chisq <- object$deviance
    dfs <- numeric(ns)
    dev <- numeric(ns)
    y <- object$y
    if(is.null(y)) y <- model.response(model.frame(object), "numeric")
#    na.coef <- (1:length(object$coefficients))[!is.na(object$coefficients)]
    wt <- object$prior.weights
    if(is.null(wt)) wt <- rep.int(1, n)
    for(i in 1:ns) {
	ii <- seq(along=asgn)[asgn == ndrop[i]]
	jj <- setdiff(seq(ncol(x)), ii)
	z <-  glm.fit(x[, jj, drop = FALSE], y, wt, offset=object$offset,
		      family=object$family, control=object$control)
	dfs[i] <- z$rank
	dev[i] <- z$deviance
    }
    scope <- c("<none>", scope)
    dfs <- c(object$rank, dfs)
    dev <- c(chisq, dev)
    dispersion <- if (is.null(scale) || scale == 0)
	summary(object, dispersion = NULL)$dispersion
    else scale
    fam <- object$family$family
    loglik <-
        if(fam == "gaussian") {
            if(scale > 0) dev/scale - n else n * log(dev/n)
        } else dev/dispersion
    aic <- loglik + k * dfs
    dfs <- dfs[1] - dfs
    dfs[1] <- NA
    aic <- aic + (extractAIC(object, k = k)[2] - aic[1])
    aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic,
		      row.names = scope, check.names = FALSE)
    if(all(is.na(aic))) aod <- aod[, -3]
    test <- match.arg(test)
    if(test == "Chisq") {
        dev <- pmax(0, loglik - loglik[1])
        dev[1] <- NA
        nas <- !is.na(dev)
        LRT <- if(dispersion == 1) "LRT" else "scaled dev."
        aod[, LRT] <- dev
        dev[nas] <- pchisq(dev[nas], aod$Df[nas], lower.tail=FALSE)
        aod[, "Pr(Chi)"] <- dev
    } else if(test == "F") {
        if(fam == "binomial" || fam == "poisson")
            warning(gettextf("F test assumes quasi%s family", fam),
                    domain = NA)
	dev <- aod$Deviance
	rms <- dev[1]/rdf
        dev <- pmax(0, dev - dev[1])
	dfs <- aod$Df
	rdf <- object$df.residual
	Fs <- (dev/dfs)/rms
	Fs[dfs < 1e-4] <- NA
	P <- Fs
	nas <- !is.na(Fs)
	P[nas] <- pf(Fs[nas], dfs[nas], rdf, lower.tail=FALSE)
	aod[, c("F value", "Pr(F)")] <- list(Fs, P)
    }
    head <- c("Single term deletions", "\nModel:",
	      deparse(as.vector(formula(object))),
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
    factor.scope(attr(terms1, "factor"),
		 list(add = attr(terms2, "factor")))$add
}

drop.scope <- function(terms1, terms2)
{
    terms1 <- terms(terms1)
    f2 <- if(missing(terms2)) numeric(0)
    else attr(terms(terms2), "factor")
    factor.scope(attr(terms1, "factor"), list(drop = f2))$drop
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
	    where <- match(nmdrop, nmfac, 0)
	    if(any(!where)) stop("lower scope is not included in model")
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
    } else nmdrop <- character(0)

    if(!length(add)) nmadd <- character(0)
    else {
	nmfac <- colnames(factor)
	nmadd <- colnames(add)
	if(!is.null(nmfac)) {
	    where <- match(nmfac, nmadd, 0)
	    if(any(!where)) stop("upper scope does not include model")
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

step <- function(object, scope, scale = 0,
		 direction = c("both", "backward", "forward"),
		 trace = 1, keep = NULL, steps = 1000, k = 2, ...)
{
#     fixFormulaObject <- function(object) {
# 	tt <- terms(object)
# 	tmp <- attr(tt, "term.labels")
# 	if (!attr(tt, "intercept"))
# 	    tmp <- c(tmp, "0")
# 	if (!length(tmp))
# 	    tmp <- "1"
#         tmp <- paste("~", paste(tmp, collapse = " + "))
#         form <- formula(object) # some formulae have no lhs
#         tmp <- if(length(form) > 2) paste(deparse(form[[2]]), tmp)
#         ## must be as.character as deparse gives spurious ()
# 	if (length(offset <- attr(tt, "offset")))
# 	    tmp <- paste(tmp, as.character(attr(tt, "variables")[offset + 1]),
# 			 sep = " + ")
# 	form <- formula(tmp)
#         environment(form) <- environment(tt)
#         form
#     }
    mydeviance <- function(x, ...)
    {
        dev <- deviance(x)
        if(!is.null(dev)) dev else extractAIC(x, k=0)[2]
    }

    cut.string <- function(string)
    {
	if(length(string) > 1)
	    string[-1] <- paste("\n", string[-1], sep = "")
	string
    }
    re.arrange <- function(keep)
    {
	namr <- names(k1 <- keep[[1]])
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
		     "\nInitial Model:", deparse(as.vector(formula(object))),
		     "\nFinal Model:", deparse(as.vector(formula(fit))),
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
	fdrop <- numeric(0)
        fadd <- attr(Terms, "factors")
        if(md) forward <- FALSE
    }
    else {
	if(is.list(scope)) {
	    fdrop <- if(!is.null(fdrop <- scope$lower))
		attr(terms(update.formula(object, fdrop)), "factors")
	    else numeric(0)
	    fadd <- if(!is.null(fadd <- scope$upper))
		attr(terms(update.formula(object, fadd)), "factors")
	}
        else {
	    fadd <- if(!is.null(fadd <- scope))
		attr(terms(update.formula(object, scope)), "factors")
	    fdrop <- numeric(0)
	}
    }
    models <- vector("list", steps)
    if(!is.null(keep)) keep.list <- vector("list", steps)
    n <- length(object$residuals)
    fit <- object
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1]
    bAIC <- bAIC[2]
    if(is.na(bAIC))
        stop("AIC is not defined for this model, so 'step' cannot proceed")
    nm <- 1
    Terms <- fit$terms
    if(trace)
	cat("Start:  AIC=", format(round(bAIC, 2)), "\n",
	    cut.string(deparse(as.vector(formula(fit)))), "\n\n")

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
	    row.names(aod) <- c(rn[1], paste("-", rn[-1], sep=" "))
            ## drop zero df terms first: one at time since they
            ## may mask each other
	    if(any(aod$Df == 0, na.rm=TRUE)) {
		zdf <- aod$Df == 0 & !is.na(aod$Df)
		change <- rev(rownames(aod)[zdf])[1]
	    }
	}
	if(is.null(change)) {
	    if(forward && length(scope$add)) {
		aodf <- add1(fit, scope$add, scale = scale,
                             trace = trace, k = k, ...)
		rn <- row.names(aodf)
		row.names(aodf) <- c(rn[1], paste("+", rn[-1], sep=" "))
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
	    nc <- nc[!is.na(nc)][1]
	    o <- order(aod[, nc])
	    if(trace) print(aod[o, ])
	    if(o[1] == 1) break
	    change <- rownames(aod)[o[1]]
	}
	usingCp <- match("Cp", names(aod), 0) > 0
        ## may need to look for a `data' argument in parent
	fit <- update(fit, paste("~ .", change), evaluate = FALSE)
        fit <- eval.parent(fit)
        if(length(fit$residuals) != n)
            stop("number of rows in use has changed: remove missing values?")
        Terms <- terms(fit)
	bAIC <- extractAIC(fit, scale, k = k, ...)
	edf <- bAIC[1]
	bAIC <- bAIC[2]
	if(trace)
	    cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n",
		cut.string(deparse(as.vector(formula(fit)))), "\n\n")
        ## add a tolerance as dropping 0-df terms might increase AIC slightly
	if(bAIC >= AIC + 1e-7) break
	nm <- nm + 1
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
    ## seems that coxph sometimes gives one and sometimes gives two values
    ## for loglik
    edf <- length(fit$coef)
    loglik <- fit$loglik[length(fit$loglik)]
    c(edf, -2 * loglik + k * edf)
}

extractAIC.survreg <- function(fit, scale, k = 2, ...)
{
    edf <- sum(fit$df)
    c(edf, -2 * fit$loglik[2] + k * edf)
}

extractAIC.glm <- function(fit, scale = 0, k = 2, ...)
{
    n <- length(fit$residuals)
    edf <- n  - fit$df.residual
    aic <- fit$aic
    c(edf, aic + (k-2) * edf)
}

extractAIC.lm <- function(fit, scale = 0, k = 2, ...)
{
    n <- length(fit$residuals)
    edf <- n  - fit$df.residual
    RSS <- deviance.lm(fit)
    dev <- if(scale > 0) RSS/scale - n else n * log(RSS/n)
    c(edf, dev + k * edf)
}
extractAIC.aov <- extractAIC.lm

extractAIC.negbin <- function(fit, scale, k = 2, ...)
{
    n <- length(fit$residuals)
    edf <- n - fit$df.residual
    c(edf, -fit$twologlik + k * edf)
}
