#  File src/library/stats/R/nls.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1999-1999 Saikat DebRoy, Douglas M. Bates, Jose C. Pinheiro
#  Copyright (C) 2000-2013 The R Core Team
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

###
###            Nonlinear least squares for R
###

numericDeriv <- function(expr, theta, rho = parent.frame(), dir=1.0)
{
    dir <- rep_len(dir, length(theta))
    val <- .Call(C_numeric_deriv, expr, theta, rho, dir)
    valDim <- dim(val)
    if (!is.null(valDim)) {
        if (valDim[length(valDim)] == 1)
            valDim <- valDim[-length(valDim)]
        if(length(valDim) > 1L)
            dim(attr(val, "gradient")) <- c(valDim,
                                            dim(attr(val, "gradient"))[-1L])
    }
    val
}

nlsModel.plinear <- function(form, data, start, wts)
{
    thisEnv <- environment()
    env <- new.env(hash = TRUE, parent=environment(form))
    for(i in names(data)) assign(i, data[[i]], envir = env)
    ind <- as.list(start)
    p2 <- 0
    for(i in names(ind)) {
        temp <- start[[i]]
        storage.mode(temp) <- "double"
        assign(i, temp, envir = env)
        ind[[i]] <- p2 + seq_along(start[[i]])
        p2 <- p2 + length(start[[i]])
    }
    lhs <- eval(form[[2L]], envir = env)
    storage.mode(lhs) <- "double"
    rhs <- eval(form[[3L]], envir = env)
    storage.mode(rhs) <- "double"
    .swts <- if(!missing(wts) && length(wts))
        sqrt(wts) else rep_len(1, NROW(rhs))
    assign(".swts", .swts, envir = env)
    p1 <- if(is.matrix(rhs)) ncol(rhs) else 1
    p <- p1 + p2
    n <- length(lhs)
    fac <- (n -  p)/p
    cc <- QR.B <- NA
    useParams <- rep_len(TRUE, p2)
    if(is.null(attr(rhs, "gradient"))) {
        getRHS.noVarying <- function()
            numericDeriv(form[[3L]], names(ind), env)
        getRHS <- getRHS.noVarying
        rhs <- getRHS()
    } else {
        getRHS.noVarying <- function() eval(form[[3L]], envir = env)
        getRHS <- getRHS.noVarying
    }
    dimGrad <- dim(attr(rhs, "gradient"))
    marg <- length(dimGrad)
    if(marg > 0) {
        gradSetArgs <- vector("list", marg + 1L)
        for(i in 2:marg)
            gradSetArgs[[i]] <- rep_len(TRUE, dimGrad[i-1])
        useParams <- rep_len(TRUE, dimGrad[marg])
    } else {
        gradSetArgs <- vector("list", 2L)
        useParams <- rep_len(TRUE, length(attr(rhs, "gradient")))
    }
    gradSetArgs[[1L]] <- (~attr(ans, "gradient"))[[2L]]
    gradCall <-
        switch(length(gradSetArgs) - 1L,
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]]),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]]),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]],
                    gradSetArgs[[3L]]),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]],
                    gradSetArgs[[3L]], gradSetArgs[[4L]]))
    getRHS.varying <- function()
    {
        ans <- getRHS.noVarying()
        attr(ans, "gradient") <- eval(gradCall)
        ans
    }
    QR.rhs <- qr(.swts * rhs)
    lin <- qr.coef(QR.rhs, .swts * lhs)
    resid <- qr.resid(QR.rhs, .swts * lhs)
    topzero <- double(p1)
    dev <- sum(resid^2)
    if(marg <= 1) {
        ddot <- function(A, b) A %*% b
        dtdot <- function(A, b) t(A) %*% b
    } else if(marg == 2) {
        if(p1 == 1) {
            ddot <- function(A, b) as.matrix(A*b)
            dtdot <- function(A, b) t(b) %*% A
        } else if(p2 == 1) {
            ddot <- function(A, b) A %*% b
            dtdot <- function(A, b) t(A) %*% b
        }
    } else {
        ddot <- function(A, b) apply(A, MARGIN = 3L, FUN="%*%", b)
        dtdot <- function(A, b) apply(A, MARGIN = c(2L,3L), FUN = "%*%", b)
    }

    getPars.noVarying <- function()
        unlist(setNames(lapply(names(ind), get, envir = env), names(ind)))
    getPars.varying <- function()
        unlist(setNames(lapply(names(ind), get, envir = env),
                        names(ind)))[useParams]
    getPars <- getPars.noVarying

    internalPars <- getPars()
    setPars.noVarying <- function(newPars)
    {
        assign("internalPars", newPars, envir = thisEnv)
        for(i in names(ind)) {
            assign(i, unname(newPars[ ind[[i]] ]), envir = env )
        }
    }
    setPars.varying <- function(newPars)
    {
        internalPars[useParams] <- newPars
        for(i in names(ind))
            assign(i, unname(internalPars[ ind[[i]] ]), envir = env)
    }
    setPars <- setPars.noVarying
    getPred <-
        if(is.matrix(rhs)) function(X) as.numeric(X %*% lin)
        else function(X) X * lin

    m <-
        list(resid = function() resid,
             fitted = function() getPred(rhs),
             formula = function() form,
             deviance = function() dev,
             lhs = function() lhs,
             gradient = function() attr(rhs, "gradient"),
             conv = function() {
                 assign("cc", c(topzero, qr.qty(QR.rhs, .swts * lhs)[ -(1L:p1)]),
                        envir = thisEnv)
                 rr <- qr.qy(QR.rhs, cc)
                 B <- qr.qty(QR.rhs, .swts * ddot(attr(rhs, "gradient"), lin))
                 B[1L:p1, ] <- dtdot(.swts * attr(rhs, "gradient"), rr)
                 R <- t( qr.R(QR.rhs)[1L:p1, ] )
                 if(p1 == 1) B[1, ] <- B[1, ]/R
                 else B[1L:p1, ] <- forwardsolve(R, B[1L:p1, ])
                 assign("QR.B", qr(B), envir = thisEnv)
                 rr <- qr.qty(QR.B, cc)
                 sqrt( fac*sum(rr[1L:p1]^2) / sum(rr[-(1L:p1)]^2) )
             },
             incr = function() qr.solve(QR.B, cc),
             setVarying = function(vary = rep_len(TRUE, length(useParams))) {
                 assign("useParams", if(is.character(vary)) {
                     temp <- logical(length(useParams))
                     temp[unlist(ind[vary])] <- TRUE
                     temp
                 } else if(is.logical(vary) && length(vary) != length(useParams))
                        stop("setVarying : 'vary' length must match length of parameters")
                 else {
                     vary
                 }, envir = thisEnv)
                 gradCall[[length(gradCall)]] <<- useParams
                 if(all(useParams)) {
                     assign("setPars", setPars.noVarying, envir = thisEnv)
                     assign("getPars", getPars.noVarying, envir = thisEnv)
                     assign("getRHS", getRHS.noVarying, envir = thisEnv)
                 } else {
                     assign("setPars", setPars.varying, envir = thisEnv)
                     assign("getPars", getPars.varying, envir = thisEnv)
                     assign("getRHS", getRHS.varying, envir = thisEnv)
                 }
             },
             setPars = function(newPars) {
                 setPars(newPars)
                 assign("QR.rhs",
                        qr(.swts * assign("rhs", getRHS(), envir = thisEnv)),
                        envir = thisEnv)
                 assign("resid", qr.resid(QR.rhs, .swts * lhs),
                        envir = thisEnv)
                 assign("dev", sum(resid^2), envir = thisEnv )
                 if(QR.rhs$rank < p1) {
                     return(1)
                 } else {
                     assign("lin", qr.coef(QR.rhs, .swts * lhs),
                            envir = thisEnv)
                     return(0)
                 }
             },
             getPars = function() getPars(),
             getAllPars = function() c( getPars(), c( .lin = lin ) ),
             getEnv = function() env,
             trace = function() {
                 cat(format(dev),": ", format(c(getPars(), lin)))
                 cat("\n")
             },
             Rmat = function()
             qr.R(qr(.swts * cbind(ddot(attr(rhs, "gradient"), lin), rhs))),
             predict = function(newdata = list(), qr = FALSE)
             getPred(eval(form[[3L]], as.list(newdata), env))
             )
    class(m) <- c("nlsModel.plinear", "nlsModel")
    m$conv()
    on.exit( remove( data, i, m, marg, n, p, start, temp, gradSetArgs) )
    m
}

nlsModel <- function(form, data, start, wts, upper=NULL)
{
    thisEnv <- environment()
    env <- new.env(hash = TRUE, parent = environment(form))
    for(i in names(data)) assign(i, data[[i]], envir = env)
    ind <- as.list(start)
    parLength <- 0
    for(i in names(ind) ) {
        temp <- start[[i]]
        storage.mode(temp) <- "double"
        assign(i, temp, envir = env)
        ind[[i]] <- parLength + seq_along(start[[i]])
        parLength <- parLength + length(start[[i]])
    }
    getPars.noVarying <- function()
        unlist(setNames(lapply(names(ind), get, envir = env), names(ind)))
    getPars <- getPars.noVarying
    internalPars <- getPars()

    if(!is.null(upper)) upper <- rep_len(upper, parLength)
    useParams <- rep_len(TRUE, parLength)
    lhs <- eval(form[[2L]], envir = env)
    rhs <- eval(form[[3L]], envir = env)
    .swts <- if(!missing(wts) && length(wts))
        sqrt(wts) else rep_len(1, length(rhs))
    assign(".swts", .swts, envir = env)
    resid <- .swts * (lhs - rhs)
    dev <- sum(resid^2)
    if(is.null(attr(rhs, "gradient"))) {
        getRHS.noVarying <- function() {
            if(is.null(upper))
                numericDeriv(form[[3L]], names(ind), env)
            else
                numericDeriv(form[[3L]], names(ind), env,
                             ifelse(internalPars < upper, 1, -1))
        }
        getRHS <- getRHS.noVarying
        rhs <- getRHS()
    } else {
        getRHS.noVarying <- function() eval(form[[3L]], envir = env)
        getRHS <- getRHS.noVarying
    }
    dimGrad <- dim(attr(rhs, "gradient"))
    marg <- length(dimGrad)
    if(marg > 0L) {
        gradSetArgs <- vector("list", marg + 1L)
        for(i in 2L:marg)
            gradSetArgs[[i]] <- rep_len(TRUE, dimGrad[i-1])
        useParams <- rep_len(TRUE, dimGrad[marg])
    } else {
        gradSetArgs <- vector("list", 2L)
        useParams <- rep_len(TRUE, length(attr(rhs, "gradient")))
    }
    npar <- length(useParams)
    gradSetArgs[[1L]] <- (~attr(ans, "gradient"))[[2L]]
    gradCall <-
        switch(length(gradSetArgs) - 1L,
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], drop = FALSE),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]],
                    drop = FALSE),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]],
                    gradSetArgs[[3L]], drop = FALSE),
               call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]],
                    gradSetArgs[[3L]], gradSetArgs[[4L]], drop = FALSE))
    getRHS.varying <- function()
    {
        ans <- getRHS.noVarying()
        attr(ans, "gradient") <- eval(gradCall)
        ans
    }
    QR <- qr(.swts * attr(rhs, "gradient"))
    qrDim <- min(dim(QR$qr))
    if(QR$rank < qrDim)
        stop("singular gradient matrix at initial parameter estimates")

    getPars.varying <- function()
        unlist(setNames(lapply(names(ind), get, envir = env),
                        names(ind)))[useParams]
    setPars.noVarying <- function(newPars)
    {
        assign("internalPars", newPars, envir = thisEnv)
        for(i in names(ind))
            assign(i, unname(newPars[ ind[[i]] ]), envir = env)
    }
    setPars.varying <- function(newPars)
    {
        internalPars[useParams] <- newPars
        for(i in names(ind))
            assign(i, unname(internalPars[ ind[[i]] ]), envir = env)
    }
    setPars <- setPars.noVarying

    on.exit(remove(i, data, parLength, start, temp, m))
    ## must use weighted resid for use with "port" algorithm.
    m <-
	list(resid = function() resid,
	     fitted = function() rhs,
	     formula = function() form,
	     deviance = function() dev,
	     lhs = function() lhs,
	     gradient = function() .swts * attr(rhs, "gradient"),
	     conv = function() {
		 if(npar == 0) return(0)
		 rr <- qr.qty(QR, resid) # rotated residual vector
		 sqrt( sum(rr[1L:npar]^2) / sum(rr[-(1L:npar)]^2))
	     },
	     incr = function() qr.coef(QR, resid),
	     setVarying = function(vary = rep_len(TRUE, length(useParams))) {
		 assign("useParams",
			if(is.character(vary)) {
			    temp <- logical(length(useParams))
			    temp[unlist(ind[vary])] <- TRUE
			    temp
			} else if(is.logical(vary) &&
				  length(vary) != length(useParams))
			stop("setVarying : 'vary' length must match length of parameters")
			else {
			    vary
			}, envir = thisEnv)
		 gradCall[[length(gradCall) - 1L]] <<- useParams
		 if(all(useParams)) {
		     assign("setPars", setPars.noVarying, envir = thisEnv)
		     assign("getPars", getPars.noVarying, envir = thisEnv)
		     assign("getRHS", getRHS.noVarying, envir = thisEnv)
		     assign("npar", length(useParams), envir = thisEnv)
		 } else {
		     assign("setPars", setPars.varying, envir = thisEnv)
		     assign("getPars", getPars.varying, envir = thisEnv)
		     assign("getRHS", getRHS.varying, envir = thisEnv)
                     ## FIXME this is which(useParams)
		     assign("npar", length(seq_along(useParams)[useParams]),
			    envir = thisEnv)
		 }
	     },
	     setPars = function(newPars) {
		 setPars(newPars)
		 assign("resid", .swts *
			(lhs - assign("rhs", getRHS(), envir = thisEnv)),
			envir = thisEnv)
		 assign("dev", sum(resid^2), envir = thisEnv)
		 assign("QR", qr(.swts * attr(rhs, "gradient")),
			envir = thisEnv )
		 return(QR$rank < min(dim(QR$qr))) # to catch the singular gradient matrix
	     },
	     getPars = function() getPars(),
	     getAllPars = function() getPars(),
	     getEnv = function() env,
	     trace = function() {
                 cat(format(dev),": ", format(getPars()))
                 cat("\n")
             },
	     Rmat = function() qr.R(QR),
	     predict = function(newdata = list(), qr = FALSE)
	     eval(form[[3L]], as.list(newdata), env)
	     )

    class(m) <- "nlsModel"
    m
}

nls.control <- function(maxiter = 50, tol = 0.00001, minFactor = 1/1024,
			printEval = FALSE, warnOnly = FALSE)
    list(maxiter = maxiter, tol = tol, minFactor = minFactor,
	 printEval = printEval, warnOnly = warnOnly)

nls_port_fit <- function(m, start, lower, upper, control, trace, give.v=FALSE)
{
    ## Establish the working vectors and check and set options
    p <- length(par <- as.double(unlist(start)))
    iv <- integer(4L*p + 82L)
    v <- double(105L + (p * (2L * p + 20L)))
    .Call(C_port_ivset, 1, iv, v)
    if (length(control)) {
	if (!is.list(control) || is.null(nms <- names(control)))
	    stop("'control' argument must be a named list")
	## remove those components that do not apply here
	for(noN in intersect(nms, c("tol", "minFactor", "warnOnly", "printEval")))
	    control[[noN]] <- NULL
	nms <- names(control)
	pos <- pmatch(nms, names(port_cpos))
	if (any(nap <- is.na(pos))) {
            warning(sprintf(ngettext(length(nap),
                                     "unrecognized control element named %s ignored",
                                     "unrecognized control elements named %s ignored"),
                            paste(nms[nap], collapse = ", ")),
                    domain = NA)
	    pos <- pos[!nap]
	    control <- control[!nap]
	}
	ivpars <- pos <= 4 ; vpars <- !ivpars
	if (any(ivpars))
	    iv[port_cpos[pos[ivpars]]] <- as.integer(unlist(control[ivpars]))
	if (any(vpars))
	    v [port_cpos[pos[ vpars]]] <- as.double(unlist(control[vpars]))
    }
    if (trace)
        iv[port_cpos[["trace"]]] <- 1L
    scale <- 1
    low <- upp <- NULL
    if (any(lower != -Inf) || any(upper != Inf)) {
        low <- rep_len(as.double(lower), length(par))
        upp <- rep_len(as.double(upper), length(par))
        if(any(unlist(start) < low) ||any( unlist(start) > upp)) {
            iv[1L] <- 300
	    return(if(give.v) list(iv = iv, v = v[seq_len(18L)]) else iv)
        }
    }
    if(p > 0) {
        ## driver routine port_nlsb() in ../src/port.c -- modifies m & iv
        .Call(C_port_nlsb, m,
              d = rep_len(as.double(scale), length(par)),
              df = m$gradient(), iv, v, low, upp)
    } else iv[1L] <- 6

    if(give.v)## also want v[] e.g., for attained precision
        ## v[1:18] --> ../src/portsrc.f
        list(iv = iv, v = v[seq_len(18L)]) else iv
}

nls <-
  function (formula, data = parent.frame(), start, control = nls.control(),
            algorithm = c("default", "plinear", "port"), trace = FALSE,
            subset, weights, na.action, model = FALSE,
            lower = -Inf, upper = Inf, ...)
{
    ## canonicalize the arguments
    formula <- as.formula(formula)
    algorithm <- match.arg(algorithm)

    if(!is.list(data) && !is.environment(data))
        stop("'data' must be a list or an environment")

    mf <- cl <- match.call()		# for creating the model frame
    varNames <- all.vars(formula) # parameter and variable names from formula
    ## adjust a one-sided model formula by using 0 as the response
    if (length(formula) == 2L) {
        formula[[3L]] <- formula[[2L]]
        formula[[2L]] <- 0
    }
    ## for prediction we will need to know those which are in RHS
    form2 <- formula; form2[[2L]] <- 0
    varNamesRHS <- all.vars(form2)
    mWeights <- missing(weights)

    ## get names of the parameters from the starting values or selfStart model
    pnames <-
	if (missing(start)) {
	    if(!is.null(attr(data, "parameters"))) {
		names(attr(data, "parameters"))
	    } else { ## try selfStart - like object
		cll <- formula[[length(formula)]]
		fn <- as.character(cll[[1L]])
		if(is.null(func <- tryCatch(get(fn), error=function(e)NULL)))
		    func <- get(fn, envir=parent.frame()) ## trying "above"
		if(!is.null(pn <- attr(func, "pnames")))
		    as.character(as.list(match.call(func, call = cll))[-1L][pn])
	    }
	} else
	    names(start)

    env <- environment(formula)
    if (is.null(env)) env <- parent.frame()

    ## Heuristics for determining which names in formula represent actual
    ## variables :

    ## If it is a parameter it is not a variable (nothing to guess here :-)
    if(length(pnames))
        varNames <- varNames[is.na(match(varNames, pnames))]

    ## This aux.function needs to be as complicated because
    ## exists(var, data) does not work (with lists or dataframes):
    lenVar <- function(var) tryCatch(length(eval(as.name(var), data, env)),
				     error = function(e) -1L)

    if(length(varNames)) {
        n <- vapply(varNames, lenVar, 0)
        if(any(not.there <- n == -1L)) {
            nnn <- names(n[not.there])
            if(missing(start)) {
                if(algorithm == "plinear")
                    ## TODO: only specify values for the non-lin. parameters
                    stop("no starting values specified")
                ## Provide some starting values instead of erroring out later;
                ## '1' seems slightly better than 0 (which is often invalid):
                warning("No starting values specified for some parameters.\n",
                        "Initializing ", paste(sQuote(nnn), collapse=", "),
                        " to '1.'.\n",
                        "Consider specifying 'start' or using a selfStart model", domain = NA)
		start <- setNames(as.list(rep_len(1., length(nnn))), nnn)
                varNames <- varNames[i <- is.na(match(varNames, nnn))]
                n <- n[i]
            }
            else                        # has 'start' but forgot some
                stop(gettextf("parameters without starting value in 'data': %s",
                              paste(nnn, collapse=", ")), domain = NA)
        }
    }
    else { ## length(varNames) == 0
	if(length(pnames) && any((np <- sapply(pnames, lenVar)) == -1)) {
            ## Can fit a model with pnames even if no varNames
            message(sprintf(ngettext(sum(np == -1),
                                     "fitting parameter %s without any variables",
                                     "fitting parameters %s without any variables"),
                            paste(sQuote(pnames[np == -1]), collapse=", ")),
                    domain = NA)
            n <- integer()
        }
	else
	    stop("no parameters to fit")
    }

    ## If its length is a multiple of the response or LHS of the formula,
    ## then it is probably a variable.
    ## This may fail (e.g. when LHS contains parameters):
    respLength <- length(eval(formula[[2L]], data, env))

    if(length(n) > 0L) {
	varIndex <- n %% respLength == 0
	if(is.list(data) && diff(range(n[names(n) %in% names(data)])) > 0) {
	    ## 'data' is a list that can not be coerced to a data.frame
	    mf <- data
            if(!missing(subset))
                warning("argument 'subset' will be ignored")
            if(!missing(na.action))
                warning("argument 'na.action' will be ignored")
	    if(missing(start))
		start <- getInitial(formula, mf)
	    startEnv <- new.env(hash = FALSE, parent = environment(formula)) # small
	    for (i in names(start))
		assign(i, start[[i]], envir = startEnv)
	    rhs <- eval(formula[[3L]], data, startEnv)
	    n <- NROW(rhs)
            ## mimic what model.frame.default does
            wts <- if (mWeights) rep_len(1, n) else
            eval(substitute(weights), data, environment(formula))
	}
        else {
            mf$formula <-  # replace by one-sided linear model formula
                as.formula(paste("~", paste(varNames[varIndex], collapse = "+")),
                           env = environment(formula))
            mf$start <- mf$control <- mf$algorithm <- mf$trace <- mf$model <- NULL
            mf$lower <- mf$upper <- NULL
            mf[[1L]] <- quote(stats::model.frame)
            mf <- eval.parent(mf)
            n <- nrow(mf)
            mf <- as.list(mf)
            wts <- if (!mWeights) model.weights(mf) else rep_len(1, n)
        }
        if (any(wts < 0 | is.na(wts)))
            stop("missing or negative weights not allowed")
    }
    else {
        ## length(n) == 0 : Some problems might have no official varNames
        ##                  but still parameters to fit
        varIndex <- logical()
        mf <- list(0)
        wts <- numeric()
    }

    ## set up iteration
    if (missing(start)) start <- getInitial(formula, mf)
    for(var in varNames[!varIndex])
        mf[[var]] <- eval(as.name(var), data, env)
    varNamesRHS <- varNamesRHS[ varNamesRHS %in% varNames[varIndex] ]

    m <- switch(algorithm,
		plinear = nlsModel.plinear(formula, mf, start, wts),
		port    = nlsModel        (formula, mf, start, wts, upper),
                ## Default:
		nlsModel(formula, mf, start, wts))

    ctrl <- nls.control()
    if(!missing(control)) {
	control <- as.list(control)
	ctrl[names(control)] <- control
    }
    ## Iterate
    if (algorithm != "port") {
	if (!identical(lower, -Inf) || !identical(upper, +Inf))
	    warning('upper and lower bounds ignored unless algorithm = "port"')
        convInfo <- .Call(C_nls_iter, m, ctrl, trace)
	nls.out <- list(m = m, convInfo = convInfo,
			data = substitute(data), call = cl)
    }
    else { ## "port" i.e., PORT algorithm
	pfit <- nls_port_fit(m, start, lower, upper, control, trace,
			     give.v=TRUE)
        iv <- pfit[["iv"]]
	msg.nls <- port_msg(iv[1L])
	conv <- (iv[1L] %in% 3:6)
	if (!conv) {
	    msg <- paste("Convergence failure:", msg.nls)
	    if(ctrl$warnOnly) warning(msg) else stop(msg)
	}
	v. <- port_get_named_v(pfit[["v"]])
	## return a 'convInfo' list compatible to the non-PORT case:
	cInfo <- list(isConv = conv,
		      finIter = iv[31L], # 31: NITER
		      finTol  =	 v.[["NREDUC"]],
		      nEval = c("function" = iv[6L], "gradient" = iv[30L]),
		      stopCode = iv[1L],
		      stopMessage = msg.nls)
        ## we need these (evaluated) for profiling
	cl$lower <- lower
	cl$upper <- upper
	nls.out <- list(m = m, data = substitute(data),
                        call = cl, convInfo = cInfo,
                        ## UGLY: this is really a logical for  *NON*convergence:
                        ## deprecate these two, as they are now part of convInfo
			convergence = as.integer(!conv),
			message = msg.nls)
    }

    ## we need these (evaluated) for profiling
    nls.out$call$algorithm <- algorithm
    nls.out$call$control <- ctrl
    nls.out$call$trace <- trace

    nls.out$na.action <- attr(mf, "na.action")
    nls.out$dataClasses <-
        attr(attr(mf, "terms"), "dataClasses")[varNamesRHS]
    if(model)
	nls.out$model <- mf
    if(!mWeights)
	nls.out$weights <- wts
    nls.out$control <- control
    class(nls.out) <- "nls"
    nls.out
}

coef.nls <- function(object, ...) object$m$getAllPars()

summary.nls <-
    function (object, correlation = FALSE, symbolic.cor = FALSE, ...)
{
    r <- as.vector(object$m$resid()) # These are weighted residuals.
    w <- object$weights
    n <- if (!is.null(w)) sum(w > 0) else length(r)
    param <- coef(object)
    pnames <- names(param)
    p <- length(param)
    rdf <- n - p
    resvar <- if(rdf <= 0) NaN else deviance(object)/rdf
    XtXinv <- chol2inv(object$m$Rmat())
    dimnames(XtXinv) <- list(pnames, pnames)
    se <- sqrt(diag(XtXinv) * resvar)
    tval <- param/se
    param <- cbind(param, se, tval, 2 * pt(abs(tval), rdf, lower.tail = FALSE))
    dimnames(param) <-
        list(pnames, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    ans <- list(formula = formula(object), residuals = r, sigma = sqrt(resvar),
                df = c(p, rdf), cov.unscaled = XtXinv,
                call = object$call,
                convInfo = object$convInfo,
                control = object$control,
                na.action = object$na.action,
                coefficients = param,
                parameters = param)# never documented, for back-compatibility
    if(correlation && rdf > 0) {
        ans$correlation <- (XtXinv * resvar)/outer(se, se)
        ans$symbolic.cor <- symbolic.cor
    }
    ## if(identical(object$call$algorithm, "port"))
    ##     ans$message <- object$message
    class(ans) <- "summary.nls"
    ans
}

.p.nls.convInfo <- function(x, digits,
			    show. = getOption("show.nls.convergence", TRUE))
{
    if(!is.null(x$convInfo)) # older fits will not have this
        with(x$convInfo,
         {
             if(identical(x$call$algorithm, "port"))
                 cat("\nAlgorithm \"port\", convergence message: ",
                     stopMessage, "\n", sep = "")
             else {
                 if(!isConv || show.) {
                     cat("\nNumber of iterations",
                         if(isConv) "to convergence:" else "till stop:", finIter,
                         "\nAchieved convergence tolerance:",
                         format(finTol, digits = digits))
                     cat("\n")
                 }
                 if(!isConv) {
                     cat("Reason stopped:", stopMessage)
                     cat("\n")
                 }
             }
         })

    invisible()
}

print.nls <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    cat("Nonlinear regression model\n")
    cat("  model: ", deparse(formula(x)), "\n", sep = "")
    cat("   data: ", deparse(x$data), "\n", sep = "")
    print(x$m$getAllPars(), digits = digits, ...)
    cat(" ", if(!is.null(x$weights) && diff(range(x$weights))) "weighted ",
	"residual sum-of-squares: ", format(x$m$deviance(), digits = digits),
	"\n", sep = "")
    .p.nls.convInfo(x, digits = digits)
    invisible(x)
}

print.summary.nls <-
  function (x, digits = max(3L, getOption("digits") - 3L),
            symbolic.cor = x$symbolic.cor,
            signif.stars = getOption("show.signif.stars"), ...)
{
    cat("\nFormula: ",
	paste(deparse(x$formula), sep = "\n", collapse = "\n"),
        "\n", sep = "")
    df <- x$df
    rdf <- df[2L]
    cat("\nParameters:\n")
    printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars,
                 ...)
    cat("\nResidual standard error:",
        format(signif(x$sigma, digits)), "on", rdf, "degrees of freedom")
    cat("\n")
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1) {
            cat("\nCorrelation of Parameter Estimates:\n")
	    if(is.logical(symbolic.cor) && symbolic.cor) {
		print(symnum(correl, abbr.colnames = NULL))
            } else {
                correl <- format(round(correl, 2), nsmall = 2L, digits = digits)
                correl[!lower.tri(correl)] <- ""
                print(correl[-1, -p, drop=FALSE], quote = FALSE)
            }
        }
    }

    .p.nls.convInfo(x, digits = digits)

    if(nzchar(mess <- naprint(x$na.action))) cat("  (", mess, ")\n", sep = "")
    cat("\n")
    invisible(x)
}

weights.nls <- function(object, ...) object$weights

predict.nls <-
  function(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
           interval = c("none", "confidence", "prediction"), level = 0.95,
           ...)
{
    if (missing(newdata)) return(as.vector(fitted(object)))
    if(!is.null(cl <- object$dataClasses)) .checkMFClasses(cl, newdata)
    object$m$predict(newdata)
}

fitted.nls <- function(object, ...)
{
    val <- as.vector(object$m$fitted())
    if(!is.null(object$na.action)) val <- napredict(object$na.action, val)
    lab <- "Fitted values"
    if (!is.null(aux <- attr(object, "units")$y)) lab <- paste(lab, aux)
    attr(val, "label") <- lab
    val
}

formula.nls <- function(x, ...) x$m$formula()

residuals.nls <- function(object, type = c("response", "pearson"), ...)
{
    type <- match.arg(type)
    if (type == "pearson") {
        val <- as.vector(object$m$resid())
        std <- sqrt(sum(val^2)/(length(val) - length(coef(object))))
        val <- val/std
        if(!is.null(object$na.action)) val <- naresid(object$na.action, val)
        attr(val, "label") <- "Standardized residuals"
    } else {
        val <- as.vector(object$m$lhs() - object$m$fitted())
        if(!is.null(object$na.action))
            val <- naresid(object$na.action, val)
        lab <- "Residuals"
        if (!is.null(aux <- attr(object, "units")$y)) lab <- paste(lab, aux)
        attr(val, "label") <- lab
    }
    val
}

logLik.nls <- function(object, REML = FALSE, ...)
{
    if (REML)
        stop("cannot calculate REML log-likelihood for \"nls\" objects")
    res <- object$m$resid()
    N <- length(res)
    if(is.null(w <- object$weights)) w <- rep_len(1, N)
    ## Note the trick for zero weights
    zw <- w == 0
    val <-  -N * (log(2 * pi) + 1 - log(N) - sum(log(w + zw)) + log(sum(w*res^2)))/2
    ## the formula here corresponds to estimating sigma^2.
    attr(val, "df") <- 1L + length(coef(object))
    attr(val, "nobs") <- attr(val, "nall") <- sum(!zw)
    class(val) <- "logLik"
    val
}

df.residual.nls <- function(object, ...)
{
    w <- object$weights
    n <- if(!is.null(w)) sum(w != 0) else length(object$m$resid())
    n - length(coef(object))
}

deviance.nls <- function(object, ...) object$m$deviance()

vcov.nls <- function(object, ...)
{
    sm <- summary(object)
    sm$cov.unscaled * sm$sigma^2
}


anova.nls <- function(object, ...)
{
    if(length(list(object, ...)) > 1L) return(anovalist.nls(object, ...))
    stop("anova is only defined for sequences of \"nls\" objects")
}

anovalist.nls <- function (object, ..., test = NULL)
{
    objects <- list(object, ...)
    responses <- as.character(lapply(objects,
				     function(x) formula(x)[[2L]]))
    sameresp <- responses == responses[1L]
    if (!all(sameresp)) {
	objects <- objects[sameresp]
        warning(gettextf("models with response %s removed because response differs from model 1",
                         sQuote(deparse(responses[!sameresp]))),
                domain = NA)
    }
    ## calculate the number of models
    nmodels <- length(objects)
    if (nmodels == 1L)
        stop("'anova' is only defined for sequences of \"nls\" objects")

    models <- as.character(lapply(objects, function(x) formula(x)))

    ## extract statistics
    df.r <- unlist(lapply(objects, df.residual))
    ss.r <- unlist(lapply(objects, deviance))
    df <- c(NA, -diff(df.r))
    ss <- c(NA, -diff(ss.r))
    ms <- ss/df
    f <- p <- rep_len(NA_real_, nmodels)
    for(i in 2:nmodels) {
	if(df[i] > 0) {
	    f[i] <- ms[i]/(ss.r[i]/df.r[i])
	    p[i] <- pf(f[i], df[i], df.r[i], lower.tail = FALSE)
	}
	else if(df[i] < 0) {
	    f[i] <- ms[i]/(ss.r[i-1]/df.r[i-1])
	    p[i] <- pf(f[i], -df[i], df.r[i-1], lower.tail = FALSE)
	}
	else {                          # df[i] == 0
            ss[i] <- 0
	}
    }
    table <- data.frame(df.r,ss.r,df,ss,f,p)
    dimnames(table) <- list(1L:nmodels, c("Res.Df", "Res.Sum Sq", "Df",
					 "Sum Sq", "F value", "Pr(>F)"))
    ## construct table and title
    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1L:nmodels),": ",
		     models, sep = "", collapse = "\n")

    ## calculate test statistic if needed
    structure(table, heading = c(title, topnote),
	      class = c("anova", "data.frame")) # was "tabular"
}
