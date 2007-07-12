###
###            Nonlinear least squares for R
###
### Copyright 1999-1999 Saikat DebRoy <saikat$stat.wisc.edu>,
###                     Douglas M. Bates <bates$stat.wisc.edu>,
###                     Jose C. Pinheiro <jcp$research.bell-labs.com>
### Copyright 2005-7    The R Development Core Team
###
### This file is part of the nls library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
### Boston, MA 02110-1301, USA



numericDeriv <- function(expr, theta, rho = parent.frame(), dir=1)
{
    dir <- rep(dir, length.out = length(theta))
    val <- .Call(R_numeric_deriv, expr, theta, rho, dir)
    valDim <- dim(val)
    if (!is.null(valDim)) {
        if (valDim[length(valDim)] == 1)
            valDim <- valDim[-length(valDim)]
        if(length(valDim) > 1)
            dim(attr(val, "gradient")) <- c(valDim,
                                            dim(attr(val, "gradient"))[-1])
    }
    val
}

nlsModel.plinear <- function(form, data, start, wts)
{
    thisEnv <- environment()
    env <- new.env(parent=environment(form))
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
    lhs <- eval(form[[2]], envir = env)
    storage.mode(lhs) <- "double"
    rhs <- eval(form[[3]], envir = env)
    storage.mode(rhs) <- "double"
    .swts <- if(!missing(wts) && (length(wts) != 0))
        sqrt(wts) else rep(1, length.out=length(rhs))
    assign(".swts", .swts, envir = env)
    p1 <- if(is.matrix(rhs)) ncol(rhs) else 1
    p <- p1 + p2
    n <- length(lhs)
    fac <- (n -  p)/p
    cc <- QR.B <- NA
    useParams <- rep(TRUE, p2)
    if(is.null(attr(rhs, "gradient"))) {
        getRHS.noVarying <- function()
            numericDeriv(form[[3]], names(ind), env)
        getRHS <- getRHS.noVarying
        rhs <- getRHS()
    } else {
        getRHS.noVarying <- function() eval(form[[3]], envir = env)
        getRHS <- getRHS.noVarying
    }
    dimGrad <- dim(attr(rhs, "gradient"))
    marg <- length(dimGrad)
    if(marg > 0) {
        gradSetArgs <- vector("list", marg+1)
        for(i in 2:marg)
            gradSetArgs[[i]] <- rep(TRUE, dimGrad[i-1])
        useParams <- rep(TRUE, dimGrad[marg])
    } else {
        gradSetArgs <- vector("list", 2)
        useParams <- rep(TRUE, length(attr(rhs, "gradient")))
    }
    gradSetArgs[[1]] <- (~attr(ans, "gradient"))[[2]]
    gradCall <-
        switch(length(gradSetArgs) - 1,
               call("[", gradSetArgs[[1]], gradSetArgs[[2]]),
               call("[", gradSetArgs[[1]], gradSetArgs[[2]], gradSetArgs[[2]]),
               call("[", gradSetArgs[[1]], gradSetArgs[[2]], gradSetArgs[[2]],
                    gradSetArgs[[3]]),
               call("[", gradSetArgs[[1]], gradSetArgs[[2]], gradSetArgs[[2]],
                    gradSetArgs[[3]], gradSetArgs[[4]]))
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
        ddot <- function(A, b) apply(A, MARGIN = 3, FUN="%*%", b)
        dtdot <- function(A, b) apply(A, MARGIN = c(2,3), FUN = "%*%", b)
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
            assign(i, clearNames(newPars[ ind[[i]] ]), envir = env )
        }
    }
    setPars.varying <- function(newPars)
    {
        internalPars[useParams] <- newPars
        for(i in names(ind))
            assign(i, clearNames(internalPars[ ind[[i]] ]), envir = env)
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
                 assign("cc", c(topzero, qr.qty(QR.rhs, .swts * lhs)[ -(1:p1)]),
                        envir = thisEnv)
                 rr <- qr.qy(QR.rhs, cc)
                 B <- qr.qty(QR.rhs, .swts * ddot(attr(rhs, "gradient"), lin))
                 B[1:p1, ] <- dtdot(.swts * attr(rhs, "gradient"), rr)
                 R <- t( qr.R(QR.rhs)[1:p1, ] )
                 if(p1 == 1) B[1, ] <- B[1, ]/R
                 else B[1:p1, ] <- forwardsolve(R, B[1:p1, ])
                 assign("QR.B", qr(B), envir = thisEnv)
                 rr <- qr.qty(QR.B, cc)
                 sqrt( fac*sum(rr[1:p1]^2) / sum(rr[-(1:p1)]^2) )
             },
             incr = function() qr.solve(QR.B, cc),
             setVarying = function(vary = rep(TRUE, length(useParams))) {
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
             trace = function() cat(format(dev),":",
             format(c(getPars(), lin)), "\n" ),
             Rmat = function()
             qr.R(qr(.swts * cbind(ddot(attr(rhs, "gradient"), lin), rhs))),
             predict = function(newdata = list(), qr = FALSE)
             getPred(eval(form[[3]], as.list(newdata), env))
             )
    class(m) <- c("nlsModel.plinear", "nlsModel")
    m$conv()
    on.exit( remove( data, i, m, marg, n, p, start, temp, gradSetArgs) )
    m
}

nlsModel <- function(form, data, start, wts, upper=NULL)
{
    thisEnv <- environment()
    env <- new.env(parent = environment(form))
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

    if(!is.null(upper)) upper <- rep(upper, length.out = parLength)
    useParams <- rep(TRUE, parLength)
    lhs <- eval(form[[2]], envir = env)
    rhs <- eval(form[[3]], envir = env)
    .swts <- if(!missing(wts) && (length(wts) != 0))
        sqrt(wts) else rep(1, length.out=length(rhs))
    assign(".swts", .swts, envir = env)
    resid <- .swts * (lhs - rhs)
    dev <- sum(resid^2)
    if(is.null(attr(rhs, "gradient"))) {
        getRHS.noVarying <- function() {
            if(is.null(upper))
                numericDeriv(form[[3]], names(ind), env)
            else
                numericDeriv(form[[3]], names(ind), env,
                             ifelse(internalPars < upper, 1, -1))
        }
        getRHS <- getRHS.noVarying
        rhs <- getRHS()
    } else {
        getRHS.noVarying <- function() eval(form[[3]], envir = env)
        getRHS <- getRHS.noVarying
    }
    dimGrad <- dim(attr(rhs, "gradient"))
    marg <- length(dimGrad)
    if(marg > 0) {
        gradSetArgs <- vector("list", marg+1)
        for(i in 2:marg)
            gradSetArgs[[i]] <- rep(TRUE, dimGrad[i-1])
        useParams <- rep(TRUE, dimGrad[marg])
    } else {
        gradSetArgs <- vector("list", 2)
        useParams <- rep(TRUE, length(attr(rhs, "gradient")))
    }
    npar <- length(useParams)
    gradSetArgs[[1]] <- (~attr(ans, "gradient"))[[2]]
    gradCall <-
        switch(length(gradSetArgs) - 1,
               call("[", gradSetArgs[[1]], gradSetArgs[[2]], drop = FALSE),
               call("[", gradSetArgs[[1]], gradSetArgs[[2]], gradSetArgs[[2]],
                    drop = FALSE),
               call("[", gradSetArgs[[1]], gradSetArgs[[2]], gradSetArgs[[2]],
                    gradSetArgs[[3]], drop = FALSE),
               call("[", gradSetArgs[[1]], gradSetArgs[[2]], gradSetArgs[[2]],
                    gradSetArgs[[3]], gradSetArgs[[4]]), drop = FALSE)
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
            assign(i, clearNames(newPars[ ind[[i]] ]), envir = env)
    }
    setPars.varying <- function(newPars)
    {
        internalPars[useParams] <- newPars
        for(i in names(ind))
            assign(i, clearNames(internalPars[ ind[[i]] ]), envir = env)
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
		 sqrt( sum(rr[1:npar]^2) / sum(rr[-(1:npar)]^2))
	     },
	     incr = function() qr.coef(QR, resid),
	     setVarying = function(vary = rep(TRUE, length(useParams))) {
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
		 gradCall[[length(gradCall) - 1]] <<- useParams
		 if(all(useParams)) {
		     assign("setPars", setPars.noVarying, envir = thisEnv)
		     assign("getPars", getPars.noVarying, envir = thisEnv)
		     assign("getRHS", getRHS.noVarying, envir = thisEnv)
		     assign("npar", length(useParams), envir = thisEnv)
		 } else {
		     assign("setPars", setPars.varying, envir = thisEnv)
		     assign("getPars", getPars.varying, envir = thisEnv)
		     assign("getRHS", getRHS.varying, envir = thisEnv)
		     assign("npar", length((1:length(useParams))[useParams]),
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
	     trace = function() cat(format(dev),": ", format(getPars()), "\n"),
	     Rmat = function() qr.R(QR),
	     predict = function(newdata = list(), qr = FALSE)
	     eval(form[[3]], as.list(newdata), env)
	     )

    class(m) <- "nlsModel"
    m
}

nls.control <- function(maxiter = 50, tol = 0.00001, minFactor = 1/1024,
			printEval = FALSE, warnOnly = FALSE)
    list(maxiter = maxiter, tol = tol, minFactor = minFactor,
	 printEval = printEval, warnOnly = warnOnly)

nls_port_fit <- function(m, start, lower, upper, control, trace)
{
    ## Establish the working vectors and check and set options
    p <- length(par <- as.double(unlist(start)))
    iv <- integer(4*p + 82)
    v <- double(105 + (p * (2 * p + 20)))
    .Call(R_port_ivset, 1, iv, v)
    if (length(control)) {
	if (!is.list(control) || is.null(nms <- names(control)))
	    stop("control argument must be a named list")
	## remove those components that do not apply here
	for(noN in intersect(nms, c("tol", "minFactor", "warnOnly", "printEval")))
	    control[[noN]] <- NULL
	nms <- names(control)
	cpos <- c(eval.max = 17, maxiter = 18, trace = 19, abs.tol = 31,
		  rel.tol = 32, x.tol = 33, step.min = 34, step.max = 35,
		  scale.init = 38, sing.tol = 37, diff.g = 42)
	pos <- pmatch(nms, names(cpos))
        if (any(nap <- is.na(pos))) {
            warning(paste("unrecognized control element(s) named `",
                          paste(nms[nap], collapse = ", "),
                          "' ignored", sep = ""))
            pos <- pos[!nap]
            control <- control[!nap]
        }
        ivpars <- pos < 4
        if (any(ivpars))
            iv[cpos[pos[ivpars]]] <- as.integer(unlist(control[ivpars]))
        if (any(!ivpars))
            v[cpos[pos[!ivpars]]] <- as.double(unlist(control[!ivpars]))
    }
    if (trace)
        iv[19] <- 1L
    scale <- 1
    low <- upp <- NULL
    if (any(lower != -Inf) || any(upper != Inf)) {
        low <- rep(as.double(lower), length.out = length(par))
        upp <- rep(as.double(upper), length.out = length(par))
        if(any(start < low || start > upp)) {
            iv[1] <- 300
            return(iv)
        }
    }
    if(p > 0) {
        ## driver routine port_nlsb() in ../src/port.c -- modifies m & iv
        .Call(R_port_nlsb, m,
              d = rep(as.double(scale), length.out = length(par)),
              df = m$gradient(), iv, v, low, upp)
    } else iv[1] <- 6
    iv
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

    mf <- match.call()                  # for creating the model frame
    varNames <- all.vars(formula) # parameter and variable names from formula
    ## for prediction we will need to know those which are in RHS
    form2 <- formula; form2[[2]] <- 0
    varNamesRHS <- all.vars(form2)
    mWeights <- missing(weights)

    ## adjust a one-sided model formula by using 0 as the response
    if (length(formula) == 2) {
        formula[[3]] <- formula[[2]]
        formula[[2]] <- 0
    }

    ## get names of the parameters from the starting values or selfStart model
    pnames <-
	if (missing(start)) {
	    if(!is.null(attr(data, "parameters"))) {
		names(attr(data, "parameters"))
	    } else { ## try selfStart - like object
		cll <- formula[[length(formula)]]
		func <- get(as.character(cll[[1]]))
		if(!is.null(pn <- attr(func, "pnames")))
		    as.character(as.list(match.call(func, call = cll))[-1][pn])
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
    if(!length(varNames)) stop("no parameters to fit")
    ## This aux.function needs to be as complicated because
    ## exists(var, data) does not work (with lists or dataframes):
    lenVar <- function(var) tryCatch(length(eval(as.name(var), data, env)),
				     error = function(e) -1)
    n <- sapply(varNames, lenVar)
    if(any(not.there <- n == -1)) {
	nnn <- names(n[not.there])
	if(missing(start)) {
	    if(algorithm == "plinear")
		## TODO: only specify values for the non-lin. parameters
		stop("No starting values specified")
	    ## Provide some starting values instead of erroring out later;
	    ## '1' seems slightly better than 0 (which is often invalid):
	    warning("No starting values specified for some parameters.\n",
		    "Intializing ", paste(sQuote(nnn), collapse=", "),
		    " to '1.'.\n",
		    "Consider specifying 'start' or using a selfStart model")
	    start <- as.list(rep(1., length(nnn)))
	    names(start) <- nnn
	    varNames <- varNames[i <- is.na(match(varNames, nnn))]
	    n <- n[i]
	}
	else # has 'start' but forgot some
	    stop("parameters without starting value in 'data': ",
		 paste(nnn, collapse=", "))
    }

    ## If its length is a multiple of the response or LHS of the formula,
    ## then it is probably a variable.
    ## This may fail (e.g. when LHS contains parameters):
    respLength <- length(eval(formula[[2]], data, env))
    varIndex <- n %% respLength == 0

    mf$formula <-                # replace by one-sided linear model formula
        as.formula(paste("~", paste(varNames[varIndex], collapse = "+")),
                   env = environment(formula))
    mf$start <- mf$control <- mf$algorithm <- mf$trace <- mf$model <- NULL
    mf$lower <- mf$upper <- NULL
    mf[[1]] <- as.name("model.frame")
    mf <- eval.parent(mf)
    n <- nrow(mf)
    mf <- as.list(mf)
    if (missing(start)) start <- getInitial(formula, mf)
    for(var in varNames[!varIndex])
        mf[[var]] <- eval(as.name(var), data, env)
    varNamesRHS <- varNamesRHS[ varNamesRHS %in% varNames[varIndex] ]
    wts <- if(!mWeights) model.weights(mf) else rep(1, n)
    if (any(wts < 0 | is.na(wts)))
	stop("missing or negative weights not allowed")

    m <- switch(algorithm,
		plinear = nlsModel.plinear(formula, mf, start, wts),
		port = nlsModel(formula, mf, start, wts, upper),
                ## Default:
		nlsModel(formula, mf, start, wts))

    ctrl <- nls.control()
    if(!missing(control)) {
	control <- as.list(control)
	ctrl[names(control)] <- control
    }
    if (algorithm != "port") {
	if (!missing(lower) || !missing(upper))
	    warning('Upper or lower bounds ignored unless algorithm = "port"')
        convInfo <- .Call(R_nls_iter, m, ctrl, trace)
	nls.out <- list(m = m, convInfo = convInfo,
			data = substitute(data), call = match.call())
    }
    else { ## "port" i.e., PORT algorithm
	iv <- nls_port_fit(m, start, lower, upper, control, trace)
	nls.out <- list(m = m, data = substitute(data), call = match.call())
        ## FIXME: this is really a logical for  *NON*convergence:
	nls.out$convergence <- as.integer(if (iv[1] %in% 3:6) 0 else 1)
	nls.out$message <-
	    switch(as.character(iv[1]),
		   "3" = "X-convergence (3)",
		   "4" = "relative convergence (4)",
		   "5" = "both X-convergence and relative convergence (5)",
		   "6" = "absolute function convergence (6)",

		   "7" = "singular convergence (7)",
		   "8" = "false convergence (8)",
		   "9" = "function evaluation limit reached without convergence (9)",
		   "10" = "iteration limit reached without convergence (9)",
		   "14" = "storage has been allocated (?) (14)",

		   "15" = "LIV too small (15)",
		   "16" = "LV too small (16)",
		   "63" = "fn cannot be computed at initial par (63)",
		   "65" = "gr cannot be computed at initial par (65)",
		   "300" = "initial par violates constraints")
	if (is.null(nls.out$message))
	    nls.out$message <-
		paste("See PORT documentation.	Code (", iv[1], ")", sep = "")
	if (nls.out$convergence) {
            msg <- paste("Convergence failure:", nls.out$message)
            if(ctrl$warnOnly) {
                warning(msg)
            } else stop(msg)
        }

	## we need these (evaluated) for profiling
	nls.out$call$lower <- lower
	nls.out$call$upper <- upper
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
    if(identical(object$call$algorithm, "port"))
	ans$message <- object$message
    class(ans) <- "summary.nls"
    ans
}

.p.nls.convInfo <- function(x, digits)
{
    if(identical(x$call$algorithm, "port"))
	cat("\nAlgorithm \"port\", convergence message:",
	    x$message, "\n")
    else
	with(x$convInfo, {
	    cat("\nNumber of iterations",
		if(isConv) "to convergence:" else "till stop:", finIter,
		"\nAchieved convergence tolerance:",
                format(finTol, digits=digits),"\n")
	    if(!isConv)
		cat("Reason stopped:", stopMessage, "\n")
	})
    invisible()
}

print.nls <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("Nonlinear regression model\n")
    cat("  model: ", deparse(formula(x)), "\n")
    cat("   data: ", deparse(x$data), "\n")
    print(x$m$getAllPars(), digits = digits, ...)
    cat(" ", if(!is.null(x$weights) && diff(range(x$weights))) "weighted ",
	"residual sum-of-squares: ", format(x$m$deviance(), digits = digits),
	"\n", sep = '')
    .p.nls.convInfo(x, digits = digits)
    invisible(x)
}

print.summary.nls <-
  function (x, digits = max(3, getOption("digits") - 3),
            symbolic.cor = x$symbolic.cor,
            signif.stars = getOption("show.signif.stars"), ...)
{
    cat("\nFormula: ")
    cat(paste(deparse(x$formula), sep = "\n", collapse = "\n"), "\n", sep = "")
    df <- x$df
    rdf <- df[2]
    cat("\nParameters:\n")
    printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars,
                 ...)
    cat("\nResidual standard error:",
        format(signif(x$sigma, digits)), "on", rdf, "degrees of freedom\n")
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1) {
            cat("\nCorrelation of Parameter Estimates:\n")
	    if(is.logical(symbolic.cor) && symbolic.cor) {
		print(symnum(correl, abbr.colnames = NULL))
            } else {
                correl <- format(round(correl, 2), nsmall = 2, digits = digits)
                correl[!lower.tri(correl)] <- ""
                print(correl[-1, -p, drop=FALSE], quote = FALSE)
            }
        }
    }

    .p.nls.convInfo(x, digits = digits)

    if(nzchar(mess <- naprint(x$na.action))) cat("  (", mess, ")\n", sep="")
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
    if(is.null(w <- object$weights)) w <- rep(1, N)
    val <-  -N * (log(2 * pi) + 1 - log(N) - sum(log(w)) + log(sum(w*res^2)))/2
    ## the formula here corresponds to estimating sigma^2.
    attr(val, "df") <- 1 + length(coef(object))
    attr(val, "nobs") <- attr(val, "nall") <- N
    class(val) <- "logLik"
    val
}

df.residual.nls <- function(object, ...) {
    w <- object$weights
    n <- if(!is.null(w)) sum(w != 0) else length(resid(object))
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
    if(length(list(object, ...)) > 1) return(anovalist.nls(object, ...))
    stop("anova is only defined for sequences of \"nls\" objects")
}

anovalist.nls <- function (object, ..., test = NULL)
{
    objects <- list(object, ...)
    responses <- as.character(lapply(objects,
				     function(x) formula(x)[[2]]))
    sameresp <- responses == responses[1]
    if (!all(sameresp)) {
	objects <- objects[sameresp]
	warning("models with response ",
                deparse(responses[!sameresp]),
                " removed because response differs from model 1")
    }
    ## calculate the number of models
    nmodels <- length(objects)
    if (nmodels == 1)
        stop("'anova' is only defined for sequences of \"nls\" objects")

    models <- as.character(lapply(objects, function(x) formula(x)))

    ## extract statistics
    df.r <- unlist(lapply(objects, df.residual))
    ss.r <- unlist(lapply(objects, deviance))
    df <- c(NA, -diff(df.r))
    ss <- c(NA, -diff(ss.r))
    ms <- ss/df
    f <- p <- rep(NA_real_, nmodels)
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
    dimnames(table) <- list(1:nmodels, c("Res.Df", "Res.Sum Sq", "Df",
					 "Sum Sq", "F value", "Pr(>F)"))
    ## construct table and title
    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1:nmodels),": ",
		     models, sep="", collapse="\n")

    ## calculate test statistic if needed
    structure(table, heading = c(title, topnote),
	      class = c("anova", "data.frame")) # was "tabular"
}
