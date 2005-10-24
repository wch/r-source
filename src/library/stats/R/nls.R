### $Id: nls.R,v 1.5 2004/06/10 11:25:28 ripley Exp $
###
###            Nonlinear least squares for R
###
### Copyright 1999-1999 Saikat DebRoy <saikat$stat.wisc.edu>,
###                     Douglas M. Bates <bates$stat.wisc.edu>,
###                     Jose C. Pinheiro <jcp$research.bell-labs.com>
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
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


numericDeriv <- function(expr, theta, rho = parent.frame()) {
    val <- .Call("numeric_deriv", expr, theta, rho, PACKAGE="stats")
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

nlsModel.plinear <- function( form, data, start ) {
    thisEnv <- environment()
    env <- new.env(parent=environment(form))
    for( i in names( data ) ) {
        assign( i, data[[i]], envir = env )
    }
    ind <- as.list( start )
    p2 <- 0
    for( i in names( ind ) ) {
        temp <- start[[ i ]]
        storage.mode( temp ) <- "double"
        assign( i, temp, envir = env )
        ind[[ i ]] <- p2 + seq( along = start[[ i ]] )
        p2 <- p2 + length( start[[ i ]] )
    }
    lhs <- eval( form[[2]], envir = env )
    storage.mode( lhs ) <- "double"
    rhs <- eval( form[[3]], envir = env )
    storage.mode( rhs ) <- "double"
    p1 <- if( is.matrix(rhs) ) { ncol(rhs) } else { 1 }
    p <- p1 + p2
    n <- length(lhs)
    fac <- ( n -  p )/p
    cc <- QR.B <- NA
    useParams <- rep(TRUE, p2)
    if( is.null( attr( rhs, "gradient" ) ) ) {
        getRHS.noVarying <- function()
          numericDeriv(form[[3]], names(ind), env)
        getRHS <- getRHS.noVarying
        rhs <- getRHS()
    } else {
        getRHS.noVarying <- function()
          eval( form[[3]], envir = env )
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
    QR.rhs <- qr( rhs )
    lin <- qr.coef( QR.rhs, lhs )
    resid <- qr.resid( QR.rhs, lhs )
    topzero <- double( p1 )
    dev <- sum( resid^2 )
    if( marg <= 1) {
        ddot <- function( A, b ) A %*% b
        dtdot <- function( A, b ) t(A) %*% b
    } else if( marg == 2 ) {
        if( p1 == 1 ) {
            ddot <- function( A, b ) A*b
            dtdot <- function( A, b ) t(b) %*% A
        } else if( p2 == 1 ) {
            ddot <- function( A, b ) A %*% b
            dtdot <- function( A, b ) t(A) %*% b
        }
    } else {
        ddot <- function( A, b ) apply( A, MARGIN = 3, FUN="%*%", b )
        dtdot <- function( A, b ) apply( A, MARGIN = c(2,3), FUN = "%*%", b )
    }

    getPars.noVarying <- function()
      unlist( setNames( lapply( names( ind ), get, envir = env ),
                       names( ind ) ) )
    getPars.varying <- function()
      unlist( setNames( lapply( names( ind ), get, envir = env ),
                       names( ind ) ) )[useParams]
    getPars <- getPars.noVarying

    internalPars <- getPars()
    setPars.noVarying <- function(newPars)
      {
          assign("internalPars", newPars, envir = thisEnv)
          for( i in names( ind ) ) {
              assign( i, clearNames(newPars[ ind[[i]] ]), envir = env )
          }
      }
    setPars.varying <- function(newPars)
      {
          internalPars[useParams] <- newPars
          for( i in names( ind ) ) {
              assign( i, clearNames(internalPars[ ind[[i]] ]), envir = env )
          }
      }
    setPars <- setPars.noVarying
    getPred <-
      if(is.matrix(rhs))
        function(X) as.numeric(X %*% lin)
      else function(X) X * lin

    m <-
      list(resid = function() resid,
           fitted = function() getPred(rhs),
           formula = function() form,
           deviance = function() dev,
           gradient = function() attr( rhs, "gradient" ),
           conv = function() {
               assign("cc", c( topzero, qr.qty( QR.rhs, lhs)[ -(1:p1)]),
                      envir = thisEnv)
               rr <- qr.qy( QR.rhs, cc )
               B <- qr.qty( QR.rhs, ddot( attr( rhs, "gradient"), lin ) )
               B[1:p1, ] <- dtdot( attr( rhs, "gradient" ), rr )
               R <- t( qr.R( QR.rhs )[1:p1, ] )
               if( p1 == 1 ) B[1, ] <- B[1, ]/R
               else B[1:p1, ] <- forwardsolve(R, B[1:p1, ])
               assign( "QR.B", qr(B), envir = thisEnv )
               rr <- qr.qty( QR.B, cc )
               sqrt( fac*sum( rr[1:p1]^2 ) / sum( rr[-(1:p1)]^2 ) )
           },
           incr = function() { qr.solve( QR.B, cc) },
           setVarying = function(vary = rep(TRUE, length(useParams)))
           {
               assign("useParams", if(is.character(vary)) {
                   temp <- logical(length(useParams))
                   temp[unlist(ind[vary])] <- TRUE
                   temp
               } else if(is.logical(vary) && length(vary) != length(useParams))
                      stop("setVarying : vary length must match length of parameters")
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
           setPars = function( newPars ) {
               setPars(newPars)
               assign("QR.rhs",
                      qr( assign( "rhs", getRHS(), envir = thisEnv ) ),
                      envir = thisEnv)
               assign( "resid", qr.resid( QR.rhs, lhs ), envir = thisEnv )
               assign("dev", sum( resid^2 ), envir = thisEnv )
               if( QR.rhs$rank < p1 ) {
                   return(1)
               } else {
                   assign( "lin", qr.coef( QR.rhs, lhs ), envir = thisEnv )
                   return(0)
               }
           },
           getPars = function() getPars(),
           getAllPars = function() {
               c( getPars(), c( .lin = lin ) )
           },
           getEnv = function() env,
           trace = function() cat( format( dev ),":",
             format( c( getPars(), lin ) ), "\n" ),
           Rmat = function()
           {
               qr.R( qr( cbind( ddot( attr( rhs, "gradient"), lin ), rhs )))
           },
           predict = function(newdata = list(), qr = FALSE)
           {
               getPred(eval(form[[3]], as.list(newdata), env))
           })
    class(m) <- c("nlsModel.plinear", "nlsModel")
    m$conv()
    on.exit( remove( data, i, m, marg, n, p, start, temp, gradSetArgs) )
    m
}

nlsModel <- function( form, data, start ) {
    thisEnv <- environment()
    env <- new.env(parent=environment(form))
    for( i in names( data ) ) {
        assign( i, data[[i]], envir = env )
    }
    ind <- as.list( start )
    parLength <- 0
    for( i in names( ind ) ) {
        temp <- start[[ i ]]
        storage.mode( temp ) <- "double"
        assign( i, temp, envir = env )
        ind[[ i ]] <- parLength + seq( along = start[[ i ]] )
        parLength <- parLength + length( start[[ i ]] )
    }
    useParams <- rep(TRUE, parLength)
    lhs <- eval( form[[2]], envir = env )
    rhs <- eval( form[[3]], envir = env )
    resid <- lhs - rhs
    dev <- sum( resid^2 )
    if( is.null( attr( rhs, "gradient" ) ) ) {
        getRHS.noVarying <- function()
          numericDeriv(form[[3]], names(ind), env)
        getRHS <- getRHS.noVarying
        rhs <- getRHS()
    } else {
        getRHS.noVarying <- function()
          eval( form[[3]], envir = env )
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
    QR <- qr( attr( rhs, "gradient" ) )
    qrDim <- min( dim( QR$qr ) )
    if( QR$rank < qrDim)
      stop("singular gradient matrix at initial parameter estimates")

    getPars.noVarying <- function()
      unlist( setNames( lapply( names( ind ), get, envir = env ),
                       names( ind ) ) )
    getPars.varying <- function()
      unlist( setNames( lapply( names( ind ), get, envir = env ),
                       names( ind ) ) )[useParams]
    getPars <- getPars.noVarying

    internalPars <- getPars()
    setPars.noVarying <- function(newPars)
      {
          assign("internalPars", newPars, envir = thisEnv)
          for( i in names( ind ) ) {
              assign( i, clearNames(newPars[ ind[[i]] ]), envir = env )
          }
      }
    setPars.varying <- function(newPars)
      {
          internalPars[useParams] <- newPars
          for( i in names( ind ) ) {
              assign( i, clearNames(internalPars[ ind[[i]] ]), envir = env )
          }
      }
    setPars <- setPars.noVarying

    on.exit(remove(i, data, parLength, start, temp, m))
    m <-
      list(resid = function() resid,
           fitted = function() rhs,
           formula = function() form,
           deviance = function() dev,
           gradient = function() attr( rhs, "gradient" ),
           conv = function()
           {
               rr <- qr.qty( QR, resid ) # rotated residual vector
               sqrt( sum( rr[1:npar]^2 ) / sum( rr[ -(1:npar) ]^2 ) )
           },
           incr = function() qr.coef( QR, resid ),
           setVarying = function(vary = rep(TRUE, length(useParams)))
           {
               assign("useParams", if(is.character(vary)) {
                   temp <- logical(length(useParams))
                   temp[unlist(ind[vary])] <- TRUE
                   temp
               } else if(is.logical(vary) && length(vary) != length(useParams))
                      stop("setVarying : vary length must match length of parameters")
               else {
                   vary
               }, envir = thisEnv)
               gradCall[[length(gradCall)]] <<- useParams
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
           setPars = function(newPars)
           {
               setPars(newPars)
               assign("resid",
                      lhs - assign("rhs", getRHS(), envir = thisEnv),
                      envir = thisEnv)
               assign("dev", sum( resid^2), envir = thisEnv)
               assign("QR", qr( attr( rhs, "gradient")), envir = thisEnv )
               return(QR$rank < min(dim(QR$qr)))  # to catch the singular gradient matrix
           },
           getPars = function() getPars(),
           getAllPars = function() getPars(),
           getEnv = function() env,
           trace = function() cat( format(dev),": ", format( getPars() ), "\n"),
           Rmat = function() qr.R( QR ),
           predict = function(newdata = list(), qr = FALSE)
           {
               eval(form[[3]], as.list(newdata), env)
           })
    class(m) <- "nlsModel"
    m
}

nls.control <- function( maxiter = 50, tol = 0.00001, minFactor = 1/1024 ) {
    list( maxiter = maxiter, tol = tol, minFactor = minFactor )
}

nls <-
  function (formula, data = parent.frame(), start, control = nls.control(),
            algorithm = c("default", "plinear", "port"), trace = FALSE,
            subset, weights, na.action, model = FALSE,
            lower =  - Inf, upper = Inf, ...)
{
    ## canonicalize the arguments
    formula <- as.formula(formula)
    algorithm <- match.arg(algorithm)

    mf <- match.call()             # for creating the model frame
    varNames <- all.vars(formula)  # parameter and variable names from formula

    ## adjust a one-sided model formula by using 0 as the response
    if (length(formula) == 2) {
        formula[[3]] <- formula[[2]]
        formula[[2]] <- 0
    }

    ## get names of the parameters from the starting values or selfStart model
    if (missing(start)) {
        if(!is.null(attr(data, "parameters"))) {
            pnames <- names(attr(data, "parameters"))
        } else {
            cll <- formula[[length(formula)]]
            func <- get(as.character(cll[[1]]))
            pnames <-
                as.character(as.list(match.call(func, call = cll))[-1][attr(func,
                                                      "pnames")])
        }
    } else {
        pnames <- names(start)
    }

    ## Heuristics for determining which names in formula represent actual
    ## variables
    ## If it is a parameter it is not a variable (nothing to guess here :-)
    varNames <- varNames[is.na(match(varNames, pnames, nomatch = NA))]
    ## If its length is a multiple of the response or LHS of the formula,
    ## then it is probably a variable
    ## This may fail if evaluation of formula[[2]] fails
    varIndex <- sapply(varNames, function(varName, data, respLength)
                       { length(eval(as.name(varName), data)) %% respLength == 0
                       }, data, length(eval(formula[[2]], data)))

    mf$formula <-                         # replace RHS by linear model formula
        as.formula(paste("~", paste( varNames[varIndex], collapse = "+")),
                   env = environment(formula))
    mf$start <- mf$control <- mf$algorithm <- mf$trace <- mf$model <- NULL
    mf[[1]] <- as.name("model.frame")
    mf <- as.list(eval(mf, parent.frame()))
    if (missing(start)) start <- getInitial(formula, mf)
    for(var in varNames[!varIndex])
        mf[[var]] <- eval(as.name(var), data)

    m <- switch(algorithm,
                plinear = nlsModel.plinear(formula, mf, start),
                port = nlsModel(formula, mf, start),
                nlsModel(formula, mf, start))

    ctrl <- nls.control()
    if(!missing(control)) {
        control <- as.list(control)
        ctrl[names(control)] <- control
    }
    if (algorithm != "port") {
        if (!missing(lower) || !missing(upper))
            warning('Upper or lower bounds ignored unless algorithm = "port"')
        nls.out <- list(m = .Call("nls_iter", m, ctrl, trace, PACKAGE = "stats"),
                        data = substitute(data), call = match.call())
        nls.out$call$control <- ctrl
        nls.out$call$trace <- trace
        nls.out$na.action <- attr(mf, "na.action")
        nls.out$dataClasses <- attr(attr(mf, "terms"), "dataClasses")
        if(model) nls.out$model <- mf
        class(nls.out) <- "nls"
        return(nls.out)
    }

    ## Establish the working vectors and check and set options
    p <- length(par <- as.double(start))
    iv <- integer(4*p + 82)
    v <- double(105 + (p * (2 * p + 20)))
    .Call("port_ivset", 1, iv, v, PACKAGE = "stats")
    if (length(control)) {
        control $tol <- control$minFactor <- NULL
        nms <- names(control)
        if (!is.list(control) || is.null(nms))
            stop("control argument must be a named list")
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
        iv[19] <- 1:1
    scale <- 1
    low <- upp <- NULL
    if (any(lower != -Inf) || any(upper != Inf)) { 
        low <- rep(as.double(lower), length = length(par))
        upp <- rep(as.double(upper), length = length(par))
    }
    ## Call driver routine
    .Call("port_nlsb", m,
          d = rep(as.double(scale), length = length(par)),
          df = m$gradient(), iv, v, low, upp, PACKAGE = "stats")
    ans <- list(m = m, data = substitute(data), call = match.call())
    ans$convergence <- as.integer(if (iv[1] %in% 3:6) 0 else 1)
    ans$message <-
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
               "65" = "gr cannot be computed at initial par (65)")
    if (is.null(ans$message))
        ans$message <-
            paste("See PORT documentation.  Code (", iv[1], ")",
                  sep = "")
    if (ans$convergence)
        stop(paste("Convergence failure:", ans$message))
    class(ans) <- "nls"
    ans
}

coef.nls <- function( object, ... ) object$m$getAllPars()

print.nls <- function(x, ...) {
    cat( "Nonlinear regression model\n" )
    cat( "  model: ", deparse( formula(x) ), "\n" )
    cat( "   data: ", deparse( x$data ), "\n" )
    print( x$m$getAllPars() )
    cat( " residual sum-of-squares: ", format( x$m$deviance() ), "\n" )
    invisible(x)
}

summary.nls <- function (object, ...)
{
    z <- object
    ## we want the raw values, not the na-adjusted ones.
    r <- resid <- as.vector(object$m$resid())
    n <- length(resid)
    param <- coef(z)
    pnames <- names(param)
    p <- length(param)
    rdf <- n - p
    f <- as.vector(object$m$fitted())
    w <- z$weights
    R <- z$m$Rmat()
    if (!is.null(w)) {
        w <- w^0.5
        resid <- resid * w
        f <- f * w
        excl <- w == 0
        if (any(excl)) {
            warning(sum(excl), " rows with zero weights not counted")
            r <- r[!excl]
            f <- f[!excl]
            rdf <- rdf - sum(excl)
        }
    }
    rss <- deviance(z)
    if (n > p) {
        resvar <- rss/rdf
    }
    R <- chol2inv(R)
    dimnames(R) <- list(pnames, pnames)
    se <- sqrt(diag(R) * resvar)
    correl <- (R * resvar)/outer(se, se)
    ans <- list(formula = formula(z), residuals = r, sigma = sqrt(resvar),
                df = c(p, rdf), cov.unscaled = R, correlation = correl)
    tval <- param/se
    param <- cbind( param, se, tval, 2 * pt(abs(tval), rdf, lower.tail = FALSE))
    dimnames(param) <-
      list(pnames, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    ans$parameters <- param
    class(ans) <- "summary.nls"
    ans
}

print.summary.nls <-
  function (x, digits = max(3, getOption("digits") - 3), symbolic.cor = p >
            4, signif.stars = getOption("show.signif.stars"), ...)
{
    cat("\nFormula: ")
    cat(paste(deparse(x$formula), sep = "\n", collapse = "\n"),
        "\n", sep = "")
    df <- x$df
    rdf <- df[2]
    cat("\nParameters:\n")
    printCoefmat(x$parameters, digits = digits, signif.stars = signif.stars,
                  ...)
    cat("\nResidual standard error:", format(signif(x$sigma,
                                                    digits)), "on", rdf, "degrees of freedom\n")
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- dim(correl)[2]
        if (p > 1) {
            cat("\nCorrelation of Parameter Estimates:\n")
            if (symbolic.cor)
              print(symnum(correl)[-1, -p])
            else {
                correl[!lower.tri(correl)] <- NA
                print(correl[-1, -p, drop = FALSE], digits = digits, na = "")
            }
        }
    }
    cat("\n")
    invisible(x)
}

# Unusually, this uses `parameters' not `coefficients'
coef.summary.nls <- function (object, ...) object$parameters

weights.nls <- function( object, ... ) object$weights

predict.nls <-
  function(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
           interval = c("none", "confidence", "prediction"), level = 0.95,
           ...)
{
    if (missing(newdata))
        return(as.vector(fitted(object)))
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
    val <- as.vector(object$m$resid())
    if (type == "pearson") {
        std <- sqrt(sum(val^2)/(length(val) - length(coef(object))))
        val <- val/std
        if(!is.null(object$na.action))
            val <- naresid(object$na.action, val)
        attr(val, "label") <- "Standardized residuals"
    } else {
        if(!is.null(object$na.action))
            val <- naresid(object$na.action, val)
        lab <- "Residuals"
        if (!is.null(aux <- attr(object, "units")$y)) {
            lab <- paste(lab, aux)
        }
        attr(val, "label") <- lab
    }
    val
}

## logLik & AIC -- generic now in base

logLik.nls <- function(object, REML = FALSE, ...)
{
    if (REML)
        stop("cannot calculate REML log-likelihood for \"nls\" objects")

    res <- object$m$resid()
    N <- length(res)
    if(is.null(w <- object$weights)) {
        w <- rep(1, N)
    }
    val <-  -N * (log(2 * pi) + 1 - log(N) - sum(log(w)) + log(sum(w*res^2)))/2
    attr(val, "df") <- length(coef(object))
    attr(val, "nobs") <- attr(val, "nall") <- N
    class(val) <- "logLik"
    val
}

df.residual.nls <- function(object, ...)
    length(resid(object)) - length(coef(object))

deviance.nls <- function(object, ...) object$m$deviance()

vcov.nls <- function(object, ...)
{
    sm <- summary(object)
    sm$cov.unscaled * sm$sigma^2
}


anova.nls <- function(object, ...)
{
    if(length(list(object, ...)) > 1)
	return(anovalist.nls(object, ...))
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
    f <- p <- rep(as.numeric(NA), nmodels)
    for(i in 2:nmodels) {
	if(df[i] > 0) {
	    f[i] <- ms[i]/(ss.r[i]/df.r[i])
	    p[i] <- pf(f[i], df[i], df.r[i], lower.tail = FALSE)
	}
	else if(df[i] < 0) {
	    f[i] <- ms[i]/(ss.r[i-1]/df.r[i-1])
	    p[i] <- pf(f[i], -df[i], df.r[i-1], lower.tail = FALSE)
	}
	else { # df[i] == 0
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
	      class= c("anova", "data.frame"))# was "tabular"
}
