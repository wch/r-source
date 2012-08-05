#  File src/library/stats/R/nls-profile.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1999-1999 Saikat DebRoy and Douglas M. Bates
#  Copyright (C) 1999-2011  The R Core Team
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
### Profiling nonlinear least squares for R
###

profiler <- function(fitted, ...) UseMethod("profiler")

profiler.nls <- function(fitted, ...)
{
    fittedModel <- fitted$m
    algorithm <- fitted$call$algorithm
    ctrl <- fitted$call$control
    trace <- fitted$call$trace
    defaultPars <- fittedPars <- fittedModel$getPars()
    lower <- fitted$call$lower
    lower <- rep_len(if(!is.null(lower)) as.double(lower) else Inf,
                     length(defaultPars))
    upper <- fitted$call$upper
    upper <- rep_len(if(!is.null(upper)) as.double(upper) else Inf,
                     length(defaultPars))
    defaultVary <- rep.int(TRUE, length(defaultPars))
    S.hat <- deviance(fitted) # need to allow for weights
    s2.hat <- summary(fitted)$sigma^2
    thisEnv <- environment()
    on.exit(remove(fitted))
    prof <- list(getFittedPars = function() fittedPars,
                 getFittedModel = function() fittedModel,
                 setDefault = function(varying, params)
             {
                 if(missing(params) && missing(varying)) {
                     fittedModel$setVarying()
                     fittedModel$setPars(fittedPars)
                     assign("defaultPars", fittedPars, envir = thisEnv)
                     assign("defaultVary", rep.int(TRUE, length(defaultPars)),
                            envir = thisEnv)
                 } else {
                     if(!missing(params)) {
                         if(length(params) != length(fittedPars))
                             stop("'params' has wrong length")
                         assign("defaultPars", params, envir = thisEnv)
                     }
                     if(!missing(varying)) {
                         if(is.numeric(varying)) {
                             if(!all(varying %in% seq_along(fittedPars)))
                                 stop("'varying' must be in seq_along(pars)")
                             varying <- !((seq_along(fittedPars)) %in% varying)
                         } else if(is.logical(varying)) {
                             if(length(varying) != length(fittedPars))
                                 stop("'varying' has wrong length")
                         } else if(is.character(varying)) {
                             if(!all(varying %in% names(fittedPars)))
                                 stop("'varying' must be in seq_along(pars)")
                             varying <- !(names(fittedPars) %in% varying)
                         } else stop("'varying' must be logical, integer or character")
                         assign("defaultVary", varying, envir = thisEnv)
                     }
                 }
             },
                 getProfile = function(...)
             {
                 args <- list(...)
                 if(length(args) == 0L) {
                     vary <- defaultVary
                     startPars <- defaultPars
                 } else if(length(args) == 2L && is.logical(args[[1L]])) {
                     vary <- args[[1L]]
                     params <- unlist(args[[2L]])
                     startPars <- defaultPars
                     startPars[!vary] <- params
                 } else {
                     if(length(args) == 1 && is.list(args[[1L]])) {
                         params <- unlist(args[[1L]])
                     } else if(all(sapply(args, is.numeric))) {
                         params <- unlist(args)
                     } else stop("invalid argument to 'getProfile'")
                     if(!all(names(params) %in% names(fittedPars)))
                         stop("cannot recognize parameter name")
                     startPars <- defaultPars
                     vary <- !(names(fittedPars) %in% names(params))
                     startPars[!vary] <- params
                 }
                 fittedModel$setVarying()
                 fittedModel$setPars(startPars)
                 fittedModel$setVarying(vary)
                 fittedModel$setPars(startPars[vary])
                 ## change fittedModel into profiledModel
		 if(algorithm != "port") {
		     if(sum(vary)) .Call(C_nls_iter, fittedModel, ctrl, trace)
		     dev <- fittedModel$deviance()
		 } else {
		     iv <- nls_port_fit(fittedModel, startPars[vary],
					lower[vary], upper[vary], ctrl, trace)
		     dev <- if(!iv[1L] %in% 3:6)
			NA_real_
		     else
			fittedModel$deviance()
		 }
		 profiledModel <- fittedModel
                 fstat <- (dev - S.hat)/s2.hat
                 fittedModel$setVarying()
                 ans <- list(fstat = fstat,
                             parameters = profiledModel$getAllPars(),
                             varying = vary)
                 fittedModel$setPars(defaultPars)
                 ans
             })
    class(prof) <- c("profiler.nls", "profiler")
    prof
}

profile.nls <-
  function(fitted, which = 1L:npar, maxpts = 100, alphamax = 0.01,
           delta.t = cutoff/5, ...)
{
    f.summary <- summary(fitted)
    std.err <- f.summary$coefficients[, "Std. Error"]
    nobs <- length(resid(fitted))
    prof <- profiler(fitted)
    pars <- prof$getFittedPars()
    npar <- length(pars)  # less in a partially linear model
    lower <- fitted$call$lower
    lower <- rep_len(if(!is.null(lower)) as.double(lower) else -Inf, npar)
    upper <- fitted$call$upper
    upper <- rep_len(if(!is.null(upper)) as.double(upper) else Inf, npar)
    if(is.character(which)) which <- match(which, names(pars), 0)
    which <- which[which >= 1 & which <= npar]
    ## was 'npar' - length(which) would have made more sense
    cutoff <- sqrt(qf(1 - alphamax, 1L, nobs - npar))
    out <- vector("list", npar)
    on.exit(prof$setDefault())     # in case there is an abnormal exit
    for(par in which) {
        pars <- prof$getFittedPars() # reset to fitted model's values
        prof$setDefault(varying = par)
        sgn <- -1
        count <- 1
        varying <- rep.int(TRUE, npar)
        varying[par] <- FALSE
        tau <- double(2 * maxpts)
        par.vals <- array(0, c(2L * maxpts, npar), list(NULL, names(pars)))
        tau[1L] <- 0
        par.vals[1,  ] <- pars
        base <- pars[par]
        profile.par.inc <- delta.t * std.err[par]
        pars[par] <- base - profile.par.inc
        pars[par] <- pmin(upper[par], pmax(lower[par], pars[par]))
        while(count <= maxpts) {
            if(is.na(pars[par]) || isTRUE(all.equal(pars, par.vals[1, ])) ||
               pars[par] < lower[par] || pars[par] > upper[par] ||
               abs(pars[par] - base)/std.err[par] > 10 * cutoff) break
            prof$setDefault(params = pars)
            ans <- prof$getProfile()
            if(is.na(ans$fstat) || ans$fstat < 0) break
            newtau <- sgn*sqrt(ans$fstat)
            if(abs(newtau - tau[count]) < 0.1) break
            count <- count + 1
            tau[count] <- newtau
            par.vals[count, ] <- pars <- ans$parameters[1L:npar]
            if(abs(tau[count]) > cutoff) break
            pars <- pars + ((pars - par.vals[count - 1,  ]) * delta.t)/
                abs(tau[count] - tau[count - 1])
            pars[-par] <- pmin(upper[-par], pmax(lower[-par], pars[-par]))
        }
        ind <- seq_len(count)
        tau[ind] <- tau[rev(ind)]
        par.vals[ind,  ] <- par.vals[rev(ind),  ]
        sgn <- 1
        newmax <- count + maxpts
        pars <- par.vals[count,  ]
        pars[par] <- base + profile.par.inc
        pars[par] <- pmin(upper[par], pmax(lower[par], pars[par]))
        while(count <= newmax) {
            if(is.na(pars[par]) || isTRUE(all.equal(pars, par.vals[1, ])) ||
               pars[par] < lower[par] || pars[par] > upper[par] ||
               abs(pars[par] - base)/std.err[par] > 10 * cutoff) break
            prof$setDefault(params = pars)
            ans <- prof$getProfile()
            if(is.na(ans$fstat)|| ans$fstat < 0) break
            newtau <- sgn*sqrt(ans$fstat)
            if(abs(newtau - tau[count]) < 0.1) break
            count <- count + 1
            tau[count] <- newtau
            par.vals[count, ] <- pars <- ans$parameters[1L:npar]
            if(abs(tau[count]) > cutoff) break
            pars <- pars + ((pars - par.vals[count - 1,  ]) * delta.t)/
                abs(tau[count] - tau[count - 1])
            pars[-par] <- pmin(upper[-par], pmax(lower[-par], pars[-par]))
        }
        ind <- seq_len(count)
        out[[par]] <- structure(list(tau = tau[ind], par.vals =
                                     par.vals[ind,  , drop=FALSE]),
                                class = "data.frame",
                                row.names = as.character(ind),
                                parameters = list(par = par,
                                std.err = std.err[par]))
        prof$setDefault()
    }
    names(out)[which] <- names(coef(fitted))[which]
    out <- out[which]
    attr(out, "original.fit") <- fitted
    attr(out, "summary") <- f.summary
    class(out) <- c("profile.nls", "profile")
    out
}

plot.profile.nls <-
    function(x, levels, conf = c(99, 95, 90, 80, 50)/100, absVal = TRUE,
             ylab = NULL, lty = 2, ...)
{
    obj <- x
    dfres <- attr(obj, "summary")$df[2L]
    if(missing(levels))
        levels <- sqrt(qf(pmax(0, pmin(1, conf)), 1, dfres))
    if(any(levels <= 0)) {
        levels <- levels[levels > 0]
        warning("levels truncated to positive values only")
    }
    mlev <- max(levels) * 1.05
    nm <- names(obj)
    opar <- par(mar = c(5, 4, 1, 1) + 0.1)
    if (absVal) {
        for (i in nm) {
            sp <- splines::interpSpline(obj[[i]]$par.vals[,i], obj[[i]]$tau)
            bsp <- splines::backSpline(sp)
            xlim <- predict(bsp, c(-mlev, mlev))$y
            if (is.na(xlim[1L])) xlim[1L] <- min(x[[i]]$par.vals[, i])
            if (is.na(xlim[2L])) xlim[2L] <- max(x[[i]]$par.vals[, i])
            dev.hold()
            if (is.null(ylab)) ylab <- expression(abs(tau))
            plot(abs(tau) ~ par.vals[, i], data = obj[[i]], xlab = i,
                 ylim = c(0, mlev), xlim = xlim, ylab = ylab, type = "n",
                 ...)
            avals <- rbind(as.data.frame(predict(sp)),
                           data.frame(x = obj[[i]]$par.vals[, i],
                                      y = obj[[i]]$tau))
            avals$y <- abs(avals$y)
            lines(avals[ order(avals$x), ], col = 4)
            abline(v = predict(bsp, 0)$y , col = 3, lty = lty)
            for(lev in levels) {
                pred <- predict(bsp, c(-lev, lev))$y
                lines(pred, rep.int(lev, 2), type = "h", col = 6, lty = lty)
                lines(pred, rep.int(lev, 2), type = "l", col = 6, lty = lty)
            }
            dev.flush()
        }
    } else {
        for (i in nm) {
            sp <- splines::interpSpline(obj[[i]]$par.vals[,i], obj[[i]]$tau)
            bsp <- splines::backSpline(sp)
            xlim <- predict(bsp, c(-mlev, mlev))$y
            if (is.na(xlim[1L])) xlim[1L] <- min(x[[i]]$par.vals[, i])
            if (is.na(xlim[2L])) xlim[2L] <- max(x[[i]]$par.vals[, i])
            dev.hold()
            if (is.null(ylab)) ylab <- expression(tau)
            plot(tau ~ par.vals[, i], data = obj[[i]], xlab = i,
                 ylim = c(-mlev, mlev), xlim = xlim, ylab = ylab, type = "n",
                 ...)
            lines(predict(sp), col = 4)
            abline(h = 0, col = 3, lty = lty)
            for(lev in levels) {
                pred <- predict(bsp, c(-lev, lev))$y
                lines(pred, c(-lev, lev), type = "h", col = 6, lty = lty)
            }
            dev.flush()
        }
    }
    par(opar)
}
