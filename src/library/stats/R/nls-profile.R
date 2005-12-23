###
### Profiling nonlinear least squares for R
###
### Copyright 1999-1999 Saikat DebRoy <saikat$stat.wisc.edu>,
###                     Douglas M. Bates <bates$stat.wisc.edu>,
### Copyright 2005      The R Development Core Team
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

profiler <- function(fitted, ...) UseMethod("profiler")

profiler.nls <- function(fitted, ...)
{
    fittedModel <- fitted$m
    algorithm <- fitted$call$algorithm
    if(is.null(algorithm)) algorithm <- "default"
    if(algorithm != "default")
        stop("only 'default' algorithm is currently supported")
    ctrl <- fitted$call$control
    trace <- fitted$call$trace
    defaultPars <- fittedPars <- fittedModel$getPars()
    defaultVary <- rep(TRUE, length(defaultPars))
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
                     assign("defaultVary", rep(TRUE, length(defaultPars)),
                            envir = thisEnv)
                 } else {
                     if(!missing(params)) {
                         if(length(params) != length(fittedPars))
                             stop("'params' has wrong length")
                         assign("defaultPars", params, envir = thisEnv)
                     }
                     if(!missing(varying)) {
                         if(is.numeric(varying)) {
                             if(!all(varying %in% (1:length(fittedPars))))
                                 stop("'varying' must be in 1:length(pars)")
                             varying <- !((1:length(fittedPars)) %in% varying)
                         } else if(is.logical(varying)) {
                             if(length(varying) != length(fittedPars))
                                 stop("'varying' has wrong length")
                         } else if(is.character(varying)) {
                             if(!all(varying %in% names(fittedPars)))
                                 stop("'varying' must be in 1:length(pars)")
                             varying <- !(names(fittedPars) %in% varying)
                         } else stop("'varying' must be logical, integer or character")
                         assign("defaultVary", varying, envir = thisEnv)
                     }
                 }
             },
                 getProfile = function(...)
             {
                 args <- list(...)
                 if(length(args) == 0) {
                     vary <- defaultVary
                     startPars <- defaultPars
                 } else if(length(args) == 2 && is.logical(args[[1]])) {
                     vary <- args[[1]]
                     params <- unlist(args[[2]])
                     startPars <- defaultPars
                     startPars[!vary] <- params
                 } else {
                     if(length(args) == 1 && is.list(args[[1]])) {
                         params <- unlist(args[[1]])
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
                 profiledModel <- .Call("nls_iter", fittedModel, ctrl, trace,
                           PACKAGE = "stats")
                 fstat <- (profiledModel$deviance()-S.hat)/s2.hat
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
  function(fitted, which = 1:npar, maxpts = 100, alphamax = 0.01,
           delta.t = cutoff/5, ...)
{
    f.summary <- summary(fitted)
    std.err <- f.summary$parameters[,"Std. Error"]
    npar <- length(std.err)
    nobs <- length(resid(fitted))
    cutoff <- sqrt(npar * qf(1 - alphamax, npar, nobs - npar))
    out <- list()
    prof <- profiler(fitted)
    on.exit(prof$setDefault())     # in case there is an abnormal exit
    for(par in which) {
        pars <- prof$getFittedPars()
        npar0 <- length(pars)  # less in a partially linear model
        prof$setDefault(varying = par)
        sgn <- -1
        count <- 1
        varying <- rep(TRUE, npar)
        varying[par] <- FALSE
        tau <- double(2 * maxpts)
        par.vals <- array(0, c(2 * maxpts, npar0), list(NULL, names(pars)))
        tau[1] <- 0
        par.vals[1,  ] <- pars
        base <- pars[par]
        profile.par.inc <- delta.t * std.err[par]
        pars[par] <- pars[par] - profile.par.inc
        while(count <= maxpts) {
            if(abs(pars[par] - base)/std.err[par] > 10 * cutoff) break
            count <- count + 1
            prof$setDefault(params = pars)
            ans <- prof$getProfile()
            tau[count] <- sgn*sqrt(ans$fstat)
            par.vals[count, ] <- pars <- ans$parameters
            if(abs(tau[count]) > cutoff) break
            pars <- pars + ((pars - par.vals[count - 1,  ]) * delta.t)/
                abs(tau[count] - tau[count - 1])
        }
        tau[1:count] <- tau[count:1]
        par.vals[1:count,  ] <- par.vals[count:1,  ]
        sgn <- 1
        newmax <- count + maxpts
        pars <- par.vals[count,  ]
        while(count <= newmax) {
            pars <- pars + ((pars - par.vals[count - 1,  ]) * delta.t)/
                abs(tau[count] - tau[count - 1])
            if(abs(pars[par] - base)/std.err[par] > 10 * cutoff) break
            count <- count + 1
            prof$setDefault(params = pars)
            ans <- prof$getProfile()
            tau[count] <- sgn*sqrt(ans$fstat)
            par.vals[count, ] <- pars <- ans$parameters
            if(abs(tau[count]) > cutoff) break
        }
        out[[par]] <- structure(list(tau = tau[1:count], par.vals =
                                     par.vals[1:count,  ]),
                                class = "data.frame",
                                row.names = as.character(1:count),
                                parameters = list(par = par,
                                std.err = std.err[par]))
        prof$setDefault()
    }
    names(out)[which] <- names(coef(fitted))[which]
    attr(out, "original.fit") <- fitted
    attr(out, "summary") <- f.summary
    class(out) <- c("profile.nls", "profile")
    out
}

plot.profile.nls <- function(x, levels, conf = c(99, 95, 90, 80, 50)/100,
                             nseg = 50, absVal = TRUE, ...)
{
    obj <- x
    dfres <- attr(obj, "summary")$df[2]
    confstr <- NULL
    if(missing(levels)) {
        levels <- sqrt(qf(pmax(0, pmin(1, conf)), 1, dfres))
        confstr <- paste(format(100 * conf), "%", sep = "")
    }
    if(any(levels <= 0)) {
        levels <- levels[levels > 0]
        warning("levels truncated to positive values only")
    }
    if(is.null(confstr)) {
        confstr <- paste(format(100 * pf(levels^2, 1, dfres)), "%", sep = "")
    }
    mlev <- max(levels) * 1.05
    nm <- names(obj)
    opar <- par(mar = c(5, 4, 1, 1) + 0.1)
    if (absVal) {
        for (i in seq(along = nm)) {
            sp <- splines::interpSpline(obj[[i]]$par.vals[,i], obj[[i]]$tau)
            bsp <- splines::backSpline(sp)
            xlim <- predict(bsp, c(-mlev, mlev))$y
            if (is.na(xlim[1])) xlim[1] <- min(x[[i]]$par.vals[, i])
            if (is.na(xlim[2])) xlim[2] <- max(x[[i]]$par.vals[, i])
            plot(abs(tau) ~ par.vals[, i], data = obj[[i]], xlab = nm[i],
                 ylim = c(0, mlev), xlim = xlim, ylab = expression(abs(tau)),
                 type = "n")
            avals <- rbind(as.data.frame(predict(sp)),
                           data.frame(x = obj[[i]]$par.vals[, i],
                                      y = obj[[i]]$tau))
            avals$y <- abs(avals$y)
            lines(avals[ order(avals$x), ], col = 4)
            abline(v = predict(bsp, 0)$y , col = 3, lty = 2)
            for(lev in levels) {
                pred <- predict(bsp, c(-lev, lev))$y
                lines(pred, rep(lev, 2), type = "h", col = 6, lty = 2)
                lines(pred, rep(lev, 2), type = "l", col = 6, lty = 2)
            }
        }
    } else {
        for (i in seq(along = nm)) {
            sp <- splines::interpSpline(obj[[i]]$par.vals[,i], obj[[i]]$tau)
            bsp <- splines::backSpline(sp)
            xlim <- predict(bsp, c(-mlev, mlev))$y
            if (is.na(xlim[1])) xlim[1] <- min(x[[i]]$par.vals[, i])
            if (is.na(xlim[2])) xlim[2] <- max(x[[i]]$par.vals[, i])
            plot(tau ~ par.vals[, i], data = obj[[i]], xlab = nm[i],
                 ylim = c(-mlev, mlev), xlim = xlim, ylab = expression(tau),
                 type = "n")
            lines(predict(sp), col = 4)
            abline(h = 0, col = 3, lty = 2)
            for(lev in  levels) {
                pred <- predict(bsp, c(-lev, lev))$y
                lines(pred, c(-lev, lev), type = "h", col = 6, lty = 2)
            }
        }
    }
    par(opar)
}
