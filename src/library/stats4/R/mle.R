#  File src/library/stats4/R/mle.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

setClass("mle", representation(call = "language",
                               coef = "numeric",
                               fullcoef = "numeric",
                               fixed = "numeric",
                               vcov = "matrix",
                               min = "numeric",
                               details = "list",
                               minuslogl = "function",
                               nobs = "integer",
                               method = "character"))

setClass("summary.mle", representation(call = "language",
                               coef = "matrix",
                               m2logL = "numeric"))

setClass("profile.mle", representation(profile="list",
                                       summary="summary.mle"))

mle <- function(minuslogl, start, optim = stats::optim,
                method = if(!useLim) "BFGS" else "L-BFGS-B",
                fixed = list(), nobs, lower, upper, ...)
{
    ## Insert more sanity checks here?
    call <- match.call()

    ## Need some care here: formals(minuslogl) may have
    ## - default expressions, which need evaluation (to know the lengths)
    ## - missing defaults, which gives an error if evaluated, assume scalar
    ## It is assumed that default expressions can be evaluated in the environment
    ## of "minuslogl". I.e., they cannot refer to internal variables.
    ## If defaults are missing, they _must_ be supplied by "start"
    ## (We could be marginally smarter about this and infer lengths from "start",
    ## but only if "start" is given as a list.)
    fullcoef <- lapply(formals(minuslogl), function(i)
        if (mode(i) == "name" && as.character(i) == "")
            NA_real_ else eval(i, envir=environment(minuslogl))
        )

    nf <- names(fullcoef)
    signature <- list(lengths = sapply(fullcoef, length),
                      names = lapply(fullcoef, names))
    sigindex <- factor(rep(nf, signature$lengths), levels=nf)
    npars <- sum(signature$lengths)
    l2v <- function(l, default=NA_real_) { # not just NA (logical) because of S4 check
        nm <- names(l)
        if (length(l) && is.null(nm)) # Don't require names on empty list
            stop("named list expected")
        oo <- match(nm, nf)
        if (anyNA(oo))
            stop("some named values are not arguments to the supplied log-likelihood")
        ## First make a "skeleton" list to be filled in with values from "l".
        ## Elements in "skeleton" but not in "l" are filled with the default value 
        skeleton <- lapply(signature$lengths, rep_len, x=default)
        for (i in names(skeleton))
            names(skeleton[[i]]) <- signature$names[[i]]
        skeleton[oo] <- l
        ## There is a marginal risk that unlisting "skeleton" results in non-unique names
        ## (e.g., unlist(list(beta=1:2, beta1=3)) uses "beta1" twice).
        v <- unlist(skeleton)
        names(v) <- make.unique(names(v))
        v
    }
    
    v2l <- function(v)
        split(v, sigindex)
        
    if (is.list(fixed))      
        fixed <- l2v(fixed)
    
    isfixed <- !is.na(fixed)

    if (missing(start))
        start <- l2v(fullcoef)
    else if (is.list(start)){
        nm <- names(start)
        if (anyNA(match(nf, nm))) { # get from defaults
            extras <- setdiff(nf, nm)
            start[extras] <- fullcoef[extras]
        }
        start <- l2v(start)
    }
    if (length(start) != npars)
        stop ("Mismatch in length of start values")
            
    start <- start[!isfixed]

    ## "Onion skin" routine:
    ## Call minuslogl with vector arg, possibly inserting fixed arg values
    f <- function(p){
        v <- numeric(npars)
        v[isfixed] <- fixed[isfixed]
        v[!isfixed] <- p
        l <- v2l(v)
        do.call("minuslogl", l)
    }
    useLim <- !missing(lower) || !missing(upper) 
    if (useLim)
    {
        if (missing(lower)) lower <- rep_len(-Inf, npars)
        if (missing(upper)) upper <- rep_len(Inf, npars)
        if (is.list(lower)) lower <- l2v(lower, -Inf)
        if (is.list(upper)) upper <- l2v(upper, Inf)
        if (any(isfixed)) # any parms kept fixed
        {
            if (!all(lower[isfixed] <= fixed[isfixed] & fixed[isfixed] <= upper[isfixed]))
                stop("fixed values violate constraints")
            lower <- lower[!isfixed]
            upper <- upper[!isfixed]
        }
        if (!all(lower <= start & start <= upper))
        {
            warning("start values do not satisfy constraints")
            start <- pmin(pmax(start, lower), upper)
        }
    }
    oout <-
        if (length(start))
            if(!useLim)
                optim(start, f, method = method, hessian = TRUE, ...)
            else
                optim(start, f,  method = method, hessian = TRUE, lower = lower, upper = upper, ...)
        else list(par = numeric(), value = f(start))
    coef <- oout$par
    vcov <- if(length(coef)) solve(oout$hessian) else matrix(numeric(), 0L, 0L)
    min <- oout$value
    fullcoef <- l2v(fullcoef) # to get the names right
    fullcoef[!isfixed] <- coef
    fullcoef[isfixed] <- fixed[isfixed]
    new("mle", call = call, coef = coef, fullcoef = fullcoef, fixed = fixed,
        vcov = vcov, min = min, details = oout, minuslogl = minuslogl,
        nobs = if(missing(nobs)) NA_integer_ else nobs,
        method = method)
}

setGeneric("coef")
setMethod("coef", "mle", function(object) object@fullcoef )
setMethod("coef", "summary.mle", function(object) object@coef )

setMethod("show", "mle", function(object){
    cat("\nCall:\n")
    print(object@call)
    cat("\nCoefficients:\n")
    print(coef(object))
})

setMethod("show", "summary.mle", function(object){
    cat("Maximum likelihood estimation\n\nCall:\n")
    print(object@call)
    cat("\nCoefficients:\n")
    print(coef(object))
    cat("\n-2 log L:", object@m2logL, "\n")
})

setGeneric("summary")
setMethod("summary", "mle", function(object, ...){
    cmat <- cbind(Estimate = object@coef,
                  `Std. Error` = sqrt(diag(object@vcov)))
    m2logL <- 2*object@min
    new("summary.mle", call = object@call, coef = cmat, m2logL = m2logL)
})

setGeneric("profile")
setMethod("profile", "mle",
          function (fitted, which = 1L:p, maxsteps = 100,
                    alpha = 0.01, zmax = sqrt(qchisq(1 - alpha, 1L)),
                    del = zmax/5, trace = FALSE, ...)
{
    onestep <- function(step)
    {
        bi <- B0[i] + sgn * step * del * std.err[i]
        fix <- fix0
        fix[[ixfix]] <- bi

        call$fixed <- fix
        
        pfit <- tryCatch(eval.parent(call, 2L), error = identity)
        if(inherits(pfit, "error")) return(NA)
        else {
            zz <- 2*(pfit@min - fitted@min)
            ri <- pv0
            ri[, names(pfit@coef)] <- pfit@coef
            ri[, pi] <- bi

            if (zz > -0.001)
                zz <- max(zz, 0)
            else stop("profiling has found a better solution, so original fit had not converged")
            z <- sgn * sqrt(zz)
            pvi <<- rbind(pvi, ri)
            zi <<- c(zi, z)
        }
        if (trace) cat(bi, z, "\n")
        z
    }
    ## Profile the likelihood around its maximum
    ## Based on profile.glm in MASS
    summ <- summary(fitted)
    std.err <- summ@coef[, "Std. Error"]
    Pnames <- names(B0 <- fitted@coef)
    B0full <- fitted@fullcoef
    pv0 <- t(as.matrix(B0))
    p <- length(Pnames)
    prof <- vector("list", length = length(which))
    names(prof) <- Pnames[which]
    call <- fitted@call
    call$minuslogl <- fitted@minuslogl
    ndeps <- eval.parent(call$control$ndeps)
    parscale <- eval.parent(call$control$parscale)
    fix0 <- fitted@fixed
    isfixed0 <- !is.na(fix0)
    ixnfix0 <- seq_along(fix0)[!isfixed0] # index of (initially) non-fixed parameters
    for (i in which) {
        zi <- 0
        pvi <- pv0
        pi <- Pnames[[i]]
        ixfix <- ixnfix0[[i]] # index of add'l param to hold fixed
        if (!is.null(ndeps)) call$control$ndeps <- ndeps[-i]
        if (!is.null(parscale)) call$control$parscale <- parscale[-i]
        for (sgn in c(-1, 1)) {
            if (trace)
                cat("\nParameter:", pi, c("down", "up")[(sgn + 1)/2 + 1], "\n")
            step <- 0
            z <- 0

            ## This logic was a bit frail in some cases with
            ## high parameter curvature. We should probably at least
            ## do something about cases where the mle call fails
            ## because the parameter gets stepped outside the domain.
            ## (We now have.)

            call$start <- B0full
            lastz <- 0
            while ((step <- step + 1) < maxsteps && abs(z) < zmax) {
                z <- onestep(step)
                if(is.na(z)) break
                lastz <- z
            }
            if(abs(lastz) < zmax) {
                ## now let's try a bit harder if we came up short
                for(dstep in c(0.2, 0.4, 0.6, 0.8, 0.9)) {
                    z <- onestep(step - 1 + dstep)
                    if(is.na(z) || abs(z) > zmax) break
                }
            } else if(length(zi) < 5L) { # try smaller steps
                mxstep <- step - 1L
                step <- 0.5
                while ((step <- step + 1L) < mxstep) onestep(step)
            }
        }
        si <- order(pvi[, i])
        prof[[pi]] <- data.frame(z = zi[si])
        prof[[pi]]$par.vals <- pvi[si,, drop=FALSE]
    }
    new("profile.mle", profile = prof, summary = summ)
})

setGeneric("plot")
setMethod("plot", signature(x="profile.mle", y="missing"),
function (x, levels, conf = c(99, 95, 90, 80, 50)/100, nseg = 50,
          absVal = TRUE, ...)
{
    ## Plot profiled likelihood
    ## Based on profile.nls (package stats)
    obj <- x@profile

    confstr <- NULL
    if (missing(levels)) {
        levels <- sqrt(qchisq(pmax(0, pmin(1, conf)), 1))
        confstr <- paste0(format(100 * conf), "%")
    }
    if (any(levels <= 0)) {
        levels <- levels[levels > 0]
        warning("levels truncated to positive values only")
    }
    if (is.null(confstr)) {
        confstr <- paste0(format(100 * pchisq(levels^2, 1)), "%")
    }
    mlev <- max(levels) * 1.05
    nm <- names(obj)
    opar <- par(mar = c(5, 4, 1, 1) + 0.1)
    if (absVal) {
        ## OBS: it is important to use the actual names for indexing,
        ## in case profile(....,which=....) was used
        for (i in nm) {
            ## <FIXME> This does not need to be monotonic
            sp <- splines::interpSpline(obj[[i]]$par.vals[, i], obj[[i]]$z,
                               na.action=na.omit)
            bsp <- splines:: backSpline(sp)
            ## </FIXME>
            xlim <- predict(bsp, c(-mlev, mlev))$y
            if (is.na(xlim[1L]))
                xlim[1L] <- min(obj[[i]]$par.vals[, i])
            if (is.na(xlim[2L]))
                xlim[2L] <- max(obj[[i]]$par.vals[, i])
            dev.hold()
            plot(abs(z) ~ par.vals[, i], data = obj[[i]], xlab = i,
                ylim = c(0, mlev), xlim = xlim, ylab = expression(abs(z)),
                type = "n")
            avals <- rbind(as.data.frame(predict(sp)),
                           data.frame(x = obj[[i]]$par.vals[, i],
                                      y = obj[[i]]$z))
            avals$y <- abs(avals$y)
            lines(avals[order(avals$x), ], col = 4)
            abline(v = predict(bsp, 0)$y, h=0, col = 3, lty = 2)
            for (lev in levels) {
                ## Note: predict may return NA if we didn't profile
                ## far enough in either direction. That's OK for the
                ## "h" part of the plot, but the horizontal line made
                ## with "l" disappears.
                pred <- predict(bsp, c(-lev, lev))$y
                lines(pred, rep(lev, 2), type = "h", col = 6, lty = 2)
                pred <- ifelse(is.na(pred), xlim, pred)
                lines(pred, rep(lev, 2), type = "l", col = 6, lty = 2)
            }
            dev.flush()
        }
    }
    else {
        for (i in nm) {
            ## <FIXME> This does not need to be monotonic
            sp <- splines::interpSpline(obj[[i]]$par.vals[, i], obj[[i]]$z,
                               na.action=na.omit)
            bsp <- splines::backSpline(sp)
            ## </FIXME>
            xlim <- predict(bsp, c(-mlev, mlev))$y
            x0 <- predict(bsp, 0)$y
            if (is.na(xlim[1L]))
                xlim[1L] <- min(obj[[i]]$par.vals[, i])
            if (is.na(xlim[2L]))
                xlim[2L] <- max(obj[[i]]$par.vals[, i])
            dev.hold()
            plot(z ~ par.vals[, i], data = obj[[i]], xlab = i,
                ylim = c(-mlev, mlev), xlim = xlim, ylab = expression(z),
                type = "n")
            lines(predict(sp), col = 4)
            abline(h = 0, v=x0, col = 3, lty = 2)
            for (lev in levels) {
                pred <- predict(bsp, c(-lev, lev))$y
                lines(pred, c(-lev, lev), type = "h", col = 6, lty = 2)
                pred <- ifelse(is.na(pred), xlim, pred)
                lines(c(x0,pred[2L]), rep(lev, 2), type = "l", col = 6, lty = 2)
                lines(c(pred[1L],x0), rep(-lev, 2), type = "l", col = 6, lty = 2)
            }
            dev.flush()
        }
    }
    par(opar)
})

setGeneric("confint")
setMethod("confint", "profile.mle",
function (object, parm, level = 0.95, ...)
{
    ## Calculate confidence intervals based on likelihood
    ## profiles
    of <- object@summary
    pnames <- rownames(of@coef)
    if (missing(parm))
        parm <- seq_along(pnames)
    if (is.character(parm))
        parm <- match(parm, pnames, nomatch = 0L)
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(round(100 * a, 1), "%")
    ci <- array(NA, dim = c(length(parm), 2L),
                dimnames = list(pnames[parm], pct))
    cutoff <- qnorm(a)
    for (pm in parm) {
        pro <- object@profile[[pnames[pm]]]
        sp <- if (length(pnames) > 1L)
            spline(x = pro[, "par.vals"][, pm], y = pro[, 1L])
        else spline(x = pro[, "par.vals"], y = pro[, 1L])
        ci[pnames[pm], ] <- approx(sp$y, sp$x, xout = cutoff)$y
    }
    drop(ci)
})

setMethod("confint", "mle",
function (object, parm, level = 0.95, ...)
{
    cat("Profiling...\n")
    confint(profile(object), alpha = (1 - level)/4, parm, level, ...)
})

setGeneric("nobs")
setMethod("nobs", "mle", function (object, ...)
    if("nobs" %in% slotNames(object)) object@nobs else NA_integer_)

setGeneric("logLik")
setMethod("logLik", "mle", function(object, ...) {
    if(!missing(...))
	warning("extra arguments discarded")
    val <- -object@min
    if ("nobs" %in% slotNames(object) && # introduced in 2.13.0
        !is.na(no <- object@nobs)) attr(val, "nobs") <- no
    attr(val, "df") <- length(object@coef)
    class(val) <- "logLik"
    val
})

setGeneric("vcov")
setMethod("vcov", "mle", function (object, ...) object@vcov)

setGeneric("update")
setMethod("update", "mle", function (object, ..., evaluate = TRUE)
{
    call <- object@call
    extras <- match.call(expand.dots = FALSE)$...
    if (length(extras) ) {
        existing <- !is.na(match(names(extras), names(call)))
        for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if (any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
        }
    }
    if (evaluate) eval(call, parent.frame()) else call
})
