require(methods)
setClass("mle", representation(call = "language",
                               coef = "numeric",
                               vcov = "matrix",
                               min = "numeric",
                               details = "list",
                               minuslogl = "function",
                               method="character"))

setClass("summary.mle", representation(call = "language",
                               coef = "matrix",
                               m2logL = "numeric"))  

setClass("profile.mle", representation(profile="list",
                                       summary="summary.mle"))

mle <- function(minuslogl, start=formals(minuslogl), method="BFGS",
                fixed=list(), ...){
    # Insert sanity checks here...
    call <- match.call()
    n <- names(fixed)
    start[n] <- NULL
    f <- function(p){
        l <- as.list(p)
        l[n] <- fixed
        do.call("minuslogl", l)
    }
    oout <- optim(unlist(start), f, method=method, hessian=TRUE, ...)
    coef <- oout$par
    vcov <- if(length(coef)) solve(oout$hessian) else matrix(numeric(0),0,0)
    min <-  oout$value
    new("mle", call=call, coef=coef, vcov=vcov, min=min, details=oout,
        minuslogl=minuslogl, method=method)
}

setMethod("show", "mle", function(object){
    cat("\nCall:\n")
    print(object@call)
    cat("\nCoefficients:\n")
    print(object@coef)
})

setMethod("show", "summary.mle", function(object){
    cat("Maximum likelihod estimation\n\nCall:\n")
    print(object@call)
    cat("\nCoeficients:\n")
    print(object@coef)
    cat("\n-2 log L:", object@m2logL, "\n")
})
 
setMethod("summary", "mle", function(object, ...){
    cmat <- cbind(Estimate = object@coef,
                  `Std. Error` = sqrt(diag(object@vcov)))
    m2logL <- 2*object@min
    new("summary.mle", call=object@call, coef=cmat, m2logL= m2logL) 
})   

setMethod("profile", "mle",
          function (fitted, which = 1:p, maxsteps = 100,
                    alpha = 0.01, zmax =sqrt(qchisq(1 - alpha/2, p)),
                    del = zmax/5, trace=FALSE,...)
{
    ## Profile the likelihood around its maximum
    ## Based on profile.glm in MASS
    summ <- summary(fitted)
    std.err <- summ@coef[, "Std. Error"]
    Pnames <- names(B0<-fitted@coef)
    pv0 <- t(as.matrix(B0))
    p <- length(Pnames)
    prof <- vector("list", length = length(which))
    names(prof) <- Pnames[which]
    for (i in which) {
        zi <- 0
        pvi <- pv0
        pi <- Pnames[i]
        for (sgn in c(-1, 1)) {
            if (trace)
                cat("\nParameter:", pi, c("down","up")[(sgn +
                  1)/2 + 1], "\n")
            step <- 0
            z <- 0

            ## FIXME: This logic is a bit frail in some cases with
            ## high parameter curvature. We should probably at least
            ## do something about cases where the mle call fails
            ## because the parameter gets stepped outside the domain.
            ## Also, the method (and boundaries in the case of
            ## L-BFGS-B) probably ought to be passed down.

            while ((step <- step + 1) < maxsteps && abs(z) <
                zmax) {
                bi <- B0[i] + sgn * step * del * std.err[i]
                fix <- list(bi)
                names(fix) <- pi
                pfit <- mle(fitted@minuslogl, start=as.list(B0), fixed=fix,...)
                zz <- 2*(pfit@min - fitted@min)
                ri <- pv0
                ri[, names(pfit@coef)] <- pfit@coef
                ri[, pi] <- bi
                pvi <- rbind(pvi, ri)

                if (zz > -0.001)
                  zz <- max(zz, 0)
                else stop("profiling has found a better solution, so original fit had not converged")
                z <- sgn * sqrt(zz)
                zi <- c(zi, z)
                if (trace) cat(bi, z, "\n")
            }
        }
        si <- order(zi)
        prof[[pi]] <- data.frame(z = zi[si])
        prof[[pi]]$par.vals <- pvi[si,, drop=FALSE]
    }
    new("profile.mle", profile = prof, summary = summ)
})

setMethod("plot",signature(x="profile.mle",y="missing"),
function (x, levels, conf = c(99, 95, 90, 80, 50)/100, nseg = 50,
    absVal = TRUE, ...)
{
    ## Plot profiled likelihood
    ## Based on profile.nls (package nls)
    require(splines)
    obj <- x@profile

    confstr <- NULL
    if (missing(levels)) {
        levels <- sqrt(qchisq(pmax(0, pmin(1, conf)), 1))
        confstr <- paste(format(100 * conf), "%", sep = "")
    }
    if (any(levels <= 0)) {
        levels <- levels[levels > 0]
        warning("levels truncated to positive values only")
    }
    if (is.null(confstr)) {
        confstr <- paste(format(100 * pchisq(levels^2, 1)),
            "%", sep = "")
    }
    mlev <- max(levels) * 1.05
    nm <- names(obj)
    opar <- par(mar = c(5, 4, 1, 1) + 0.1)
    if (absVal) {
        for (i in seq(along = nm)) {
            sp <- interpSpline(obj[[i]]$par.vals[, i], obj[[i]]$z)
            bsp <- backSpline(sp)
            xlim <- predict(bsp, c(-mlev, mlev))$y
            if (is.na(xlim[1]))
                xlim[1] <- min(obj[[i]]$par.vals[, i])
            if (is.na(xlim[2]))
                xlim[2] <- max(obj[[i]]$par.vals[, i])
            plot(abs(z) ~ par.vals[, i], data = obj[[i]], xlab = nm[i],
                ylim = c(0, mlev), xlim = xlim, ylab = expression(abs(z)),
                type = "n")
            avals <- rbind(as.data.frame(predict(sp)), data.frame(x = obj[[i]]$par.vals[,
                i], y = obj[[i]]$z))
            avals$y <- abs(avals$y)
            lines(avals[order(avals$x), ], col = 4)
            abline(v = predict(bsp, 0)$y, h=0, col = 3, lty = 2)
            for (lev in levels) {
                ## Note: predict may return NA if we didn't profile
                ## far enough in either direction. That's OK for the
                ## "h" part of the plot, but the horisontal line made
                ## with "l" disappears.
                pred <- predict(bsp, c(-lev, lev))$y
                lines(pred, rep(lev, 2), type = "h", col = 6,
                  lty = 2)
                pred <- ifelse(is.na(pred), xlim, pred)
                lines(pred, rep(lev, 2), type = "l", col = 6,
                  lty = 2)
            }
        }
    }
    else {
        for (i in seq(along = nm)) {
            sp <- interpSpline(obj[[i]]$par.vals[, i], obj[[i]]$z)
            bsp <- backSpline(sp)
            xlim <- predict(bsp, c(-mlev, mlev))$y
            x0 <- predict(bsp, 0)$y
            if (is.na(xlim[1]))
                xlim[1] <- min(obj[[i]]$par.vals[, i])
            if (is.na(xlim[2]))
                xlim[2] <- max(obj[[i]]$par.vals[, i])
            plot(z ~ par.vals[, i], data = obj[[i]], xlab = nm[i],
                ylim = c(-mlev, mlev), xlim = xlim, ylab = expression(z),
                type = "n")
            lines(predict(sp), col = 4)
            abline(h = 0, v=x0, col = 3, lty = 2)
            for (lev in levels) {
                pred <- predict(bsp, c(-lev, lev))$y
                lines(pred, c(-lev, lev), type = "h", col = 6,
                  lty = 2)
                pred <- ifelse(is.na(pred), xlim, pred)
                lines(c(x0,pred[2]), rep(lev, 2), type = "l", col = 6,
                  lty = 2)
                lines(c(pred[1],x0), rep(-lev, 2), type = "l", col = 6,
                  lty = 2)
            }
        }
    }
    par(opar)
})

setMethod("confint","profile.mle",
function (object, parm, level = 0.95, ...)
{
    ## Calculate confidence intervals based on likelihood
    ## profiles
    of <- object@summary
    pnames <- rownames(of@coef)
    if (missing(parm))
        parm <- seq(along=pnames)
    if (is.character(parm))
        parm <- match(parm, pnames, nomatch = 0)
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(round(100 * a, 1), "%")
    ci <- array(NA, dim = c(length(parm), 2), dimnames = list(pnames[parm],
        pct))
    cutoff <- qnorm(a)
    for (pm in parm) {
        pro <- object@profile[[pnames[pm]]]
        if (length(pnames) > 1)
            sp <- spline(x = pro[, "par.vals"][, pm], y = pro[,
                1])
        else sp <- spline(x = pro[, "par.vals"], y = pro[, 1])
        ci[pnames[pm], ] <- approx(sp$y, sp$x, xout = cutoff)$y
    }
    drop(ci)
})

setMethod("confint","mle",
function (object, parm, level = 0.95, ...)
{
    cat("Profiling...\n")
    pr <- profile(object)
    confint(pr)
})

.onLoad <- function(lib, pkg) require(methods)
