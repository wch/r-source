##  File src/library/stats/R/glm-profile.R
##  Part of the R package, https://www.R-project.org

## Based on
# File MASS/profiles.q copyright (C) 1996 D. M. Bates and W. N. Venables.
#
# port to R by B. D. Ripley copyright (C) 1998
#
# corrections copyright (C) 2000,3,6,7 B. D. Ripley

## Modified for Rao Score test ("test=" argument) by Peter Dalgaard 2023
## Modifications Copyright (C) 2023 The R Core Team

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

profile.glm <- function(fitted, which = 1:p, alpha = 0.01,
			maxsteps = 10, del = zmax/5, trace = FALSE, test = c("LRT", "Rao"), ...)
{
    test <- match.arg(test)
    Pnames <- names(B0 <- coef(fitted))
    nonA <- !is.na(B0)
    pv0 <- t(as.matrix(B0))
    p <- length(Pnames)
    if(is.character(which)) which <- match(which, Pnames)
    summ <- summary(fitted)
    std.err <- summ$coefficients[, "Std. Error", drop = FALSE] # unaliased only
    mf <- model.frame(fitted)
    Y <- model.response(mf) ### change
    n <- NROW(Y)            ### change: NROW() rather than length()
    O <- model.offset(mf)
    if(!length(O)) O <- rep(0, n)
    W <- model.weights(mf)
    if(length(W) == 0L) W <- rep(1, n)
    OriginalDeviance <- deviance(fitted)
    DispersionParameter <- summ$dispersion
    X <- model.matrix(fitted)
    fam <- family(fitted)
    switch(fam$family,
           binomial = ,
           poisson = ,
           `Negative Binomial` = {
               zmax <- sqrt(qchisq(1 - alpha, 1))
               profName <- "z"
           }
           ,
           gaussian = ,
           quasi = ,
           inverse.gaussian = ,
           quasibinomial = ,
           quasipoisson = ,
       {
	   zmax <- sqrt(qf(1 - alpha, 1, n - p))
	   profName <- "tau"
       }
           )
    prof <- vector("list", length=length(which))
    names(prof) <- Pnames[which]
    for(i in which) {
        if(!nonA[i]) next
        zi <- 0
        pvi <- pv0
        a <- nonA; a[i] <- FALSE
        Xi <- X[, a, drop = FALSE]
        pi <- Pnames[i]
        for(sgn in c(-1, 1)) {
            if(trace)
                message("\nParameter: ", pi, " ",
                        c("down", "up")[(sgn + 1)/2 + 1])
            step <- 0
            z <- 0
            ## LP is the linear predictor including offset.
            ## we need to take care to avoid aliased-out coefficients.
            LP <- X[, nonA, drop = FALSE] %*% B0[nonA] + O
            while((step <- step + 1) < maxsteps && abs(z) < zmax) {
                bi <- B0[i] + sgn * step * del * std.err[Pnames[i], 1]
                o <- O + X[, i] * bi
                ## call to glm.fit.null not needed from 1.4.1 on
                
                ## FIXME: in non-canonical models, o or LP+o can end
                ##        up outside valid range. (E.g. binomial("identity").) This
                ##        really should be checked for and the range adjusted.
                ##        
                
                fm <- glm.fit(x = Xi, y = Y, weights = W, etastart = LP,
                              offset = o, family = fam,
                              control = fitted$control)
                LP <- Xi %*% fm$coefficients + o
                ri <- pv0
                ri[, names(coef(fm))] <- coef(fm)
                ri[, pi] <- bi
                pvi <- rbind(pvi, ri)
                zz <- (fm$deviance - OriginalDeviance)/DispersionParameter
                if(zz > - 1e-3) zz <- max(zz, 0)
                else stop("profiling has found a better solution, so original fit had not converged")
                if (test == "Rao"){
                    ## Local fit to residual, using WLS:
                    r <- fm$residuals
                    w <- fm$weights
                    fml <- glm.fit(x = X, y = r, weights = w,
                                   control = fitted$control, intercept=FALSE)
                    zz <- (fml$null.deviance - fml$deviance)/DispersionParameter
                    zz <- max(zz, 0) # small negative can happen at bi == 0...
                }
                z <- sgn * sqrt(zz)
                zi <- c(zi, z)
            }
        }
        si <- order(zi)
        prof[[pi]] <- structure(data.frame(zi[si]), names = profName)
        prof[[pi]]$par.vals <- pvi[si, ,drop=FALSE]
    }
    val <- structure(prof, original.fit = fitted, summary = summ)
    class(val) <- c("profile.glm", "profile")
    attr(val, "test") <- test
    val
}

plot.profile <-
  ## R version: non-Trellis-based replacement for plot.profile
  function(x, ...)
{
    nulls <- sapply(x, is.null)
    if (all(nulls)) return(NULL)
    x <- x[!nulls]
    nm <- names(x)
    nr <- ceiling(sqrt(length(nm)))
    oldpar <- par(mfrow = c(nr, nr))
    on.exit(par(oldpar))
    for(nm in names(x)) {
        tau <- x[[nm]][[1L]]
        parval <- x[[nm]][[2L]][, nm]
        dev.hold()
        plot(parval, tau, xlab = nm, ylab = "tau", type="n")
        ## allow for profiling failures
        if(sum(tau == 0) == 1) points(parval[tau == 0], 0, pch = 3)
        splineVals <- spline(parval, tau)
        lines(splineVals$x, splineVals$y)
        dev.flush()
    }
}

pairs.profile <-
    ## Another plot method for profile objects showing pairwise traces.
    ## Recommended only for diagnostic purposes.

    ## pd: support added for plotting a subset of parameter traces, 
    ## defaulting to those from the which= argument in profile()

    ## FIXME (pd): Could have markings according to approx. 2D
    ## conf. regions supplementing/replacing the equidistant steps
    
function(x, colours = 2:3, which=names(x), ...)
{
    parvals <- lapply(x, "[[", "par.vals")
    rng <- apply(do.call("rbind", parvals), 2L, range, na.rm = TRUE)
    Pnames <- colnames(rng)
    npar <- length(Pnames)
    force(which)
    if (is.character(which)) which <- match(which, Pnames)
    stopifnot(all(!is.na(which)))
    stopifnot(all(which %in% 1:npar))
    stopifnot((nw <- length(which)) >= 2)
    coefs <- coef(attr(x, "original.fit"))
    form <- paste(as.character(formula(attr(x, "original.fit")))[c(2, 1, 3)],
                  collapse = "")
    oldpar <- par(mar = c(0, 0, 0, 0), mfrow = c(1, 1),
                  oma = c(3, 3, 6, 3), las = 1)
    on.exit(par(oldpar))
    ##
    ## The following dodge ensures that the plot region is square
    ##
    fin <- par("fin")
    dif <- (fin[2L] - fin[1L])/2
    if(dif > 0) adj <- c(dif, 0, dif, 0)
    else adj <- c(0,  - dif, 0,  - dif)
    par(omi = par("omi") + adj)
    ##
    ##
    cex <- 1 + 1/nw
    frame()
    mtext(form, side = 3, line = 3, cex = 1.5, outer = TRUE)
    del <- 1/nw
    for(i in 1L:nw) {
        ci <- nw - i
        pi <- Pnames[which[i]]
        for(j in 1L:nw) {
            dev.hold()
            pj <- Pnames[which[j]]
            par(fig = del * c(j - 1, j, ci, ci + 1))
            if(i == j) {
                par(new=TRUE)
                plot(rng[, pj], rng[, pi], axes = FALSE,
                     xlab = "", ylab = "", type = "n")
                op <- par(usr = c(-1, 1, -1, 1))
                text(0, 0, pi, cex = cex, adj = 0.5)
                par(op)
            } else {
                col <- colours
                if(i < j) col <- col[2:1]
		par(new=TRUE)
                if(!is.null(parvals[[pj]])) {
                    plot(spline(x <- parvals[[pj]][, pj],
                                y <- parvals[[pj]][, pi]),
                         type = "l", xlim = rng[, pj],
                         ylim = rng[, pi], axes = FALSE,
                         xlab = "", ylab = "", col = col[2L])
                    pu <- par("usr")
                    smidge <- 2/100 * (pu[4L] - pu[3L])
                    segments(x, pmax(pu[3L], y - smidge), x,
                             pmin(pu[4L], y + smidge))
                } else
                plot(rng[, pj], rng[, pi], axes = FALSE,
                     xlab = "", ylab = "", type = "n")
                if(!is.null(parvals[[pi]])) {
                    lines(x <- parvals[[pi]][, pj], y <- parvals[[pi]][, pi],
                          type = "l", col = col[1L])
                    pu <- par("usr")
                    smidge <- 2/100 * (pu[2L] - pu[1L])
                    segments(pmax(pu[1L], x - smidge), y, pmin(pu[2L], x + smidge), y)
                }
                points(coefs[pj], coefs[pi], pch = 3, cex = 3)
            }
            if(i == nw) axis(1)
            if(j == 1) axis(2)
            if(i == 1) axis(3)
            if(j == npar) axis(4)
            dev.flush()
        }
    }
    par(fig = c(0, 1, 0, 1))
    invisible(x)
}
