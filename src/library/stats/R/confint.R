#  File src/library/stats/R/confint.R
#  Part of the R package, https://www.R-project.org

# Portions copied from MASS/R/confint.R
#
#  Copyright (C) 2003-2023 The R Core Team
#  Copyright (C) 1994-2006 W. N. Venables and B. D. Ripley
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

confint <- function(object, parm, level = 0.95, ...) UseMethod("confint")

## *different* and simpler than  format_perc()  in ./quantile.R for quantile.default()
.format_perc <- function(probs, digits)
    paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
	  "%")

confint.lm <- function(object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    ses <- sqrt(diag(vcov(object))) # gives NA for aliased parms
    pnames <- names(ses) # ok for "mlm", too
    if(is.matrix(cf)) cf <- setNames(as.vector(cf), pnames) # for "mlm"
    if(missing(parm)) parm <- pnames
    else if(is.numeric(parm)) parm <- pnames[parm]
    ## else 'parm' must contain parameter names matching those in 'pnames'
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qt(a, object$df.residual) # difference from default method
    pct <- .format_perc(a, 3)
    ci <- array(NA_real_,
                dim = c(length(parm), 2L), dimnames = list(parm, pct))
    ci[] <- cf[parm] + ses[parm] %o% fac
    ci
}

##Copied from MASS, modified for score tests ("Rao") by pd July 2023
confint.glm <- function(object, parm, level = 0.95, trace = FALSE, test = c("LRT", "Rao"), ...)
{
    test <- match.arg(test)
    pnames <- names(coef(object))
    if(missing(parm)) parm <- seq_along(pnames)
    else if(is.character(parm))  parm <- match(parm, pnames, nomatch = 0L)
    message("Waiting for profiling to be done...")
    utils::flush.console()
    object <- profile(object, which = parm, alpha = (1. - level)/4.,
                      trace = trace, test = test)
    confint(object, parm=parm, level=level, trace = trace, test = test, ...)
}

confint.profile.glm <- function(object, parm = seq_along(pnames), level = 0.95,
                                trace = FALSE, test = attr(object, "test"), ...)
{
    if (is.null(test)) test <- "LRT" # For backwards compatibility

    of <- attr(object, "original.fit")
    pnames <- names(coef(of))
    if(is.character(parm))  parm <- match(parm, pnames, nomatch = 0L)

    if (is.null(attr(object,"test"))) # For backwards compatibility
        attr(object,"test") <- "LRT" 
    if (test != attr(object, "test")) {
        message("Reprofiling for ", test, " statistic. Waiting...")
        utils::flush.console()
        object <- profile(of, which = parm, alpha = (1. - level)/4.,
                          trace = trace, test = test)
    }
 
    a <- (1-level)/2
    a <- c(a, 1-a)
    pct <- paste(round(100*a, 1), "%")
    ci <- array(NA, dim = c(length(parm), 2L),
                dimnames = list(pnames[parm], pct))
    ## Hmm: This could have a df correction if there's an estimated dispersion parameter
    ## Leave for now. -pd
    cutoff <- qnorm(a)
    for(pm in parm) {
        pro <- object[[ pnames[pm] ]]
        ## skip aliased params
        if(is.null(pro)) next
        if(length(pnames) > 1L)
            sp <- spline(x = pro[, "par.vals"][, pm], y = pro[, 1])
        else sp <- spline(x = pro[, "par.vals"], y = pro[, 1])
        ci[pnames[pm], ] <- approx(sp$y, sp$x, xout = cutoff)$y
    }
    drop(ci)
    ## This could be nice, but maybe also disruptive...
    # attr(ci, "test") <- "test" 
}

## loading the MASS namespace will overwrite these in the registry.
## stub is a specialized version of MASS:::confint.xxx with specific message

## Disabled for glm -pd
#confint.glm <- function(object, parm, level = 0.95, ...)
#{
#    if(!requireNamespace("MASS", quietly = TRUE))
#        stop("package 'MASS' must be installed")
#    asNamespace("MASS")$confint.glm(object, parm, level, ...)
#}

#confint.nls <- function(object, parm, level = 0.95, ...)
#{
#    if(!requireNamespace("MASS", quietly = TRUE))
#        stop("package 'MASS' must be installed")
#    asNamespace("MASS")$confint.nls(object, parm, level, ...)
#}

confint.nls <-
  function(object, parm, level = 0.95, ...)
{
  pnames <- names(coef(object))
  if(missing(parm)) parm <- seq_along(pnames)
  if(is.numeric(parm))  parm <- pnames[parm]
  message("Waiting for profiling to be done...")
  utils::flush.console()
  object <- profile(object, which = parm, alphamax = (1. - level)/4.)
  confint(object, parm=parm, level=level, ...)
}

confint.profile.nls <-
  function(object, parm = seq_along(pnames), level = 0.95, ...)
{
  pnames <- names(object) # non-linear pars only
  ncoefs <- length(coef(attr(object, "original.fit")))
  of <- attr(object, "original.fit")
  if(is.numeric(parm))  parm <- pnames[parm]
  ## drop any plinear paramaters
  parm <- parm[parm %in% pnames]
  n <- length(fitted(of)) - length(of$m$getPars())
  a <- (1-level)/2
  a <- c(a, 1-a)
  pct <- paste(round(100*a, 1), "%", sep = "")
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  cutoff <- qt(a, n)
  for(pm in parm) {
    pro <- object[[pm]]
    sp <- if(ncoefs > 1) spline(x = pro[, "par.vals"][, pm], y = pro$tau)
    else spline(x = pro[, "par.vals"], y = pro$tau)
    ci[pm, ] <- approx(sp$y, sp$x, xout = cutoff)$y
  }
  drop(ci)
}


confint.default <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    pnames <- names(cf)
    if(missing(parm)) parm <- pnames
    else if(is.numeric(parm)) parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- .format_perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L),
		dimnames = list(parm, pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}
