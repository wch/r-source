#  File src/library/stats/R/confint.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1994-2003 W. N. Venables and B. D. Ripley
#  Copyright (C) 2003-2012 The R Core Team
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

format.perc <- function(probs, digits)
    ## Not yet exported, maybe useful in other contexts:
    ## quantile.default() sometimes uses a version of it
    paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
	  "%")

confint.lm <- function(object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    pnames <- names(cf)
    if(missing(parm)) parm <- pnames
    else if(is.numeric(parm)) parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qt(a, object$df.residual) # difference from default method
    pct <- format.perc(a, 3)
    ci <- array(NA, dim = c(length(parm), 2L),
		dimnames = list(parm, pct))
    ses <- sqrt(diag(vcov(object)))[parm] # gives NA for aliased parms
    ci[] <- cf[parm] + ses %o% fac
    ci
}

## loading the MASS namespace will overwrite these in the registry,
## so there are stubs to do just that.
confint.glm <- function(object, parm, level = 0.95, ...)
{
    if(!requireNamespace("MASS", quietly = TRUE))
        stop("package 'MASS' must be installed")
    confint.glm <- getS3method("confint", "glm")
    confint.glm(object, parm, level, ...)
}

confint.nls <- function(object, parm, level = 0.95, ...)
{
    if(!requireNamespace("MASS", quietly = TRUE))
        stop("package 'MASS' must be installed")
    confint.nls <- getS3method("confint", "nls")
    confint.nls(object, parm, level, ...)
}

confint.default <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    pnames <- names(cf)
    if(missing(parm)) parm <- pnames
    else if(is.numeric(parm)) parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L),
		dimnames = list(parm, pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}
