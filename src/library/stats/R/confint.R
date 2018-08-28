#  File src/library/stats/R/confint.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2003-2018 The R Core Team
#  Copyright (C) 1994-2003 W. N. Venables and B. D. Ripley
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
    ses <- sqrt(diag(vcov(object))) # gives NA for aliased parms
    pnames <- names(ses) # ok for "mlm", too
    if(is.matrix(cf)) cf <- setNames(as.vector(cf), pnames) # for "mlm"
    if(missing(parm)) parm <- pnames
    else if(is.numeric(parm)) parm <- pnames[parm]
    ## else 'parm' must contain parameter names matching those in 'pnames'
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qt(a, object$df.residual) # difference from default method
    pct <- format.perc(a, 3)
    ci <- array(NA_real_,
                dim = c(length(parm), 2L), dimnames = list(parm, pct))
    ci[] <- cf[parm] + ses[parm] %o% fac
    ci
}

## loading the MASS namespace will overwrite these in the registry.
## stub is a specialized version of MASS:::confint.xxx with specific message
confint.glm <- function(object, parm, level = 0.95, ...)
{
    if(!requireNamespace("MASS", quietly = TRUE))
        stop("package 'MASS' must be installed")
    confint.glm <- get("confint.glm", asNamespace("MASS"), inherits = FALSE)
    confint.glm(object, parm, level, ...)
}

confint.nls <- function(object, parm, level = 0.95, ...)
{
    if(!requireNamespace("MASS", quietly = TRUE))
        stop("package 'MASS' must be installed")
    confint.nls <- get("confint.nls", asNamespace("MASS"), inherits = FALSE)
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
