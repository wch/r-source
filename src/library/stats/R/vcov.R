#  File src/library/stats/R/vcov.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2002-2017 The R Core Team
#  Copyright (C) 1994-2002 W. N. Venables and B. D. Ripley
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

vcov <- function(object, ...) UseMethod("vcov")

##' Augment a vcov - matrix by NA rows & cols when needed:
.vcov.aliased <- function(aliased, vc, complete = TRUE) {
    ## Checking for "NA coef": "same" code as in print.summary.lm() in ./lm.R :
    if(complete && NROW(vc) < (P <- length(aliased)) && any(aliased)) {
	## add NA rows and columns in vcov
	cn <- names(aliased)
	VC <- matrix(NA_real_, P, P, dimnames = list(cn,cn))
	j <- which(!aliased)
	VC[j,j] <- vc
	VC
    } else  # default
	vc
}

## The next three have to call the summary method explicitly, as classes which
## inherit from "glm" need not have summary methods which
## inherit from "summary.glm", and similarly for "lm" and "mlm"

## Allow for 'dispersion' to be passed down (see the help for vcov)
vcov.glm <- function(object, complete = TRUE, ...) vcov.summary.glm(summary.glm(object), complete=complete, ...)

vcov.lm <- function(object, complete = TRUE, ...) vcov.summary.lm(summary.lm(object), complete=complete, ...)

## To be consistent with coef.aov() which has complete = FALSE :
vcov.aov <- vcov.lm ; formals(vcov.aov)$complete <- FALSE


vcov.mlm <- function(object, complete = TRUE, ...)
{
    so <- summary.mlm(object)[[1L]]
    kronecker(estVar(object),
	      .vcov.aliased(so$aliased, so$cov.unscaled, complete=complete),
	      make.dimnames = TRUE)
}

vcov.summary.glm <- function(object, complete = TRUE, ...)
    .vcov.aliased(object$aliased, object$cov.scaled, complete=complete)

vcov.summary.lm  <- function(object, complete = TRUE, ...)
    .vcov.aliased(object$aliased, object$sigma^2 * object$cov.unscaled, complete=complete)

## gls and lme methods moved to nlme in 2.6.0


### "The" sigma in lm/nls - "like" models:

sigma <- function(object, ...) UseMethod("sigma")

## works whenever deviance(), nobs() and coef() do fine:
sigma.default <- function (object, use.fallback=TRUE, ...)
    sqrt(deviance(object, ...) /
	 (nobs(object, use.fallback=use.fallback) - sum(!is.na(coef(object)))))

sigma.mlm <- function (object, ...)
    sqrt(colSums(object$residuals^2) / object$df.residual)

