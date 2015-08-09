#  File src/library/stats/R/vcov.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1994-2002 W. N. Venables and B. D. Ripley
#  Copyright (C) 2002-11 The R Core Team
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

## The next three have to call the summary method explicitly, as classes which
## inherit from "glm" need not have summary methods which
## inherit from "summary.glm", and similarly for "lm" and "mlm"

## Allow for 'dispersion' to be passed down (see the help for vcov)
vcov.glm <- function(object, ...) summary.glm(object, ...)$cov.scaled

vcov.lm <- function(object, ...)
{
    so <- summary.lm(object)
    so$sigma^2 * so$cov.unscaled
}

vcov.mlm <- function(object, ...)
{
    so <- summary.mlm(object)[[1L]]
    kronecker(estVar(object), so$cov.unscaled, make.dimnames = TRUE)
}

vcov.summary.glm <- function(object, ...) object$cov.scaled
vcov.summary.lm  <- function(object, ...) object$sigma^2 * object$cov.unscaled

## gls and lme methods moved to nlme in 2.6.0


### "The" sigma in lm/nls - "like" models:

sigma <- function(object, ...) UseMethod("sigma")

## works whenever deviance(), nobs() and coef() do fine:
sigma.default <- function (object, use.fallback=TRUE, ...)
    sqrt(deviance(object, ...) /
             (nobs(object, use.fallback=use.fallback) - length(coef(object))))

sigma.mlm <- function (object, ...)
    sqrt(colSums(object$residuals^2) / object$df.residual)

