#  File src/library/stats/R/AIC.R
#  Part of the R package, http://www.R-project.org
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

#### Return the value of Akaike's Information Criterion

AIC <- function(object, ..., k = 2) UseMethod("AIC")

## For back-compatibility
AIC.logLik <- function(object, ..., k = 2)
    -2 * as.numeric(object) + k * attr(object, "df")

AIC.default <- function(object, ..., k = 2)
{
    ## AIC for various fitted objects --- any for which there's a logLik() method:
    ll <- if("stats4" %in% loadedNamespaces()) stats4:::logLik else logLik
    if(length(list(...))) {# several objects: produce data.frame
	lls <- lapply(list(object, ...), ll)
        vals <- sapply(lls, function(el)
                       c(as.numeric(el), attr(el, "df"), attr(el, "nobs")))
        val <- data.frame(df = vals[2,], ll = vals[1,], nobs = vals[3,])
        if (any(val$nobs != val$nobs[1L]))
            warning("models are not all fitted to the same number of observations")
        val <- data.frame(df = val$df, AIC = -2*val$ll + k*val$df)
        Call <- match.call()
        Call$k <- NULL
	row.names(val) <- as.character(Call[-1L])
	val
    } else {
        lls <- ll(object)
         -2 * as.numeric(lls) + k * attr(lls, "df")
    }
}

BIC <- function(object, ...) UseMethod("BIC")

## For back-compatibility
BIC.logLik <- function(object, ...)
    -2 * as.numeric(object) + attr(object, "df") * log(nobs(object))

BIC.default <- function(object, ...)
{
    ll <- if("stats4" %in% loadedNamespaces()) stats4:::logLik else logLik
    if(length(list(...))) {
        lls <- lapply(list(object, ...), ll)
        vals <- sapply(lls, function(el)
                       c(as.numeric(el), attr(el, "df"), attr(el, "nobs")))
        val <- data.frame(df = vals[2,], ll = vals[1,], nobs = vals[3,])
        if (any(val$nobs != val$nobs[1L]))
            warning("models are not all fitted to the same number of observations")
        val <- data.frame(df = val$df, BIC = -2*val$ll + log(val$nobs)*val$df)
        row.names(val) <- as.character(match.call()[-1])
        val
    } else {
        lls <- ll(object)
         -2 * as.numeric(lls) + log(attr(lls, "nobs")) * attr(lls, "df")
    }
}
