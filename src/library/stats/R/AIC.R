#  File src/library/stats/R/AIC.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 2001-3 The R Core Team
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
### originally from package nlne.

AIC <- function(object, ..., k = 2) UseMethod("AIC")

## For back-compatibility
AIC.logLik <- function(object, ..., k = 2)
    -2 * as.numeric(object) + k * attr(object, "df")

AIC.default <- function(object, ..., k = 2)
{
    ## AIC for various fitted objects --- any for which there's a logLik() method:
    ll <- if("stats4" %in% loadedNamespaces()) stats4:::logLik else logLik
    if(!missing(...)) {# several objects: produce data.frame
	lls <- lapply(list(object, ...), ll)
        vals <- sapply(lls, function(el) {
            no <- attr(el, "nobs")
            c(as.numeric(el), attr(el, "df"),
              if(is.null(no)) NA_integer_ else no)
        })
        val <- data.frame(df = vals[2L,], ll = vals[1L,])
        nos <- na.omit(vals[3L,])
        if (length(nos) && any(nos != nos[1L]))
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
    Nobs <- if("stats4" %in% loadedNamespaces()) stats4:::nobs else nobs
    if(!missing(...)) {
        lls <- lapply(list(object, ...), ll)
        vals <- sapply(lls, function(el) {
            no <- attr(el, "nobs")
            c(as.numeric(el), attr(el, "df"),
              if(is.null(no)) NA_integer_ else no)
        })
        val <- data.frame(df = vals[2L,], ll = vals[1L,], nobs = vals[3L,])
        nos <- na.omit(val$nobs)
        if (length(nos) && any(nos != nos[1L]))
            warning("models are not all fitted to the same number of observations")
        ## if any val$nobs = NA, try to get value via nobs().
        unknown <- is.na(val$nobs)
        if(any(unknown))
            val$nobs[unknown] <-
                sapply(list(object, ...)[unknown],
                       function(x, f) tryCatch(f(x), error = function(e) NA_real_),
                       f = Nobs)
        val <- data.frame(df = val$df, BIC = -2*val$ll + log(val$nobs)*val$df)
        row.names(val) <- as.character(match.call()[-1L])
        val
    } else {
        lls <- ll(object)
        nos <- attr(lls, "nobs")
        if (is.null(nos))
            nos <- tryCatch(Nobs(object), error = function(e) NA_real_)
        -2 * as.numeric(lls) + log(nos) * attr(lls, "df")
    }
}
