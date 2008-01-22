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

#### Return the object's value of the Akaike Information Criterion
#### (or "An Inf.. Crit..")

AIC <- function(object, ..., k = 2) UseMethod("AIC")

## AIC for logLik objects
AIC.logLik <- function(object, ..., k = 2)
    -2 * c(object) + k * attr(object, "df")

AIC.default <- function(object, ..., k = 2)
{
    ## AIC for various fitted objects --- any for which there's a logLik() method:
    ll <- if("stats4" %in% loadedNamespaces()) stats4:::logLik else logLik
    if(length(list(...))) {# several objects: produce data.frame
	object <- list(object, ...)
	val <- lapply(object, ll)
	val <- as.data.frame(t(sapply(val,
				      function(el)
				      c(attr(el, "df"), AIC(el, k = k)))))
	names(val) <- c("df", "AIC")
        Call <- match.call()
        Call$k <- NULL
	row.names(val) <- as.character(Call[-1])
	val
    } else AIC(ll(object), k = k)
}
