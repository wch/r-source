#  File src/library/stats4/R/BIC.R
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

## created for use in other packages, e.g. flexmix
setGeneric("AIC")

setGeneric("BIC", function(object, ...) standardGeneric("BIC"))

setMethod("BIC", signature(object="logLik"),
          function(object, ...)
          -2 * c(object) + attr(object, "df") * log(attr(object, "nobs")) )

setMethod("BIC", signature(object="ANY"),
	  ## work like AIC with *multiple* objects
	  function(object, ...) {
	      if(length(list(...))) {# several objects: produce data.frame
		  val <- lapply(list(object, ...), logLik)
		  val <- as.data.frame(t(sapply(val,
						function(el)
						c(attr(el, "df"), BIC(el)),
						USE.NAMES=FALSE)))
		  names(val) <- c("df", "BIC")
		  row.names(val) <- as.character(match.call()[-1L])
		  val
	      } else BIC(logLik(object))
	  })
