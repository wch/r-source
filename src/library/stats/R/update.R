#  File src/library/stats/R/update.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

getCall <- function(x, ...) UseMethod("getCall")
getCall.default <- function(x, ...) getElement(x, "call")
## Using getCall() instead of  x$call  renders update.default() more
## generally applicable.

update.default <-
    function (object, formula., ..., evaluate = TRUE)
{
    if (is.null(call <- getCall(object)))
	stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(formula.))
	call$formula <- update.formula(formula(object), formula.)
    if(length(extras)) {
	existing <- !is.na(match(names(extras), names(call)))
	## do these individually to allow NULL to remove entries.
	for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
	if(any(!existing)) {
	    call <- c(as.list(call), extras[!existing])
	    call <- as.call(call)
	}
    }
    if(evaluate) eval(call, parent.frame())
    else call
}

update.formula <- function (old, new, ...)
{
    tmp <- .Call(C_updateform, as.formula(old), as.formula(new))
    out <- formula(terms.formula(tmp, simplify = TRUE))
    return(out)
}
