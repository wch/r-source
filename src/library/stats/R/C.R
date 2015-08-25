#  File src/library/stats/R/C.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1998 B. D. Ripley
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

C <- function(object, contr, how.many, ...)
{
    if(!nlevels(object)) stop("object not interpretable as a factor")
    if(!missing(contr) && is.name(Xcontr <- substitute(contr)))
	contr <- switch(as.character(Xcontr),
			poly =	"contr.poly",
			helmert = "contr.helmert",
			sum = "contr.sum",
			treatment = "contr.treatment",
			SAS = "contr.SAS",
			contr
			)
    if(missing(contr)) {
	oc <- getOption("contrasts")
	contr <-
	    if(length(oc) < 2L) # should not happen
		if(is.ordered(object)) contr.poly else contr.treatment
	    else oc[1 + is.ordered(object)]
    }
    if(missing(how.many) && missing(...))
	contrasts(object) <- contr
    else {
	if(is.character(contr)) contr <- get(contr, mode = "function")
	if(is.function(contr)) contr <- contr(nlevels(object), ...)
	contrasts(object, how.many) <- contr
    }
    object
}
