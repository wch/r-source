#  File src/library/base/R/ifelse.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

ifelse <- function (test, yes, no)
{
    if(is.atomic(test)) { # do not lose attributes
        if (typeof(test) != "logical")
            storage.mode(test) <- "logical"
        ## quick return for cases where 'ifelse(a, x, y)' is used
        ## instead of 'if (a) x else y'
        if (length(test) == 1 && is.null(attributes(test))) {
            if (is.na(test)) return(NA)
            else if (test) {
                if (length(yes) == 1 && is.null(attributes(yes)))
                    return(yes)
            }
            else if (length(no) == 1 && is.null(attributes(no)))
                return(no)
        }
    }
    else ## typically a "class"; storage.mode<-() typically fails
	test <- if(isS4(test)) as(test, "logical") else as.logical(test)
    ans <- test
    ok <- !(nas <- is.na(test))
    if (any(test[ok]))
	ans[test & ok] <- rep(yes, length.out = length(ans))[test & ok]
    if (any(!test[ok]))
	ans[!test & ok] <- rep(no, length.out = length(ans))[!test & ok]
    ans[nas] <- NA
    ans
}
