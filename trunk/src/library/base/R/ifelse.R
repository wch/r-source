#  File src/library/base/R/ifelse.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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
                if (length(yes) == 1) {
                    yat <- attributes(yes)
                    if (is.null(yat) || (is.function(yes) &&
                                         identical(names(yat), "srcref")))
                        return(yes)
                }
            }
            else if (length(no) == 1) {
                nat <- attributes(no)
                if (is.null(nat) || (is.function(no) &&
                                     identical(names(nat), "srcref")))
                    return(no)
            }
        }
    }
    else ## typically a "class"; storage.mode<-() typically fails
	test <- if(isS4(test)) methods::as(test, "logical") else as.logical(test)
    ans <- test
    len <- length(ans)
    ## which already removes NAs so we don't need to ever call is.na
    ypos <- which(test)
    npos <- which(!test)
    if(length(ypos) > 0L)
        ans[ypos] <- rep(yes, length.out = len)[ypos]
    if(length(npos) > 0L)
        ans[npos] <- rep(no, length.out = len)[npos]
    ans
}
