#  File src/library/base/R/unname.R
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

unname <- function (obj, force = FALSE)
{
    ## *** temporary direct replacement function calls until the
    ## *** compiler can handle this:
    if (!is.null(names(obj)))
        ## names(obj) <- NULL
        obj <- `names<-`(obj, value = NULL)
    if (!is.null(dimnames(obj)) && (force || !is.data.frame(obj)))
        ## dimnames(obj) <- NULL
        obj <- `dimnames<-`(obj, value = NULL)
    obj
}
