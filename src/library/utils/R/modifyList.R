#  File src/library/utils/R/modifyList.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

### Originates from Deepayan Sarkar as  updateList() from 'lattice' package

modifyList <- function(x, val, keep.null = FALSE)
{
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    vnames <- names(val)
    ## Will not update unnamed components.  FIXME: What if names are repeated? Warn?
    vnames <- vnames[nzchar(vnames)]
    if (keep.null) {
        for (v in vnames) {
            x[v] <-
                if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
                    list(modifyList(x[[v]], val[[v]], keep.null = keep.null))
                else val[v]
        }
    }
    else {
        for (v in vnames) {
            x[[v]] <-
                if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
                    modifyList(x[[v]], val[[v]], keep.null = keep.null)
                else val[[v]]
        }
    }
    x
}
