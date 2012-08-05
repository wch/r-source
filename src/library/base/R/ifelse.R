#  File src/library/base/R/ifelse.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

ifelse <-
    function (test, yes, no)
{
    storage.mode(test) <- "logical"
    ans <- test
    nas <- is.na(test)
    if (any(test[!nas]))
        ans[test & !nas] <- rep(yes, length.out = length(ans))[test & !nas]
    if (any(!test[!nas]))
        ans[!test & !nas] <- rep(no, length.out = length(ans))[!test & !nas]
    ans[nas] <- NA
    ans
}
