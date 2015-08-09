#  File src/library/stats/R/reorder.factor.R
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

reorder.default <- function(x, X, FUN = mean, ..., order = is.ordered(x))
{
    scores <- tapply(X = X, INDEX = x, FUN = FUN, ...)
    ans <- (if (order) ordered else factor)(x, levels = names(sort(scores, na.last = TRUE)))
    attr(ans, "scores") <- scores
    ans
}

