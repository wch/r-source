#  File src/library/stats/R/reorder.factor.R
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

reorder.character <- 
reorder.factor <- function(x, X, FUN = mean, ..., order = is.ordered(x))
{
    scores <- tapply(X, x, FUN, ...)
    ans <- (if (order) ordered else factor)(x, levels = names(sort(scores)))
    attr(ans, "scores") <- scores
    ans
}

