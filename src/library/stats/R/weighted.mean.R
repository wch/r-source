#  File src/library/stats/R/weighted.mean.R
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

weighted.mean <- function(x, w, na.rm = FALSE) {
    if(missing(w))
      w <- rep.int(1, length(x))
    else if (length(w) != length(x))
      stop("'x' and 'w' must have the same length")
    if(is.integer(w)) w <- as.numeric(w)  # avoid overflow in sum.
    if (na.rm) {
	w <- w[i <- !is.na(x)]
	x <- x[i]
    }
    sum(x*w)/sum(w)
}
