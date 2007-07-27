#  File src/library/base/R/trunc.R
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

## Commented by KH on 1999/01/30.
## trunc() should really be in the `Math' group.

##trunc <- function(x, ...) UseMethod("trunc")
##trunc.default <- function(x) {
##    a <- attributes(x)
##    x <- ifelse(x < 0, ceiling(x), floor(x))
##    attributes(x) <- a
##    x
##}
