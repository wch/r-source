#  File src/library/stats/R/stats-deprecated.R
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

## deprecated in 2.2.1
## mauchley.test <- function(object, Sigma=diag(nrow=p),
##                           T = Thin.row(proj(M)-proj(X)),
##                           M = diag(nrow=p),
##                           X = ~0,
##                           idata=data.frame(index=seq(length=p)),...)
## {
## 	.Deprecated("mauchly.test")
## 	UseMethod("mauchly.test")
## }
