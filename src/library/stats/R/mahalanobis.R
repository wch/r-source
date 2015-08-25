#  File src/library/stats/R/mahalanobis.R
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

if(FALSE)
mahalanobis. <- function(x, center, cov, inverted=FALSE, ...)
{
    x <- if(is.vector(x)) matrix(x, ncol=length(x)) else as.matrix(x)
    ## save speed in customary case:
    ## if(any(center != 0))
    x <- t(sweep(x, 2, center))# = (x - center)
    setNames(colSums(x * if(inverted) cov%*%x else solve(cov, x, ...)),
	     rownames(x))
}


mahalanobis <- function(x, center, cov, inverted=FALSE, ...)
{
    x <- if(is.vector(x)) matrix(x, ncol=length(x)) else as.matrix(x)
    ## save speed in customary case
    if(!identical(center, FALSE))
	x <- sweep(x, 2L, center)# = "x - center"
    ## NB:  sweep(...., check.margin=FALSE) does not measurably save time

    ## The following would be considerably faster for  small nrow(x) and
    ## slower otherwise; probably always faster if the t(.) wasn't needed:
    ##
    ## x <- t(sweep(x, 2, center))# = (x - center)
    ## retval <- colSums(x * if(inverted) cov %*% x else solve(cov,x, ...))
    if(!inverted)
	cov <- solve(cov, ...)
    setNames(rowSums(x %*% cov * x), rownames(x))
}
