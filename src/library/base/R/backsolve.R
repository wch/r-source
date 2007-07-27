#  File src/library/base/R/backsolve.R
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

forwardsolve <- function(l, x, k=ncol(l), upper.tri = FALSE, transpose = FALSE)
    backsolve(l,x, k=k, upper.tri= upper.tri, transpose= transpose)

backsolve <- function(r, x, k=ncol(r), upper.tri = TRUE, transpose = FALSE)
{
    r <- as.matrix(r)# nr  x  k
    storage.mode(r) <- "double"
    x.mat <- is.matrix(x)
    if(!x.mat) x <- as.matrix(x)# k  x	nb
    storage.mode(x) <- "double"
    k <- as.integer(k)
    if(k <= 0 || nrow(x) < k) stop("invalid argument values in 'backsolve'")
    nb <- ncol(x)
    upper.tri <- as.logical(upper.tri)
    transpose <- as.logical(transpose)
    job <- as.integer((upper.tri) + 10*(transpose))
    z <- .C("bakslv",
	    t  = r, ldt= nrow(r), n  = k,
	    b  = x, ldb= k,	  nb = nb,
	    x  = matrix(0, k, nb),
	    job = job,
	    info = integer(1),
	    DUP = FALSE, PACKAGE = "base")[c("x","info")]
    if(z$info != 0)
	stop(gettextf("singular matrix in 'backsolve'. First zero in diagonal [%d]", z$info), domain = NA)
    if(x.mat) z$x else drop(z$x)
}
