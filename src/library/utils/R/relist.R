#  File src/library/utils/R/relist.R
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

# relist.R -- an inverse operator to unlist
# written by Andrew Clausen <clausen@econ.upenn.edu> in 2007
# with helpful suggestions from
#	Martin Maechler  ETHZ CH
#	Gabor Grothendieck at Gmail dotcom
#	Seth Falcon, sfalcon (near) fhcrc (comma) org
#
# Some functions need many parameters, which are most easily represented in
# complex structures.  Unfortunately, many mathematical functions in R,
# including optim, nlm, and grad can only operate on functions whose domain is
# a vector.  R has a function called "unlist" to convert complex objects into a
# vector representation.  This file provides an inverse operation called
# "relist" to convert vectors back to the convenient structural representation.
# Together, these functions allow structured functions to have simple
# mathematical interfaces.
#
# For example, a likelihood function for a multivariate normal model needs a
# variance-covariance matrix and a mean vector.	 It would be most convenient to
# represent it as a list containing a vector and a matrix.  A typical parameter
# might look like
#
#	list(mean=c(0, 1), vcov=cbind(c(1, 1), c(1, 0)))
#
# However, optim can't operate on functions that take lists as input; it
# only likes vectors.  The solution is conversion:
#
## 	initial.param <- list(mean=c(0, 1), vcov=cbind(c(1, 1), c(1, 0)))
## 	initial.param <- as.relistable(initial.param)
## #
## 	ll <- function(param.vector)
## 	{
## 		param <- relist(initial.param)
## 		-sum(dnorm(x, mean=param$mean, vcov=param$vcov, log=TRUE))
## 		# note: dnorm doesn't do vcov... but I hope you get the point
## 	}

## 	optim(unlist(initial.param), ll)
#
# "relist" takes two parameters: skeleton and flesh.  Skeleton is a sample
# object that has the right "shape" but the wrong content.  "flesh" is a vector
# with the right content but the wrong shape.  Invoking
#
#	relist(flesh, skeleton)
#
# will put the content of flesh on the skeleton.  You don't need to specify
# skeleton explicitly if the skeleton is stored as an attribute inside flesh.
# In particular, flesh was created from some object obj with
#
#	unlist(as.relistable(obj))
#
# then the skeleton attribute is automatically set.
#
# As long as "skeleton" has the right shape, it should be a precise inverse
# of unlist.  These equalities hold:
#
#	relist(unlist(x), skeleton) == x
#	unlist(relist(y, skeleton)) == y
#
#	x <- as.relistable(x)
#	relist(unlist(x)) == x

is.relistable <- function(x) inherits(x, "relistable")

as.relistable <- function(x)
{
    if (!inherits(x, "relistable"))
	class(x) <- c("relistable", class(x))
    x
}

## NB: unlist() is generic *internally* (i.e. not visible from 'unlist')
unlist.relistable <- function(x, recursive=TRUE, use.names=TRUE)
{
    if (!recursive)
	warning("relist() requires recursively unlisted objects.")
    skeleton <- x
### MM: FIXME?  I think this is just  NextMethod()
    ## remove 'relistable'
    class(x) <- setdiff(class(x), "relistable")
    result <- unlist(x, recursive, use.names)
    attr(result, "skeleton") <- skeleton
    result
}

relist <- function(flesh, skeleton=attr(flesh, "skeleton"))
{
    if (is.null(skeleton)) {
	stop("The 'flesh' argument does not contain a skeleton attribute.\n",
	     "Either ensure you unlist a relistable object, or specify the skeleton separately.")
    }
    UseMethod("relist", skeleton)
}

## was 'relist.numeric' in Andrew's code
relist.default <- function(flesh, skeleton=attr(flesh, "skeleton"))
{
    result <- flesh
    names(result) <- names(skeleton)
    result
}

relist.list <- function(flesh, skeleton=attr(flesh, "skeleton"))
{
    ind <- 1L
    result <- skeleton
    for (i in seq_along(skeleton)) {
	size <- length(unlist(result[[i]]))
	result[[i]] <-
	    relist(flesh[ind:(ind + size - 1L)], result[[i]])
	ind <- ind + size
    }
    result
}


relist.matrix <- function(flesh, skeleton=attr(flesh, "skeleton"))
{
    if (is.numeric(skeleton[1,1]))
	return(matrix(flesh, nrow=nrow(skeleton),
		      dimnames=dimnames(skeleton)))
    n <- nrow(skeleton)
    m <- ncol(skeleton)
    result <- skeleton
    ind <- 1L
    for (j in 1L:m)
	for (i in 1L:n) {
	    size <- length(unlist(skeleton[[i, j]]))
	    result[[i, j]] <- relist(flesh[ind:(ind + size - 1)],
				     skeleton[[i, j]])
	    ind <- ind + size
	}
    result
}

relist.factor <- function(flesh, skeleton=attr(flesh, "skeleton"))
{
    as.factor(levels(skeleton)[flesh])
}
