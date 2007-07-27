#  File src/library/base/R/vector.R
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

vector <- function(mode = "logical", length = 0).Internal(vector(mode,length))
logical <- function(length = 0) vector("logical", length)
character <- function(length = 0) vector("character", length)
integer <- function(length = 0) vector("integer", length)
double <- function(length = 0) vector("double", length)
real <- double
numeric <- double
complex <- function(length.out = 0,
		    real = numeric(), imaginary = numeric(),
		    modulus = 1, argument = 0) {
    if(missing(modulus) && missing(argument)) {
	## assume 'real' and 'imaginary'
	.Internal(complex(length.out, real, imaginary))
    } else {
	n <- max(length.out, length(argument), length(modulus))
	rep(modulus,length.out=n) *
	    exp(1i * rep(argument, length.out=n))
    }
}

single <- function(length = 0)
    structure(vector("double", length), Csingle=TRUE)
