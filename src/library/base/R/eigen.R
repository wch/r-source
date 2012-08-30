#  File src/library/base/R/eigen.R
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


isSymmetric <- function(object, ...) UseMethod("isSymmetric")

isSymmetric.matrix <- function(object, tol = 100*.Machine$double.eps, ...)
{
    if(!is.matrix(object)) return(FALSE) ## we test for  symmetric *matrix*
    ## cheap pretest: is it square?
    d <- dim(object)
    if(d[1L] != d[2L]) return(FALSE)
    test <-
        if(is.complex(object))
            all.equal.numeric(object, Conj(t(object)), tolerance = tol, ...)
        else # numeric, character, ..
            all.equal(object, t(object), tolerance = tol, ...)
    isTRUE(test)
}

eigen <- function(x, symmetric, only.values = FALSE, EISPACK = FALSE)
{
    x <- as.matrix(x)
    if(!is.null(dimnames(x)))
        dimnames(x) <- list(NULL, NULL)  # or they appear on eigenvectors
    n <- nrow(x)
    if (!n) stop("0 x 0 matrix")
    if (n != ncol(x)) stop("non-square matrix in 'eigen'")
    n <- as.integer(n)
    if(is.na(n)) stop("invalid nrow(x)")

    complex.x <- is.complex(x)
    if(!complex.x && !is.double(x))
	storage.mode(x) <- "double"
    if (!all(is.finite(x))) stop("infinite or missing values in 'x'")

    if(missing(symmetric)) symmetric <- isSymmetric.matrix(x)

    if (!EISPACK) {
        if (symmetric) {
            z <- if(!complex.x)
                .Call("La_rs", x, only.values, PACKAGE = "base")
            else
                .Call("La_rs_cmplx", x, only.values, PACKAGE = "base")
            ord <- rev(seq_along(z$values))
        } else {
            z <- if(!complex.x)
                .Call("La_rg", x, only.values, PACKAGE = "base")
            else
                .Call("La_rg_cmplx", x, only.values, PACKAGE = "base")
            ord <- sort.list(Mod(z$values), decreasing = TRUE)
        }
        return(list(values = z$values[ord],
                    vectors = if (!only.values) z$vectors[, ord, drop = FALSE]))
    }

    warning("EISPACK = TRUE is deprecated", domain = NA)
    if(!complex.x && !is.double(x)) storage.mode(x) <- "double"
    dbl.n <- double(n)
    if(symmetric) {##--> real values
	if(complex.x) {
	    z <- .Fortran("ch",
			  n,
			  n,
			  Re(x),
			  Im(x),
			  values = dbl.n,
			  !only.values,
			  vectors = Re(x),
			  ivectors = Im(x),
			  dbl.n,
			  dbl.n,
			  double(2*n),
			  ierr = integer(1L),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'ch' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	    if(!only.values)
		z$vectors <- matrix(complex(real=z$vectors,
					    imaginary=z$ivectors), ncol=n)
	}
	else {
	    z <- .Fortran("rs",
			  n,
			  n,
			  x,
			  values = dbl.n,
			  !only.values,
			  vectors = x,
			  dbl.n,
			  dbl.n,
			  ierr = integer(1L),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'rs' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	}
	ord <- sort.list(z$values, decreasing = TRUE)
    }
    else {##- Asymmetric :
	if(complex.x) {
	    xr <- Re(x)
	    xi <- Im(x)
	    z <- .Fortran("cg",
			  n,
			  n,
			  xr,
			  xi,
			  values = dbl.n,
			  ivalues = dbl.n,
			  !only.values,
			  vectors = xr,
			  ivectors = xi,
			  dbl.n,
			  dbl.n,
			  dbl.n,
			  ierr = integer(1L),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'cg' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	    z$values <- complex(real=z$values,imaginary=z$ivalues)
	    if(!only.values)
		z$vectors <- matrix(complex(real=z$vectors,
					    imaginary=z$ivectors), ncol=n)
	}
	else {
	    z <- .Fortran("rg",
			  n,
			  n,
			  x,
			  values = dbl.n,
			  ivalues = dbl.n,
			  !only.values,
			  vectors = x,
			  integer(n),
			  dbl.n,
			  ierr = integer(1L),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'rg' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	    ind <- z$ivalues > 0L
	    if(any(ind)) {#- have complex (conjugated) values
		ind <- seq.int(n)[ind]
		z$values <- complex(real=z$values,imaginary=z$ivalues)
		if(!only.values) {
		    z$vectors[, ind] <- complex(real=z$vectors[,ind],
						imaginary=z$vectors[,ind+1])
		    z$vectors[, ind+1] <- Conj(z$vectors[,ind])
		}
	    }
	}
	ord <- sort.list(Mod(z$values), decreasing = TRUE)
    }
    list(values = z$values[ord],
	 vectors = if(!only.values) z$vectors[, ord, drop = FALSE])
}
