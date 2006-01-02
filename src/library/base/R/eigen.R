
isSymmetric <- function(object, ...) UseMethod("isSymmetric")

isSymmetric.matrix <- function(object, tol = 100*.Machine$double.eps, ...) {
    if(!is.matrix(object)) return(FALSE) ## we test for  symmetric *matrix*

    test <-
        if(is.complex(object))
            all.equal.numeric(object, Conj(t(object)), tol = tol, ...)
        else # numeric, character, ..
            all.equal(object, t(object), tol = tol, ...)
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

    complex.x <- is.complex(x)
    if(!complex.x && !is.numeric(x))
        stop("numeric or complex values required in 'eigen'")
    if (any(!is.finite(x))) stop("infinite or missing values in 'x'")

    if(missing(symmetric))
        symmetric <- isSymmetric.matrix(x)

    if (!EISPACK) {
        if (symmetric) {
            z <- if(!complex.x)
                .Call("La_rs", x, only.values, "dsyevr", PACKAGE = "base")
            else
                .Call("La_rs_cmplx", x, only.values, PACKAGE = "base")
            ord <- rev(seq(along = z$values))
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

    dbl.n <- double(n)
    if(symmetric) {##--> real values
	if(complex.x) {
	    xr <- Re(x)
	    xi <- Im(x)
	    z <- .Fortran("ch",
			  n,
			  n,
			  xr,
			  xi,
			  values = dbl.n,
			  !only.values,
			  vectors = xr,
			  ivectors = xi,
			  dbl.n,
			  dbl.n,
			  double(2*n),
			  ierr = integer(1),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'ch' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	    if(!only.values)
		z$vectors <- matrix(complex(re=z$vectors,
					    im=z$ivectors), nc=n)
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
			  ierr = integer(1),
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
			  ierr = integer(1),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'cg' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	    z$values <- complex(re=z$values,im=z$ivalues)
	    if(!only.values)
		z$vectors <- matrix(complex(re=z$vectors,
					    im=z$ivectors), nc=n)
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
			  ierr = integer(1),
                          PACKAGE="base")
	    if (z$ierr)
		stop(gettextf("'rg' returned code %d in 'eigen'", z$ierr),
                     domain = NA)
	    ind <- z$ivalues > 0
	    if(any(ind)) {#- have complex (conjugated) values
		ind <- seq(n)[ind]
		z$values <- complex(re=z$values,im=z$ivalues)
		if(!only.values) {
		    z$vectors[, ind] <- complex(re=z$vectors[,ind],
						im=z$vectors[,ind+1])
		    z$vectors[, ind+1] <- Conj(z$vectors[,ind])
		}
	    }
	}
	ord <- sort.list(Mod(z$values), decreasing = TRUE)
    }
    list(values = z$values[ord],
	 vectors = if(!only.values) z$vectors[,ord, drop = FALSE])
}
