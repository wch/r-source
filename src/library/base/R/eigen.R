eigen <- function(x, symmetric, only.values = FALSE, EISPACK = FALSE)
{
    x <- as.matrix(x)
    dimnames(x) <- list(NULL, NULL)  # or they appear on eigenvectors
    n <- nrow(x)
    if (!n) stop("0 x 0 matrix")
    if (n != ncol(x)) stop("non-square matrix in 'eigen'")

    complex.x <- is.complex(x)

    if (any(!is.finite(x))) stop("infinite or missing values in 'x'")

    if(complex.x) {
	if(missing(symmetric)) {
            test <- all.equal.numeric(x, Conj(t(x)), 100*.Machine$double.eps)
	    symmetric <- is.logical(test) && test
        }
    }
    else if(is.numeric(x)) {
	storage.mode(x) <- "double"
	if(missing(symmetric)) {
            test <- all.equal.numeric(x, t(x), 100*.Machine$double.eps)
	    symmetric <- is.logical(test) && test
        }
    }
    else stop("numeric or complex values required in 'eigen'")
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
