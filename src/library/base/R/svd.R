svd <- function(x, nu = min(n,p), nv = min(n,p), LINPACK = FALSE)
{
    x <- as.matrix(x)
    if (any(!is.finite(x))) stop("infinite or missing values in x")
    dx <- dim(x)
    n <- dx[1]
    p <- dx[2]
    if(!n || !p) stop("0 extent dimensions")
    if (is.complex(x)) {
        res <- La.svd(x, nu, nv)
        return(list(d = res$d, u = if(nu) res$u, v = if(nv) Conj(t(res$vt))))
    }
    if (!LINPACK) {
        res <- La.svd(x, nu, nv)
        return(list(d = res$d, u = if(nu) res$u, v = if(nv) t(res$vt)))
    }
    if(!is.numeric(x))
	stop("argument to svd must be numeric")

    if(nu == 0) {
	job <- 0
	u <- double(0)
    }
    else if(nu == n) {
	job <- 10
	u <- matrix(0, n, n)
    }
    else if(nu == p) {
	job <- 20
	u <- matrix(0, n, p)
    }
    else
	stop("nu must be 0, nrow(x) or ncol(x)")

    job <- job +
	if(nv == 0) 0 else if(nv == p || nv == n) 1 else
    stop("nv must be 0 or ncol(x)")

    v <- if(job == 0) double(0) else matrix(0, p, p)

    mn <- min(n,p)
    mm <- min(n+1,p)
    z <- .Fortran("dsvdc",
		  as.double(x),
		  n,
		  n,
		  p,
		  d=double(mm),
		  double(p),
		  u=u,
		  n,
		  v=v,
		  p,
		  double(n),
		  as.integer(job),
		  info=integer(1),
		  DUP=FALSE, PACKAGE="base")[c("d","u","v","info")]
    if(z$info)
	stop("error ", z$info, " in dsvdc")
    z$d <- z$d[1:mn]
    if(nv && nv < p) z$v <- z$v[, 1:nv, drop = FALSE]
    z[c("d", if(nu) "u", if(nv) "v")]
}
