
qbirthday <- function(prob = 0.5, classes = 365, coincident = 2)
{
    k <- coincident
    c <- classes
    p <- prob
    if (p <= 0) return(1)
    if (p >= 1) return(c*(k-1)+1)
    if ((k-1)*log(c) > 8 ||  1 - p < 1e-7) {
        lnN <- ((k-1)*log(c) + lgamma(k+1) + log(-log1p(-p)))/k
        N <- exp(lnN)
    } else {
        N <- (c^(k-1) * gamma(k+1) * log(1/(1-p)))^(1/k)
    }
    round(N)
}

pbirthday <- function(n, classes = 365, coincident = 2)
{
    k <- coincident
    c <- classes
    if (k < 2) return(1)
    if (k > n) return(0)
    if (n > c*(k-1)) return(1)
    eps <- 1e-14
    if (qbirthday(1-eps, c, k) <= n)
	return(1-eps)
    f1 <- function(p) qbirthday(p,c,k) - n
    upper <- min(1, exp(k * log(n) - (k-1) * log(c)), na.rm = TRUE)
    nmin <- uniroot(f1, lower = 0, upper = upper, tol = eps)
    if(nmin$root == 0 && f1(.Machine$double.xmin) < 0) {
	## try again -- on log scale --
	f2 <- function(ln.p) qbirthday(exp(ln.p), c, k) - n
	nmin <- uniroot(f2, lower= floor(log(.Machine$double.xmin)),
			upper = -2, tol = eps)
	exp(nmin$root)
    }
    else
	nmin$root
}

