fft <- function(z, inverse=FALSE)
    .Internal(fft(z, inverse))

mvfft <- function(z, inverse=FALSE)
    .Internal(mvfft(z, inverse))

nextn <- function(n, factors=c(2,3,5))
    .Internal(nextn(n, factors))

convolve <- function(x, y, conj=TRUE, type=c("circular","open","filter")) {
    type <- match.arg(type)
    n <- length(x)
    ny <- length(y)
    Real <- is.numeric(x) && is.numeric(y)
    ## switch(type, circular = ..., )
    if(type == "circular") {
        if(ny != n)
            stop("length mismatch in convolution")
    }
    else { ## "open" or "filter": Pad with zeros
        n1 <- ny - 1
        x <- c(rep.int(0, n1), x)
        n <- length(y <- c(y, rep.int(0, n - 1)))# n = nx+ny-1
    }
    x <- fft(fft(x)* (if(conj)Conj(fft(y)) else fft(y)), inverse=TRUE)
    if(type == "filter")
        (if(Real) Re(x) else x)[-c(1:n1, (n-n1+1):n)]/n
    else
        (if(Real) Re(x) else x)/n
}

