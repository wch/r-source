fft <- function(z, inverse=FALSE)
    .Internal(fft(z, inverse))

mvfft <- function(z, inverse=FALSE)
    .Internal(mvfft(z, inverse))

nextn <- function(n, factors=c(2,3,5))
    .Internal(nextn(n, factors))

convolve <- function(x, y, conj=TRUE, circular=TRUE) {
    n <- length(x)
    ny <- length(y)
    Real <- is.numeric(x) && is.real(y)
    if(circular) {
        if(ny != n)
            stop("length mismatch in convolution")
    }
    else { ## "open": Pad with zeros
        x <- c(rep(0, ny - 1), x)
        n <- length(y <- c(y, rep(0, n - 1)))# n = nx+ny-1
    }
    x <- fft(fft(x)* (if(conj)Conj(fft(y)) else fft(y)), inv=TRUE)
    if(Real) Re(x)/n else x/n
}

