fft <- function(z, inverse=FALSE)
.Internal(fft(z, inverse))

mvfft <- function(z, inverse=FALSE)
.Internal(mvfft(z, inverse))

nextn <- function(n, factors=c(2,3,5))
.Internal(nextn(n, factors))

convolve <- function(x, y, conj=TRUE) {
	n <- length(x)
	if(length(y) != n)
		stop("length mismatch in convolution")
        Re(fft(fft(x)* (if(conj)Conj(fft(y)) else fft(y)), inv=TRUE))/n
}
