fft <- function(z, inverse=FALSE)
.Internal(fft(z, inverse))

mvfft <- function(z, inverse=FALSE)
.Internal(mvfft(z, inverse))

nextn <- function(n, factors=c(2,3,5))
.Internal(nextn(n, factors))

convolve <-
function(x, y, conj=T) {
	if(length(x) != length(y))
		stop("length mismatch in convolution")
	if(conj)
		Re(fft(fft(x)*Conj(fft(y)),inv=T))/length(x)
	else
		Re(fft(fft(x)*fft(y),inv=T))/length(x)
}
