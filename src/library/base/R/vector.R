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
