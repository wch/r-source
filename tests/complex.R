### Tests of complex arithemetic.

## complex
z <- 0i ^ (-3:3)
stopifnot(Re(z) == 0 ^ (-3:3))
set.seed(123)
z <- complex(real = rnorm(100), imag = rnorm(100))
stopifnot(Mod ( 1 -  sin(z) / ( (exp(1i*z)-exp(-1i*z))/(2*1i) ))
	  < 20 * .Machine$double.eps)
## end of moved from complex.Rd


## powers, including complex ones
a <- -4:12
m <- outer(a +0i, b <- seq(-.5,2, by=.5), "^")
dimnames(m) <- list(paste(a), "^" = sapply(b,format))
round(m,3)


## Complex Trig.:
Meps <- .Machine$double.eps
abs(Im(cos(acos(1i))) -	 1) < 2*Meps
abs(Im(sin(asin(1i))) -	 1) < 2*Meps
##P (1 - Im(sin(asin(Ii))))/Meps
##P (1 - Im(cos(acos(Ii))))/Meps
abs(Im(asin(sin(1i))) -	 1) < 2*Meps
cos(1i) == cos(-1i)# i.e. Im(acos(*)) gives + or - 1i:
abs(abs(Im(acos(cos(1i)))) - 1) < 4*Meps


.Random.seed <- c(0, 629, 6137, 22167) # want reproducible output
Isi <- Im(sin(asin(1i + rnorm(100))))
all(abs(Isi-1) < 100* Meps)
##P table(2*abs(Isi-1)	/ Meps)
Isi <- Im(cos(acos(1i + rnorm(100))))
all(abs(Isi-1) < 100* Meps)
##P table(2*abs(Isi-1)	/ Meps)
Isi <- Im(atan(tan(1i + rnorm(100)))) #-- tan(atan(..)) does NOT work (Math!)
all(abs(Isi-1) < 100* Meps)
##P table(2*abs(Isi-1)	/ Meps)


## polyroot():
all(abs(1 + polyroot(choose(8, 0:8))) < 1e-10)# maybe smaller..


## PR#7781
## This is not as given by e.g. glibc on AMD64
(z <- tan(1+1000i)) # 0+1i from R's own code.
stopifnot(is.finite(z))
##


## Branch cuts in complex inverse trig functions
atan(2)
atan(2+0i)
tan(atan(2+0i))
## should not expect exactly 0i in result
round(atan(1.0001+0i), 7)
round(atan(0.9999+0i), 7)
## previously not as in Abramowitz & Stegun.


## typo in z_atan2.
(z <- atan2(0+1i, 0+0i))
stopifnot(all.equal(z, pi/2+0i))
## was NA in 2.1.1


## precision of complex numbers
signif(1.678932e80+0i, 5)
signif(1.678932e-300+0i, 5)
signif(1.678932e-302+0i, 5)
signif(1.678932e-303+0i, 5)
signif(1.678932e-304+0i, 5)
signif(1.678932e-305+0i, 5)
signif(1.678932e-306+0i, 5)
signif(1.678932e-307+0i, 5)
signif(1.678932e-308+0i, 5)
signif(1.678932-1.238276i, 5)
signif(1.678932-1.238276e-1i, 5)
signif(1.678932-1.238276e-2i, 5)
signif(1.678932-1.238276e-3i, 5)
signif(1.678932-1.238276e-4i, 5)
signif(1.678932-1.238276e-5i, 5)
signif(8.678932-9.238276i, 5)
## prior to 2.2.0 rounded real and imaginary parts separately.
