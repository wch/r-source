# Notes:
#
# 1. Because of the difference in scoping rules between R and S
#    it is not necessary (and indeed an error) to assign the function
#    in frame 1.  The function f.check can see the function f because
#    it exists in the environment where f is defined.
#
# 2. It is also not necessary (although permissible) to wrap a
#    "list" around the function f.check in the .C call.  R passes
#    such functions through to the underlying C code in "undigested"
#    form.  Corresponding, the underlying C code does not need to
#    extract the function from the passed "list".

dyn.load("zero.so")

zero <- function(f, guesses, tol=1e-7) {
	f.check <- function(x) {
		x <- f(x)
		if(!is.numeric(x)) stop("Need a numeric result")
		as.double(x)
	}
	z <- .C("zero_find",
		f.check,
		ans=as.double(guesses),
		as.double(tol))
	z$ans[1]
}

cube1 <- function(x) (x^2+1)*(x-1.5)
x0 <- zero(cube1, c(0,5))
print(x0)
print(x0,15)
