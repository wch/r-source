## R code to run the .Call/.External examples in
##    `Writing R extensions'

dyn.load(paste("R-exts", .Platform$dynlib.ext, sep=""))

## ----- outer products example -----

out <- function(x, y)
{
   storage.mode(x) <- storage.mode(y) <- "double"
   .Call("out", x, y)
}
out(1:3, 2:4)

x <- 1:3; names(x) <- letters[x]
out(x, 2:4)


## ----- convolution example -----

conv <- function(a, b) .Call("convolve2", a, b)
u <- rep(1, 5)
conv(u, u)

conv <- function(a, b) .Call("convolve2b", a, b)
conv(u, u)

convE <- function(a, b) .External("convolveE", a, b)
convE(u, u)


## ----- Lists examples -----

showArgs <- function(...) .External("showArgs", ...)
showArgs(u=u, x=x, let=letters)

a <- list(a = 1:5, b = rnorm(10), test = runif(100))
.Call("lapply", a, quote(sum(x)), new.env())

.Call("lapply2", a, sum, new.env())


## ----- zero-finding -----

zero <- function(f, guesses, tol = 1e-7) {
    f.check <- function(x) {
        x <- f(x)
        if(!is.numeric(x)) stop("Need a numeric result")
        as.double(x)
    }
    .Call("zero", body(f.check), as.double(guesses), as.double(tol),
          new.env())
}

cube1 <- function(x) (x^2 + 1) * (x - 1.5)
zero(cube1, c(0, 5))

## ----- numerical derivatives -----

numeric.deriv <- function(expr, theta, rho=sys.frame(sys.parent()))
{
    eps <- sqrt(.Machine$double.eps)
    ans <- eval(substitute(expr), rho)
    grad <- matrix(,length(ans), length(theta),
                   dimnames=list(NULL, theta))
    for (i in seq(along=theta)) {
        old <- get(theta[i], envir=rho)
        delta <- eps * min(1, abs(old))
        assign(theta[i], old+delta, envir=rho)
        ans1 <- eval(substitute(expr), rho)
        assign(theta[i], old, envir=rho)
        grad[, i] <- (ans1 - ans)/delta
    }
    attr(ans, "gradient") <- grad
    ans
}
omega <- 1:5; x <- 1; y <- 2

numeric.deriv(sin(omega*x*y), c("x", "y"))

.External("numeric_deriv", quote(sin(omega*x*y)),
          c("x", "y"), .GlobalEnv)
