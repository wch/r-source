\name{integrate}
\alias{integrate}
\alias{print.integrate}
\title{Integration of One-Dimensional Functions}
\description{
  Adaptive quadrature of functions of one variable over a finite or
  infinite interval.
}
\usage{
integrate(f, lower, upper, subdivisions=100,
          rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol,
          stop.on.error = TRUE, keep.xy = FALSE, aux = NULL, \dots)
}
\arguments{
  \item{f}{an \R function taking a numeric first argument and returning
    a numeric vector of the same length.  Returning a non-finite element will
    generate an error.}
  \item{lower, upper}{the limits of integration.  Can be infinite.}
  \item{subdivisions}{the maximum number of subintervals.}
  \item{rel.tol}{relative accuracy requested.}
  \item{abs.tol}{absolute accuracy requested.}
  \item{stop.on.error}{logical. If true (the default) an error stops the
    function.  If false some errors will give a result with a warning in
    the \code{message} component.}
  \item{keep.xy}{unused.  For compatibility with S.}
  \item{aux}{unused.  For compatibility with S.}
  \item{\dots}{additional arguments to be passed to \code{f}.  Remember
    to use argument names \emph{not} matching those of \code{integrate(.)}!}
}
\details{
  If one or both limits are infinite, the
  infinite range is mapped onto a finite interval.

  For a finite interval, globally adaptive interval subdivision is used
  in connection with extrapolation by the Epsilon algorithm.

  \code{rel.tol} cannot be less than \code{max(50*.Machine$double.eps,
    0.5e-28)} if \code{abs.tol <= 0}.
}
\note{
  Like all numerical integration routines, these evaluate the function
  on a finite set of points.  If the function is approximately constant
  (in particular, zero) over nearly all its range it is possible that
  the result and error estimate may be seriously wrong.

  When integrating over infinite intervals do so explicitly, rather than
  just using a large number as the endpoint.  This increases the chance
  of a correct answer -- any function whose integral over an infinite
  interval is finite must be near zero for most of that interval.
  
  \code{f} must accept a vector of inputs and produce a vector of function
  evaluations at those points.  The \code{\link{Vectorize}} function
  may be helpful to convert \code{f} to this form.
}
\value{
  A list of class \code{"integrate"} with components
  \item{value}{the final estimate of the integral.}
  \item{abs.error}{estimate of the modulus of the absolute error.}
  \item{subdivisions}{the number of subintervals produced in the
    subdivision process.}
  \item{message}{\code{"OK"} or a character string giving the error message.}
  \item{call}{the matched call.}
}
\references{
  Based on QUADPACK routines \code{dqags} and \code{dqagi} by
  R. Piessens and E. deDoncker-Kapenga, available from Netlib.

  See\cr
  R. Piessens, E. deDoncker-Kapenga, C. Uberhuber, D. Kahaner (1983)
  \emph{Quadpack: a Subroutine Package for Automatic Integration};
  Springer Verlag.
}
\seealso{
  The function \code{\link[adapt]{adapt}} in the \pkg{adapt} package on
  CRAN, for multivariate integration.
}
\examples{
integrate(dnorm, -1.96, 1.96)
integrate(dnorm, -Inf, Inf)

## a slowly-convergent integral
integrand <- function(x) {1/((x+1)*sqrt(x))}
integrate(integrand, lower = 0, upper = Inf)

## don't do this if you really want the integral from 0 to Inf
integrate(integrand, lower = 0, upper = 10)
integrate(integrand, lower = 0, upper = 100000)
integrate(integrand, lower = 0, upper = 1000000, stop.on.error = FALSE)

## some functions do not handle vector input properly
f <- function(x) 2  
try(integrate(f, 0, 1)) 
integrate(Vectorize(f), 0, 1)  ## correct
integrate(function(x) rep(2, length(x)), 0, 1)  ## correct

## integrate can fail if misused
integrate(dnorm,0,2)
integrate(dnorm,0,20)
integrate(dnorm,0,200)
integrate(dnorm,0,2000)
integrate(dnorm,0,20000) ## fails on many systems
integrate(dnorm,0,Inf)   ## works
}
\keyword{math}
\keyword{utilities}
