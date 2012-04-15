% File src/library/stats/man/nls.control.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{nls.control}
\alias{nls.control}
\title{Control the Iterations in nls}
\description{
  Allow the user to set some characteristics of the \code{nls}
  nonlinear least squares algorithm.
}
\usage{
nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/1024,
            printEval = FALSE, warnOnly = FALSE)
}
\arguments{
  \item{maxiter}{A positive integer specifying the maximum number of
    iterations allowed.}
  \item{tol}{A positive numeric value specifying the tolerance level for
    the relative offset convergence criterion.}
  \item{minFactor}{A positive numeric value specifying the minimum
    step-size factor allowed on any step in the iteration.  The
    increment is calculated with a Gauss-Newton algorithm and
    successively halved until the residual sum of squares has been
    decreased or until the step-size factor has been reduced below this
    limit.}
  \item{printEval}{a logical specifying whether the number of evaluations
    (steps in the gradient direction taken each iteration) is printed.}
  \item{warnOnly}{a logical specifying whether \code{\link{nls}()} should
    return instead of signalling an error in the case of termination
    before convergence.
    Termination before convergence happens upon completion of \code{maxiter}
    iterations, in the case of a singular gradient, and in the case that the
    step-size factor is reduced below \code{minFactor}.}
}
\value{
  A \code{list} with exactly five components:
  \item{maxiter}{}
  \item{tol}{}
  \item{minFactor}{}
  \item{printEval}{}
  \item{warnOnly}{}
  with meanings as explained under \sQuote{Arguments}.
}
\references{
  Bates, D. M. and Watts, D. G. (1988),
  \emph{Nonlinear Regression Analysis and Its Applications}, Wiley.
}
\author{Douglas Bates and Saikat DebRoy}
\seealso{
  \code{\link{nls}}
}
\examples{
nls.control(minFactor = 1/2048)
}
\keyword{nonlinear}
\keyword{regression}
\keyword{models}
