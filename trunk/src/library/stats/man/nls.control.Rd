\name{nls.control}
\alias{nls.control}
\title{Control the Iterations in nls}
\description{
  Allow the user to set some characteristics of the \code{nls}
  nonlinear least squares algorithm.
}
\usage{
nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/1024)
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
}
\value{
  A \code{list} with exactly three components:
  \item{maxiter}{}
  \item{tol}{}
  \item{minFactor}{}
  with meanings as explained under \sQuote{Arguments}.
}
\references{
  Bates and Watts (1988),
  \emph{Nonlinear Regression Analysis and Its Applications}, Wiley.
}
\author{Douglas Bates and Saikat DebRoy}
\seealso{\code{\link{nls}}}
\examples{
nls.control(minFactor = 1/2048)
}
\keyword{nonlinear}
\keyword{regression}
\keyword{models}
