\name{ppoints}
\title{Ordinates for Probability Plotting}
\usage{
ppoints(n, a = ifelse(n <= 10, 3/8, 1/2))
}
\alias{ppoints}
\arguments{
  \item{n}{either the number of points generated or a vector of
    observations.}
  \item{a}{the offset fraction to be used; typically in \eqn{(0,1)}.}
}
\description{
  Generates the sequence of \dQuote{probability} points
  \code{(1:m - a)/(m + (1-a)-a)}
  where \code{m} is either \code{n}, if \code{length(n)==1}, or
  \code{length(n)}.
}
\details{
  If \eqn{0 < a < 1}, the resulting values are within \eqn{(0,1)}
  (excluding boundaries).
  In any case, the resulting sequence is symmetric in \eqn{[0,1]}, i.e.,
  \code{p + rev(p) == 1}.

  \code{ppoints()} is used in \code{qqplot} and \code{qqnorm} to generate
  the set of probabilities at which to evaluate the inverse distribution.

  The choice of \code{a} follows the documentation of the function of the
  same name in Becker \emph{et al} (1988), and appears to have been
  motivated by results from Blom (1958) on approximations to expect normal
  order statistics (see also \code{\link{quantile}}).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Blom, G. (1958)
  \emph{Statistical Estimates and Transformed Beta Variables.}
  Wiley
}
\seealso{
  \code{\link{qqplot}}, \code{\link{qqnorm}}.
}
\examples{
ppoints(4) # the same as  ppoints(1:4)
ppoints(10)
ppoints(10, a=1/2)
}
\keyword{dplot}
\keyword{arith}
\keyword{distribution}
