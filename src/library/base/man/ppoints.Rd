\name{ppoints}
\title{Ordinates for Probability Plotting}
\usage{
ppoints(n)
}
\alias{ppoints}
\arguments{
\item{n}{either the number of points generate or a vector of observations.}
}
\description{
This generates the sequence of points \code{(1:m - 0.5)/m} where \code{m} is
either \code{n}, if \code{length(n)==1}, or \code{length(n)}.
It is used in \code{qqplot}, \code{qqnorm} to generate the set of
probabilities at which to evaluate the inverse distribution.
}
\seealso{
\code{\link{qqplot}}, \code{\link{qqnorm}}.
}
\examples{
ppoints(4)
ppoints(10)
}
\keyword{dplot}
\keyword{arith}
\keyword{distribution}
