\name{IQR}
\title{The Interquartile Range}
\usage{
IQR(x, na.rm = FALSE)
}
\alias{IQR}
\description{computes interquartile range of the \code{x} values.}
\details{
Note that this function computes the quartiles using the
\code{\link{quantile}} function rather than following Tukey's recommendations,
i.e., \code{IQR(x) = quantile(x,3/4) - quantile(x,1/4)}.

For normally \eqn{N(m,1)} distributed \eqn{X}, the expected value of
\code{IQR(X)} is \code{2*qnorm(3/4) = 1.3490}, i.e., for a normal-consistent
estimate of the standard deviation, use \code{IQR(x) / 1.349}.
}
\references{
Tukey, J. W. (1977).
\emph{Exploratory Data Analysis}.
Reading: Addison-Wesley.
}
\seealso{
 \code{\link{fivenum}}, \code{\link{mad}} which is more robust,
 \code{\link{range}}, \code{\link{quantile}}.
}
\examples{
data(rivers)
IQR(rivers)
}
\keyword{univar}
\keyword{robust}
\keyword{distribution}

