\name{mad}
\title{Median Absolute Deviation}
\usage{
mad(x, center, constant=1.4826, na.rm=FALSE)
}
\alias{mad}
\description{
\code{mad} provides a scale estimate based on the
median absolute deviation.
The actual value calculated is
\code{constant * (median(abs(x - center)))}
with the default value of
\code{center} being \code{median(x)}.

The default \code{constant = 1.4826} is \eqn{\approx \Phi^{-1}(\frac 3 4)}= 1.4826 ~=ensures consistency, i.e.,\cr
\eqn{E[\code{mad}(X_1,\dots,X_n)] = \sigma} for
\eqn{X_i} distributed as \eqn{N(\mu,\sigma^2)} and large \eqn{n}.

If \code{na.rm} is \code{TRUE} then \code{NA}
values are stripped from \code{x} before computation takes place.
If this is not done then an \code{NA} value in
\code{x} will cause \code{mad} to return \code{NA}.
}
\seealso{
\code{\link{median}}, \code{\link{var}}.
}
\examples{
mad(c(1:9))
print(mad(c(1:9),     constant=1)) ==
      mad(c(1:8,100), constant=1)	# = 2 ; TRUE
}
\keyword{univar}
\keyword{robust}

