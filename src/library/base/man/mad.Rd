\name{mad}
\title{Median Absolute Deviation}
\usage{
mad(x, center, constant = 1.4826, na.rm = FALSE)
}
\alias{mad}
\arguments{
  \item{x}{a numeric vector.}
  \item{center}{Optionally, the centre: defauls to the median.}
  \item{constant}{scale factor.}
  \item{na.rm}{if \code{TRUE} then \code{NA} values are stripped
    from \code{x} before computation takes place.}
}
\description{
  Compute a scale estimate based on the median absolute deviation.
}
\details{
  The actual value calculated is
  \code{constant * (median(abs(x - center)))}
  with the default value of
  \code{center} being \code{median(x)}.

  The default \code{constant = 1.4826} (approximately
  \eqn{1/\Phi^{-1}(\frac 3 4)}{1/ Phi^(-1)(3/4)} = \code{1/qnorm(3/4)})
  ensures consistency, i.e.,
  \deqn{E[mad(X_1,\dots,X_n)] = \sigma}
  for \eqn{X_i} distributed as \eqn{N(\mu,\sigma^2)} and large \eqn{n}.

  If \code{na.rm} is \code{TRUE} then \code{NA}
  values are stripped from \code{x} before computation takes place.
  If this is not done then an \code{NA} value in
  \code{x} will cause \code{mad} to return \code{NA}.
}
\seealso{
  \code{\link{IQR}} which is simpler but less robust,
  \code{\link{median}}, \code{\link{var}}.
}
\examples{
mad(c(1:9))
print(mad(c(1:9),     constant=1)) ==
      mad(c(1:8,100), constant=1)	# = 2 ; TRUE
}
\keyword{univar}
\keyword{robust}

