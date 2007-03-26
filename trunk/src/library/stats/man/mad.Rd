\name{mad}
\title{Median Absolute Deviation}
\usage{
mad(x, center = median(x), constant = 1.4826, na.rm = FALSE,
    low = FALSE, high = FALSE)
}
\alias{mad}
\arguments{
  \item{x}{a numeric vector.}
  \item{center}{Optionally, the centre: defaults to the median.}
  \item{constant}{scale factor.}
  \item{na.rm}{if \code{TRUE} then \code{NA} values are stripped
    from \code{x} before computation takes place.}
  \item{low}{if \code{TRUE}, compute the \dQuote{lo-median}, i.e., for even
    sample size, do not average the two middle values, but take the
    smaller one.}
  \item{high}{if \code{TRUE}, compute the \dQuote{hi-median}, i.e., take the
    larger of the two middle values for even sample size.}
}
\description{
  Compute the median absolute deviation, i.e., the (lo-/hi-) median of
  the absolute deviations from the median, and (by default) adjust by a
  factor for asymptotically normal consistency.
}
\details{
  The actual value calculated is \code{constant * cMedian(abs(x - center))}
  with the default value of \code{center} being \code{median(x)}, and
  \code{cMedian} being the usual, the \dQuote{low} or \dQuote{high} median, see
  the arguments description for \code{low} and \code{high} above.

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
      mad(c(1:8,100), constant=1)       # = 2 ; TRUE
x <- c(1,2,3, 5,7,8)
sort(abs(x - median(x)))
c(mad(x, co=1), mad(x, co=1, lo = TRUE), mad(x, co=1, hi = TRUE))
}
\keyword{univar}
\keyword{robust}
