% File src/library/base/man/cumsum.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{cumsum}
\alias{cumsum}
\alias{cumprod}
\alias{cummin}
\alias{cummax}
\title{Cumulative Sums, Products, and Extremes}
\description{
  Returns a vector whose elements are the cumulative sums, products,
  minima or maxima of the elements of the argument.
}
\usage{
cumsum(x)
cumprod(x)
cummax(x)
cummin(x)
}
\arguments{
  \item{x}{a numeric or complex (not \code{cummin} or \code{cummax})
    object, or an object that can be coerced to one of these.}
}
\details{
  These are generic functions: methods can be defined for them
  individually or via the \code{\link[=S3groupGeneric]{Math}} group generic.
}
\value{
  A vector of the same length and type as \code{x} (after coercion),
  except that \code{cumprod} returns a numeric vector for integer input
  (for consistency with \code{*}).  Names are preserved.

  An \code{NA} value in \code{x} causes the corresponding and following
  elements of the return value to be \code{NA}, as does integer overflow
  in \code{cumsum} (with a warning).
}
\section{S4 methods}{
  \code{cumsum} and \code{cumprod} are S4 generic functions:
  methods can be defined for them individually or via the
  \code{\link[=S4groupGeneric]{Math}} group generic.
  \code{cummax} and \code{cummin} are individually S4 generic functions.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole. (\code{cumsum} only.)
}
\examples{
cumsum(1:10)
cumprod(1:10)
cummin(c(3:1, 2:0, 4:2))
cummax(c(3:1, 2:0, 4:2))
}
\keyword{arith}
