\name{raw}
\alias{raw}
\alias{as.raw}
\title{Raw Vectors}
\description{
  Creates or tests for objects of type \code{"raw"}.
}
\usage{
raw(length = 0)
as.raw(x)
}
\arguments{
  \item{length}{desired length.}
  \item{x}{object to be coerced or tested.}
}
\value{
  \code{raw} creates a raw vector of the specified length.
  Each element of the vector is equal to \code{0}.
  Raw vectors are used to store fixed-length sequences of bytes.

  \code{as.raw} attempts to coerce its argument to be of raw
  type. The (elementwise) answer will be \code{0} unless the coercion succeeds.
}
\keyword{classes}
