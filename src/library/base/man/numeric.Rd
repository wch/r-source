\name{numeric}
\title{Numeric Vectors}
\usage{
numeric(n = 0)
as.numeric(x)
is.numeric(x)
}
\alias{numeric}
\alias{as.numeric}
\alias{is.numeric}
\description{
  \code{numeric} creates a real vector of length \code{n}.  The vector
  is filled with zeros.

  \code{as.numeric} attempts to coerce its argument to numeric type
  (either integer or real).

  \code{is.numeric} returns \code{TRUE} if its argument is of type real
  or type integer and \code{FALSE} otherwise.
}
\note{
  \emph{\R has no single precision data type.  All real numbers are
    stored in double precision format}.
}
\keyword{classes}
\keyword{attribute}
