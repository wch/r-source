\name{real}
\title{Real Vectors}
\alias{real}
\alias{as.real}
\alias{is.real}
\usage{
real(length = 0)
as.real(x)
is.real(x)
}
\description{
  \code{real} creates a double precision vector of the specified
  length. Each element of the vector is equal to \code{0}.

  \code{as.real} attempts to coerce its argument to be of real type.

  \code{is.real} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of real type or not.
}
\note{
  \emph{\R has no single precision data type.
    All real numbers are stored in double precision format}.
}
\keyword{classes}
