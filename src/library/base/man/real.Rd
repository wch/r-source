\name{real}
\title{Real Vectors}
\alias{real}
\alias{as.real}
\alias{is.real}
\usage{
real(length = 0)
as.real(x, \dots)
is.real(x)
}
\description{
  \code{real} creates a double precision vector of the specified
  length. Each element of the vector is equal to \code{0}.

  \code{as.real} attempts to coerce its argument to be of double type.

  \code{is.real} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of double type or not.
}
\arguments{
  \item{length}{desired length.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\note{
  \emph{\R has no single precision data type.
    All real numbers are stored in double precision format}.
  These functions are the same as their \code{double} equivalents.
}
\keyword{classes}
