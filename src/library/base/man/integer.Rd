\name{integer}
\title{Integer Vectors}
\alias{integer}
\alias{as.integer}
\alias{is.integer}
\usage{
integer(length = 0)
as.integer(x)
is.integer(x)
}
\value{
  This function creates a integer vector of the specified length.
  Each element of the vector is equal to \code{0}.
  Integer vectors exist so that data can be passed to C or Fortran code
  which expects them.

  \code{as.integer} attempts to coerce its argument to be of integer
  type.

  \code{is.integer} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of integer type or not. 
}
\keyword{classes}
