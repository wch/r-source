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
  These functions are the same as their \code{double} equivalents and
  are provided for backwards compatibility.
}
\arguments{
  \item{length}{desired length.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{as.real} is a generic function, but S3 methods must be
  written for \code{\link{as.double}}.  
}
\keyword{classes}
