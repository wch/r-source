\name{logical}
\alias{logical}
\alias{as.logical}
\alias{as.logical.default}
\alias{is.logical}
\alias{TRUE}
\alias{FALSE}
\alias{T}
\alias{F}
\title{Logical Vectors}
\description{
  Create or test for objects of type \code{"logical"}, and the basic
  logical \dQuote{constants}.
}
\usage{
TRUE
FALSE
T; F

logical(length = 0)
as.logical(x, \dots)
is.logical(x)
}
\details{
  \code{TRUE} and \code{FALSE} are part of the \R language, where
  \code{T} and \code{F} are global variables set to these.
  All four are \code{logical(1)} vectors.

  \code{is.logical} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
}
\arguments{
  \item{length}{desired length.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  \code{logical} creates a logical vector of the specified length.
  Each element of the vector is equal to \code{FALSE}.

  \code{as.logical} attempts to coerce its argument to be of logical
  type.  For \code{\link{factor}}s, this uses the \code{\link{levels}}
  (labels).  Like \code{\link{as.vector}} it strips attributes including
  names.

  \code{is.logical} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of logical type or not.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\keyword{classes}
\keyword{logic}
