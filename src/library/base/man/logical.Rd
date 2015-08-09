% File src/library/base/man/logical.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{logical}
\alias{logical}
\alias{as.logical}
\alias{as.logical.factor}
\alias{is.logical}
\alias{TRUE}
\alias{FALSE}
\alias{T}
\alias{F}
\title{Logical Vectors}
\description{
  Create or test for objects of type \code{"logical"}, and the basic
  logical constants.
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
  \code{TRUE} and \code{FALSE} are \link{reserved} words denoting logical
  constants in the \R language, whereas \code{T} and \code{F} are global
  variables whose initial values set to these.  All four are
  \code{logical(1)} vectors.

  Logical vectors are coerced to integer vectors in contexts where a
  numerical value is required, with \code{TRUE} being mapped to
  \code{1L}, \code{FALSE} to \code{0L} and \code{NA} to \code{NA_integer_}.
}
\arguments{
  \item{length}{A non-negative integer specifying the desired length.
    Double values will be coerced to integer:
    supplying an argument of length other than one is an error.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  \code{logical} creates a logical vector of the specified length.
  Each element of the vector is equal to \code{FALSE}.

  \code{as.logical} attempts to coerce its argument to be of logical
  type.  For \code{\link{factor}}s, this uses the \code{\link{levels}}
  (labels).  Like \code{\link{as.vector}} it strips attributes including
  names.  Character strings \code{c("T", "TRUE", "True", "true")} are
  regarded as true, \code{c("F", "FALSE", "False", "false")} as false,
  and all others as \code{NA}.

  \code{is.logical} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of logical type or not.
}
\seealso{
  \code{\link{NA}}, the other logical constant.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\keyword{classes}
\keyword{logic}
