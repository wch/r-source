% File src/library/base/man/NULL.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{NULL}
\title{The Null Object}
\usage{
NULL
as.null(x, \dots)
is.null(x)
}
\alias{NULL}
\alias{as.null}
\alias{as.null.default}
\alias{is.null}
\description{
  \code{NULL} represents the null object in \R: it is a \link{reserved}
  word.  \code{NULL} is often returned by expressions and functions
  whose value is undefined.

  \code{as.null} ignores its argument and returns the value \code{NULL}.

  \code{is.null} returns \code{TRUE} if its argument
  is \code{NULL} and \code{FALSE} otherwise.
}
\arguments{
  \item{x}{an object to be tested or coerced.}
  \item{\dots}{ignored.}
}
\details{
  \code{NULL} can be indexed (see \link{Extract}) in just about any
  syntactically legal way: whether is makes sense or not, the result is
  always \code{NULL}.  Objects with value \code{NULL} can be changed by
  replacement operators and will be coerced to the type of the
  right-hand side.

  \code{NULL} is also used as the empty \link{pairlist}.
}
\note{
  \code{is.null} is a \link{primitive} function.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
is.null(list())     # FALSE (on purpose!)
is.null(integer(0)) # FALSE
is.null(logical(0)) # FALSE
as.null(list(a = 1, b = "c"))
}
\keyword{attribute}
\keyword{manip}
\keyword{list}
\keyword{sysdata}
