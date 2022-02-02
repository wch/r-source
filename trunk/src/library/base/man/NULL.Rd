% File src/library/base/man/NULL.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
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
}
\arguments{
  \item{x}{an object to be tested or coerced.}
  \item{\dots}{ignored.}
}
\details{
  \code{NULL} can be indexed (see \link{Extract}) in just about any
  syntactically legal way: whether it makes sense or not, the result is
  always \code{NULL}.  Objects with value \code{NULL} can be changed by
  replacement operators and will be coerced to the type of the
  right-hand side.

  \code{NULL} is also used as the empty \link{pairlist}: see the
  examples. Because pairlists are often promoted to lists, you may
  encounter \code{NULL} being promoted to an empty list.

  Objects with value \code{NULL} cannot have attributes as there is only
  one null object: attempts to assign them are either an error
  (\code{\link{attr}}) or promote the object to an empty list with
  attribute(s) (\code{\link{attributes}} and \code{\link{structure}}).
}
\note{
  \code{is.null} is a \link{primitive} function.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\value{
  \code{as.null} ignores its argument and returns \code{NULL}.

  \code{is.null} returns \code{TRUE} if its argument's value
  is \code{NULL} and \code{FALSE} otherwise.
}
\examples{
is.null(list())     # FALSE (on purpose!)
is.null(pairlist()) # TRUE
is.null(integer(0)) # FALSE
is.null(logical(0)) # FALSE
as.null(list(a = 1, b = "c"))
}
\keyword{attribute}
\keyword{manip}
\keyword{list}
\keyword{sysdata}
