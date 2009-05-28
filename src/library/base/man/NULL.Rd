% File src/library/base/man/NULL.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
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
\code{NULL} represents the null object in R: it is a \link{reserved} word.
NULL is often returned by expressions and functions whose
value is undefined: it is also used as the empty \link{pairlist}.

\code{as.null} ignores its argument and returns the value \code{NULL}.

\code{is.null} returns \code{TRUE} if its argument
is \code{NULL} and \code{FALSE} otherwise.
}
\arguments{
  \item{x}{an object to be tested or coerced.}
  \item{\dots}{ignored.}
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
is.null(list())    # FALSE (on purpose!)
is.null(integer(0))# F
is.null(logical(0))# F
as.null(list(a=1,b='c'))
}
\keyword{attribute}
\keyword{manip}
\keyword{list}
\keyword{sysdata}
