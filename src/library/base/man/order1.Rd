% File src/library/base/man/order.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{order1}
\alias{order1}
\alias{order1.default}
\title{Find the Ordering Permuation for a Single Object}
\description{
  Find the permutation of the indices that will sort a single
  object. This is like \code{\link{sort.list}} but generic so methods
  can be written for it.
}
\usage{
order1(x, ...)
}
\arguments{
  \item{x}{an \R object, including vectors but not lists.}
  \item{\dots}{Futher arguments to be passed to or from methods.  For
    the default method, any of the arguments allowed for
    \code{\link{sort.list}}.}
}
\details{
  The default method is just \code{\link{sort.list}}.  Methods should if
  possible support arguments \code{na.last} and \code{decreasing}.
}
\value{
  An integer vector \code{ind} such that \code{x[ind]} is sorted.
}

\seealso{
  \code{\link{sort.list}}, \code{\link{order}}.
}

\keyword{univar}
\keyword{manip}
