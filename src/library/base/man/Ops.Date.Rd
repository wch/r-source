% File src/library/base/man/Ops.Date.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{Ops.Date}
\alias{+.Date}
\alias{-.Date}
\alias{Ops.Date}

\title{Operators on the Date Class}
\description{
  Operators for the \code{"\link{Date}"} class.

  There is a method for the \code{\link[=S3groupGeneric]{Ops}} group
  generic handing the comparison operators and specific methods for
  \code{+} and \code{-}.
}
\usage{
\special{date + x}
\special{x + date}
\special{date - x}
\special{date1 lop date2}
}
\arguments{
  \item{date}{date objects}
  \item{date1, date2}{date objects.  One can be a character
    vector which will be converted by \code{\link{as.Date}}.}
  \item{x}{a numeric vector (in days) \emph{or} an object of class
    \code{"\link{difftime}"}, rounded to the nearest whole day.}
  \item{lop}{One of \code{==}, \code{!=}, \code{<}, \code{<=}, \code{>}
    or \code{>=}.}
}
\details{
  \code{x} does not need to be integer if specified as a numeric vector,
  but see the comments about fractional days in the help for
  \code{\link{Dates}}.
}
\examples{
(z <- Sys.Date())
z + 10
z < c("2009-06-01", "2010-01-01", "2015-01-01")
}
\keyword{utilities}
\keyword{chron}
