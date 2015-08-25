% File src/library/base/man/typeof.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{typeof}
\alias{typeof}
\alias{type}
\title{The Type of an Object}
\description{
  \code{typeof} determines the (\R internal)
  type or storage mode of any object
}
\usage{
typeof(x)
}
\arguments{
  \item{x}{any \R object.}
}
\value{
  A character string.  The possible values are listed in the structure
  \code{TypeTable} in \file{src/main/util.c}. Current values are
  the vector types \code{"logical"}, \code{"integer"}, \code{"double"},
  \code{"complex"}, \code{"character"}, \code{"raw"} and \code{"list"},
  \code{"NULL"},
  \code{"closure"} (function), \code{"special"} and \code{"builtin"}
  (basic functions and operators), \code{"environment"}, \code{"S4"}
  (some S4 objects) and others that are unlikely to be seen at user
  level (\code{"symbol"}, \code{"pairlist"}, \code{"promise"},
  \code{"language"}, \code{"char"}, \code{"..."}, \code{"any"},
  \code{"expression"}, \code{"externalptr"}, \code{"bytecode"} and
  \code{"weakref"}).
}
\seealso{
  \code{\link{mode}}, \code{\link{storage.mode}}.

  \code{\link{isS4}} to determine if an object has an S4 class.
}
\examples{
typeof(2)
mode(2)
}
\keyword{attribute}
