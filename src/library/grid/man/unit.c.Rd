% File src/library/grid/man/unit.c.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{unit.c}
\alias{unit.c}
\title{Combine Unit Objects}
\description{
  This function produces a new unit object by combining the
  unit objects specified as arguments.
}
\usage{
unit.c(..., check = TRUE)
}
\arguments{
  \item{\dots}{An arbitrary number of unit objects.}
  \item{check}{Should input be checked? If you are certain all arguments are unit objects this can be set to \code{FALSE}}
}
\value{
  An object of class \code{unit}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{unit}}.
}
\keyword{dplot}
