% File src/library/grid/man/grobCoords.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grobCoords}
\alias{grobCoords}
\alias{grobPoints}
\title{Calculate Points on the Perimeter of a Grob}
\description{
  These functions calculate points along the perimeter (or length) of
  a grob.
}
\usage{
grobCoords(x, closed, \dots)
grobPoints(x, closed, \dots)
}
\arguments{
  \item{x}{A grob object.}
  \item{closed}{Whether we are asking for points along the perimeter of
    a closed object or points along the length of an open object.
    Some grobs (e.g., X-splines) can do both.}
  \item{\dots}{Arguments to be used by methods.}
}
\details{
  Custom grobs can write their own methods for \code{grobPoints}.
}
\value{
  A list of lists with components \code{x} and \code{y}.
  All locations are in inches relative to the current \pkg{grid} viewport.
}
\author{Paul Murrell}
\keyword{dplot}
