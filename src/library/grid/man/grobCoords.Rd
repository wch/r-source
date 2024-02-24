% File src/library/grid/man/grobCoords.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grobCoords}
\alias{grobCoords}
\alias{grobPoints}
\alias{isClosed}
\title{Calculate Points on the Perimeter of a Grob}
\description{
  These functions calculate points along the perimeter (or length) of
  a grob.
}
\usage{
grobCoords(x, closed, \dots)
grobPoints(x, closed, \dots)
isClosed(x, \dots)
}
\arguments{
  \item{x}{A grob object.}
  \item{closed}{Whether we are asking for points along the perimeter of
    a closed object or points along the length of an open object.
    Some grobs (e.g., X-splines) can do both.

    This defaults to \code{TRUE} except for known cases that are not
    closed (e.g., lines and segments).
  }
  \item{\dots}{Arguments to be used by methods.}
}
\details{
  The difference between \code{grobCoords} and \code{grobPoints}
  is that \code{grobCoords} performs all \I{pre-} and post-drawing operations
  on the grob that would normally occur if the grob was being drawn,
  then calls \code{grobPoints}.  So the former takes into account
  any \code{vp} and \code{gp} settings on the grob.  This means
  that users should usually only want to call \code{grobCoords};
  only (expert) developers may have a need to call \code{grobPoints}.
  
  Custom grobs can write their own methods for \code{grobPoints}
  (see \code{\link{gridCoords}}).

  The \code{isClosed} function returns \code{TRUE} or \code{FALSE}
  to indicate whether a grob is a closed shape.  The default response
  is \code{TRUE}, unless a method has been defined otherwise
  (e.g., for lines and line segments).
}
\value{
  Either a \code{"GridGrobCoords"} object
  (a list of lists with components \code{x} and \code{y}) or
  a \code{"GridGTreeCoords"} object
  (a list of \code{"GridGrobCoords"} and/or \code{"GridGTreeCoords"}
  objects).
  
  All locations are in inches relative to the current \pkg{grid} viewport.
}
\author{Paul Murrell}
\keyword{dplot}
