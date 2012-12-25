% File src/library/grid/man/grid.points.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{grid.points}
\alias{grid.points}
\alias{pointsGrob}
\title{Draw Data Symbols}
\description{
  These functions create and draw data symbols.
}
\usage{
grid.points(x = stats::runif(10),
            y = stats::runif(10),
            pch = 1, size = unit(1, "char"),
            default.units = "native", name = NULL,
            gp = gpar(), draw = TRUE, vp = NULL)
pointsGrob(x = stats::runif(10),
           y = stats::runif(10),
           pch = 1, size = unit(1, "char"),
           default.units = "native", name = NULL,
           gp = gpar(), vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-values.}
  \item{y}{A numeric vector or unit object specifying y-values.}
  \item{pch}{A numeric or character vector indicating what sort of
    plotting symbol to use.  See \code{\link{points}} for the
    interpretation of these values.}
  \item{size}{A unit object specifying the size of the plotting symbols.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y} are only given as numeric vectors.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\details{
  Both functions create a points grob (a graphical object describing
  points), but only \code{grid.points} draws the points (and then only
  if \code{draw} is \code{TRUE}).
}
\value{
  A points grob.  \code{grid.points} returns the value invisibly.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\keyword{dplot}
