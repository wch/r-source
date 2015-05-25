% File src/library/grid/man/grid.circle.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{grid.circle}
\alias{grid.circle}
\alias{circleGrob}
\title{ Draw a Circle }
\description{
  Functions to create and draw a circle.
}
\usage{
grid.circle(x=0.5, y=0.5, r=0.5, default.units="npc", name=NULL,
            gp=gpar(), draw=TRUE, vp=NULL)
circleGrob(x=0.5, y=0.5, r=0.5, default.units="npc", name=NULL,
            gp=gpar(), vp=NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-locations.}
  \item{y}{A numeric vector or unit object specifying y-locations.}
  \item{r}{A numeric vector or unit object specifying radii.}
  \item{default.units}{A string indicating the default units to use
    if \code{x}, \code{y}, \code{width}, or \code{height}
    are only given as numeric vectors.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\details{
  Both functions create a circle grob (a graphical object describing a
  circle), but only \code{grid.circle()}
  draws the circle (and then only if \code{draw} is \code{TRUE}).

  The radius may be given in any
  units;  if the units are \emph{relative} (e.g., \code{"npc"} or
  \code{"native"}) then the radius will be different depending on
  whether it is interpreted as a width or as a height.  In such cases,
  the smaller of these two values will be the result.  To see the
  effect, type \code{grid.circle()} and adjust the size of the window.

  What happens for very small radii is device-dependent: the
  circle may become invisible or be shown at a fixed minimum size.
  Circles of zero radius will not be plotted.
}
\section{Warning}{
  Negative values for the radius are silently converted to their
  absolute value.
  }
\value{
  A circle grob.  \code{grid.circle()} returns the value invisibly.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\keyword{dplot}
