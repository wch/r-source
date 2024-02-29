% File src/library/grid/man/drawDetails.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{drawDetails}
\alias{drawDetails}
\alias{preDrawDetails}
\alias{postDrawDetails}
\title{ Customising grid Drawing }
\description{
  These generic hook functions are called whenever a grid grob is drawn.
  They provide an opportunity for customising the drawing of a
  new class derived from grob (or \I{gTree}).
}
\usage{
drawDetails(x, recording)
preDrawDetails(x)
postDrawDetails(x)
}
\arguments{
  \item{x}{ A grid grob. }
  \item{recording}{ A logical value indicating whether a grob
  is being added to the display list or redrawn from the display list. }
}
\details{
  \strong{NOTE:} these functions have been largely superceded by the
  \code{\link{makeContent}} and \code{\link{makeContext}} functions,
  though they are still run and may still be useful in some contexts.

  These functions are called by the \code{grid.draw} methods for grobs
  and \I{gTree}s.

  \code{preDrawDetails} is called first during the drawing of a grob.
  This is where any additional viewports should be pushed.  Note that
  the default behaviour for grobs is to push any viewports in the
  \code{vp} slot, and for \I{gTree}s is to also push and up any viewports in the
  \code{childrenvp} slot so there is typically nothing to do here.

  \code{drawDetails} is called next and is where any additional
  calculations and graphical output should occur.
  Note that the default behaviour
  for \I{gTree}s is to draw all grobs in the \code{children} slot
  so there is typically nothing to do here.

  \code{postDrawDetails} is called last and should reverse anything
  done in \code{preDrawDetails} (i.e., pop or up any viewports that were
  pushed).  Note that the
  default behaviour for grobs is to pop any viewports that were pushed
  so there is typically nothing to do here.

  Note that \code{preDrawDetails} and \code{postDrawDetails} are
  also called in the calculation of \code{"grobwidth"} and
  \code{"grobheight"} units.
 }
\value{
  None of these functions are expected to return a value.
}
\author{ Paul Murrell }
\references{ "Changes to grid for R 3.0.0",
Paul Murrell, \emph{The R Journal} (2013) 5:2, pages 148-160. }
\seealso{ \code{\link{grid.draw}} and \code{\link{makeContent}} }
\keyword{ dplot }
