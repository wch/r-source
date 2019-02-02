% File src/library/grid/man/drawDetails.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{drawDetails}
\alias{drawDetails}
\alias{preDrawDetails}
\alias{postDrawDetails}
\alias{preDraw}
\alias{postDraw}
\title{ Customising grid Drawing }
\description{
  These generic hook functions are called whenever a grid grob is drawn.
  They provide an opportunity for customising the drawing of a
  new class derived from grob (or gTree).
}
\usage{
drawDetails(x, recording)
preDrawDetails(x)
postDrawDetails(x)
preDraw(x)
postDraw(x)
}
\arguments{
  \item{x}{ A grid grob. }
  \item{recording}{ A logical value indicating whether a grob
  is being added to the display list or redrawn from the display list. }
}
\details{
  These functions are called by the \code{grid.draw} methods for grobs
  and gTrees.

  \code{preDrawDetails} is called first during the drawing of a grob.
  This is where any additional viewports should be pushed (see,
  for example, \code{grid:::preDrawDetails.frame}).  Note that
  the default behaviour for grobs is to push any viewports in the
  \code{vp} slot, and for gTrees is to also push and up any viewports in the
  \code{childrenvp} slot so there is typically nothing to do here.

  \code{drawDetails} is called next and is where any additional
  calculations and graphical output should occur (see, for example,
  \code{grid:::drawDetails.xaxis}).  Note that the default behaviour
  for gTrees is to draw all grobs in the \code{children} slot
  so there is typically nothing to do here.

  \code{postDrawDetails} is called last and should reverse anything
  done in \code{preDrawDetails} (i.e., pop or up any viewports that were
  pushed;  again, see,
  for example, \code{grid:::postDrawDetails.frame}).  Note that the
  default behaviour for grobs is to pop any viewports that were pushed
  so there is typically nothing to do here.

  Note that \code{preDrawDetails} and \code{postDrawDetails} are
  also called in the calculation of \code{"grobwidth"} and
  \code{"grobheight"} units.
 
  The functions \code{preDraw} and \code{postDraw} are what call
  \code{preDrawDetails} and \code{postDrawDetails} respectively.
  These functions are exported so that external packages can 
  write functions that work with grobs and perform the same
  set up as usually happens when a grob is drawn.  Note that the
  result of calling the \code{preDraw} function is a potentially
  modified copy of the grob, so \code{preDraw} should be used like
  this: \code{x <- preDraw(x)}. 
}
\value{
  None of these functions are expected to return a value.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.draw}} }
\keyword{ dplot }
