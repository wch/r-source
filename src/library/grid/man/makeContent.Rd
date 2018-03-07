% File src/library/grid/man/makeContent.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{makeContent}
\alias{makeContent}
\alias{makeContext}
\title{ Customised grid Grobs }
\description{
  These generic hook functions are called whenever a grid grob is drawn.
  They provide an opportunity for customising the drawing context
  and drawing content of a
  new class derived from grob (or gTree).
}
\usage{
makeContext(x)
makeContent(x)
}
\arguments{
  \item{x}{ A grid grob. }
}
\details{
  These functions are called by the \code{grid.draw} methods for grobs
  and gTrees.

  \code{makeContext} is called first during the drawing of a grob.
  This function should be used to \emph{modify} the \code{vp} slot
  of \code{x} (and/or the \code{childrenvp} slot if \code{x}
  is a gTree).  The function \emph{must} return the modified \code{x}.
  Note that
  the default behaviour for grobs is to push any viewports in the
  \code{vp} slot, and for gTrees is to also push and up any viewports in the
  \code{childrenvp} slot, so this function is used to customise the
  drawing context for a grob or gTree.

  \code{makeContent} is called next and is where any additional
  calculations should occur and graphical content should
  be generated (see, for example,
  \code{grid:::makeContent.xaxis}).
  This function should be used to \emph{modify} the \code{children}
  of a gTree.
  The function \emph{must} return the modified \code{x}.
  Note that the default behaviour
  for gTrees is to draw all grobs in the \code{children} slot,
  so this function is used to customise the drawing content for a gTree.
  It is also possible to customise the drawing content for a simple
  grob, but more care needs to be taken;  for example, the function
  should return a standard grid primitive with a \code{drawDetails()}
  method in this case.

  Note that these functions should be \emph{cumulative} in their
  effects, so that the \code{x} returned by \code{makeContent()}
  \emph{includes} any changes made by \code{makeContext()}.

  Note that \code{makeContext} is
  also called in the calculation of \code{"grobwidth"} and
  \code{"grobheight"} units.
}
\value{
  Both functions are expected to return a grob or gTree
  (a modified version of \code{x}).
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.draw}} }
\keyword{ dplot }
