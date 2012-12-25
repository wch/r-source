% File src/library/grid/man/grid.force.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.force}
\alias{grid.force}
\alias{grid.revert}
\title{ Force a grob into its components }
\description{
  Some grobs only generate their content to draw at
  drawing time;  this function replaces such grobs with
  their at-drawing-time content.
}
\usage{
grid.force(redraw = TRUE)
grid.revert(redraw = TRUE)
}
\arguments{
  \item{redraw}{logical value indicating whether to redraw the
    \pkg{grid} scene after the forcing operation.}
}
\details{
  Some grobs wait until drawing time to generate what content
  will actually be drawn (an axis, as produced by \code{grid.xaxis()},
  with an \code{at} or \code{NULL} is a good example because it
  has to see what viewport it is going to be drawn in before
  it can decide what tick marks to draw).

  The content of such grobs (e.g., the tick marks) are not usually
  visible to \code{grid.ls()} or accessible to \code{grid.edit()}.

  The \code{grid.force()} function \emph{replaces} a grob with its
  at-drawing-time contents.  For example, an axis will be
  replaced by a vanilla gTree with lines and text representing
  the axis tick marks that were actually drawn.  This makes
  the tick marks
  visible to \code{grid.ls()} and accessible to \code{grid.edit()}.

  The \code{grid.revert()} function reverses the effect of
  \code{grid.force()}, replacing forced content with the original
  grob.
}
\section{Note}{
  These functions only have an effect for grobs that generate their content
  at drawing time using \code{makeContext()} and \code{makeContent()}
  methods (\emph{not} for grobs that generate their content
  at drawing time using \code{preDrawDetails()} and
  \code{drawDetails()} methods).
}
\author{ Paul Murrell }
\examples{
grid.newpage()
pushViewport(viewport(width=.5, height=.5))
grid.xaxis()
grid.ls()
grid.force()
grid.ls()
grid.revert()
grid.ls()
}
\keyword{ dplot }
