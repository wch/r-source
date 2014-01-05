% File src/library/grid/man/grid.force.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{grid.force}
\alias{grid.force}
\alias{grid.force.default}
\alias{grid.force.gPath}
\alias{grid.force.grob}
\alias{forceGrob}
\alias{grid.revert}
\alias{grid.revert.gPath}
\alias{grid.revert.grob}
\title{ Force a grob into its components }
\description{
  Some grobs only generate their content to draw at
  drawing time;  this function replaces such grobs with
  their at-drawing-time content.
}
\usage{
grid.force(x, ...)
\method{grid.force}{default}(x, redraw = FALSE, ...)
\method{grid.force}{gPath}(x, strict = FALSE, grep = FALSE, global = FALSE,
                           redraw = FALSE, ...) 
\method{grid.force}{grob}(x, draw = FALSE, ...)
forceGrob(x)
grid.revert(x, ...)
\method{grid.revert}{gPath}(x, strict = FALSE, grep = FALSE, global = FALSE,
                            redraw = FALSE, ...) 
\method{grid.revert}{grob}(x, draw = FALSE, ...)
}
\arguments{
  \item{x}{
    For the default method, \code{x} should not be specified.
    Otherwise, \code{x} should be a grob or a gPath.
    If \code{x} is character, it is assumed to be a gPath.
  }
  \item{strict}{
    A boolean indicating whether the \code{path} must be matched
    exactly.
  }
  \item{grep}{
    Whether the \code{path} should be treated as a regular expression.
  }
  \item{global}{
    A boolean indicating whether the function should affect just the
    first match of the \code{path}, or whether all matches should be
    affected.
  }
  \item{draw}{logical value indicating whether a grob should be
    drawn after it is forced.
  }
  \item{redraw}{logical value indicating whether to redraw the
    \pkg{grid} scene after the forcing operation.
  }
  \item{\dots}{ Further arguments for use by methods. }
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

  The \code{forceGrob()} function is the internal work horse for
  \code{grid.force()}, so will not normally be called directly by
  the user.  It is exported so that methods can be written for
  custom grob classes if necessary.
  
  The \code{grid.revert()} function reverses the effect of
  \code{grid.force()}, replacing forced content with the original
  grob.
}
\section{Warning}{
  Forcing an explicit grob produces a result as if the grob were
  drawn in the \emph{current} drawing context.
  It may not make sense to draw the result in a different
  drawing context.
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
# Draw xaxis
grid.xaxis(name="xax")
grid.ls()
# Force xaxis
grid.force()
grid.ls()
# Revert xaxis
grid.revert()
grid.ls()
# Draw and force yaxis
grid.force(yaxisGrob(), draw=TRUE)
grid.ls()
# Revert yaxis
grid.revert()
grid.ls()
# Force JUST xaxis
grid.force("xax")
grid.ls()
# Force ALL
grid.force()
grid.ls()
# Revert JUST xaxis
grid.revert("xax")
grid.ls()
}
\keyword{ dplot }
