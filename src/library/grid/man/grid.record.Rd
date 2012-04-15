% File src/library/grid/man/grid.record.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.record}
\alias{grid.record}
\alias{recordGrob}
\title{ Encapsulate calculations and drawing }
\description{
  Evaluates an expression that includes both calculations
  and drawing that depends on the calculations so that
  both the calculations and the drawing will be rerun when
  the scene is redrawn (e.g., device resize or editing).

  Intended \emph{only} for expert use.
}
\usage{
recordGrob(expr, list, name=NULL, gp=NULL, vp=NULL)
grid.record(expr, list, name=NULL, gp=NULL, vp=NULL)
}
\arguments{
  \item{expr}{object of mode \code{\link{expression}} or \code{call} or
    an unevaluated expression.}
  \item{list}{a list defining the environment in which \code{expr} is to
    be evaluated.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\details{
  A grob is created of special class \code{"recordedGrob"}
  (and drawn, in the case of \code{grid.record}).
  The \code{drawDetails} method for this class
  evaluates the expression with the list as the evaluation
  environment (and the grid Namespace as the parent of that
  environment).
}
\author{ Paul Murrell }
\note{ This function \emph{must} be used instead of the
  function \code{recordGraphics};  all of the dire warnings
  about using \code{recordGraphics} responsibly also apply here.
}
\seealso{ \code{\link{recordGraphics}} }
\examples{
grid.record({
              w <- convertWidth(unit(1, "inches"), "npc")
              grid.rect(width=w)
            },
            list())
}
\keyword{ dplot }
