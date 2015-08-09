% File src/library/grid/man/grid.delay.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.delay}
\alias{grid.delay}
\alias{delayGrob}
\title{ Encapsulate calculations and generating a grob }
\description{
  Evaluates an expression that includes both calculations
  and generating a grob that depends on the calculations so that
  both the calculations and the grob generation will be rerun when
  the scene is redrawn (e.g., device resize or editing).

  Intended \emph{only} for expert use.
}
\usage{
delayGrob(expr, list, name=NULL, gp=NULL, vp=NULL)
grid.delay(expr, list, name=NULL, gp=NULL, vp=NULL)
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
  A grob is created of special class \code{"delayedgrob"}
  (and drawn, in the case of \code{grid.delay}).
  The \code{makeContent} method for this class
  evaluates the expression with the list as the evaluation
  environment (and the grid Namespace as the parent of that
  environment).

  The \code{expr} argument should return a grob as its result.

  These functions are analogues of the \code{grid.record()}
  and \code{recordGrob()} functions;  the difference is that
  these functions are based on the \code{makeContent()} hook,
  while those functions are based on the \code{drawDetails()}
  hook.
}
\author{ Paul Murrell }
\note{ This function \emph{must} be used instead of the
  function \code{recordGraphics};  all of the dire warnings
  about using \code{recordGraphics} responsibly also apply here.
}
\seealso{ \code{\link{recordGraphics}} }
\examples{
grid.delay({
              w <- convertWidth(unit(1, "inches"), "npc")
              rectGrob(width=w)
            },
            list())
}
\keyword{ dplot }
