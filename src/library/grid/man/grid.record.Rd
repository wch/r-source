\name{grid.record}
\alias{grid.record}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Encapsulate calculations and drawing }
\description{
  Evaluates an expression that includes both calculatons
  and drawing that depends on the calculations so that
  both the calculations and the drawing will be rerun when
  the scene is redrawn (e.g., device resize or editing).

  Intended \emph{only} for expert use.
}
\usage{
grid.record(expr, list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{object of mode \code{\link{expression}} or \code{call} or
    an \dQuote{unevaluated expression}.}
  \item{list}{a list defining the environment in which \code{expr} is to
    be evaluated.}
}
\details{
  A grob is created of special class \code{"recordedGrob"}
  and drawn.  The \code{drawDetails} method for this class
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
\keyword{ dplot }% at least one, from doc/KEYWORDS
