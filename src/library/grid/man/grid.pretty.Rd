% File src/library/grid/man/grid.pretty.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{grid.pretty}
\alias{grid.pretty}
\title{ Generate a Sensible ("Pretty") Set of Breakpoints }
\description{
  Produces a pretty set of approximately \code{n} breakpoints within the
  range given.

  This is a direct interface to \R's graphical engine \code{GEpretty()}
  function, which also underlies base \pkg{graphics}' package
  \code{\link{axis}()}, \code{\link{axTicks}()}, etc.
}
\usage{
grid.pretty(range, n = 5)
}
\arguments{
  \item{range}{a numeric vector of length at least two, as e.g., returned
    by \code{\link{range}()}.}
  \item{n}{a non-negative integer specifying the \emph{approximate} number
    of breakpoints to be produced.}
}
\value{
  A numeric vector of \dQuote{pretty} breakpoints.
}
\author{ Paul Murrell }
\keyword{dplot}
