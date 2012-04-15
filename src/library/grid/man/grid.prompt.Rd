% File src/library/grid/man/grid.prompt.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{grid.prompt}
\alias{grid.prompt}
\title{ Prompt before New Page }
\description{
  This function can be used to control whether the user
  is prompted before starting a new page of output.
}
\usage{
grid.prompt(ask)
}
\arguments{
  \item{ask}{ a logical value.  If \code{TRUE}, the user
    is prompted before a new page of output is started. }
}
\details{
  Yhis is deprecated in favour of \code{\link{devAskNewPage}} as a
  single setting inside the device affects both the base and grid
  graphics systems.
  
  The default value when a device is opened is taken from the
  setting of \code{\link{options}("device.ask.default")}.
}
\value{
  The current prompt setting \emph{before} any new setting is applied.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.newpage}} }
\keyword{ dplot }
