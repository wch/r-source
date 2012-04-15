% File src/library/grid/man/grid.DLapply.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.DLapply}
\alias{grid.DLapply}
\title{ Modify the Grid Display List }
\description{
  Call a function on each element of the current display list
  and replace the element with the result.
}
\usage{
grid.DLapply(FUN, ...)
}
\arguments{
  \item{FUN}{A function.}
  \item{\dots}{Further arguments to pass to \code{FUN} .}
}
\value{
  The side effect of this function is to modify the grid display list.
}
\details{
  This is an insanely dangerous function (for the grid display list).

  Two token efforts are made to try to avoid ending up with complete
  garbage on the display list:

  \enumerate{
    \item The display list is only replaced once all new elements have
    been generated (so an error during generation does not result in a
    half-finished display list).
    \item All new elements must be either \code{NULL} or inherit from
    the class of the element that they are replacing.
  }
}
\seealso{
  \link{Grid}.
}
\examples{
grid.newpage()
grid.rect(width=.5, height=.5, gp=gpar(fill="grey"))
grid.DLapply(function(x) { if (is.grob(x)) x$gp <- gpar(); x })
grid.refresh()
}
\keyword{dplot}
