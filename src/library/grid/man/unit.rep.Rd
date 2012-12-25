% File src/library/grid/man/unit.rep.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{unit.rep}
\alias{unit.rep}
\title{ Replicate Elements of Unit Objects }
\description{
  Replicates the units according to the values given in \code{times} and
  \code{length.out}.

  This function has been deprecated in favour of a unit method for
  the generic \code{rep} function.
}
\usage{
unit.rep(x, \dots)
}
\arguments{
  \item{x}{ An object of class \code{"unit"}. }
  \item{\dots}{arguments to be passed to \code{\link{rep}} such as
    \code{times} and \code{length.out}.}
}
\value{
  An object of class \code{"unit"}.
}
\author{ Paul Murrell }

\seealso{
  \code{\link{rep}}
}

\examples{
rep(unit(1:3, "npc"), 3)
rep(unit(1:3, "npc"), 1:3)
rep(unit(1:3, "npc") + unit(1, "inches"), 3)
rep(max(unit(1:3, "npc") + unit(1, "inches")), 3)
rep(max(unit(1:3, "npc") + unit(1, "strwidth", "a"))*4, 3)
rep(unit(1:3, "npc") + unit(1, "strwidth", "a")*4, 3)
}
\keyword{dplot}
