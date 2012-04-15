% File src/library/grid/man/grid.grill.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.grill}
\alias{grid.grill}
\title{Draw a Grill}
\description{
  This function draws a grill within a Grid viewport.
}
\usage{
grid.grill(h = unit(seq(0.25, 0.75, 0.25), "npc"),
           v = unit(seq(0.25, 0.75, 0.25), "npc"),
           default.units = "npc", gp=gpar(col = "grey"), vp = NULL)
}
\arguments{
  \item{h}{A numeric vector or unit object
    indicating the horizontal location of the
    vertical grill lines.}
  \item{v}{A numeric vector or unit object
    indicating the vertical location of the
    horizontal grill lines.}
  \item{default.units}{A string indicating the default units to use
    if \code{h} or \code{v} are only given as numeric vectors.}
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object.}
}
\value{
  None.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}.
}
\keyword{dplot}
