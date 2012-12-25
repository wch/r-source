% File src/library/grid/man/grid.roundrect.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008 R Core Team
% Distributed under GPL 2 or later

\name{roundrect}
\alias{roundrect}
\alias{grid.roundrect}
\alias{roundrectGrob}
\title{Draw a rectangle with rounded corners}
\description{Draw a \emph{single} rectangle with rounded corners.}
\usage{
roundrectGrob(x=0.5, y=0.5, width=1, height=1,
              default.units="npc",
              r=unit(0.1, "snpc"),
              just="centre",
              name=NULL, gp=NULL, vp=NULL)
grid.roundrect(...)
}
\arguments{
  \item{x, y, width, height}{The location and size of the rectangle.}
  \item{default.units}{A string indicating the default units to use
    if \code{x}, \code{y}, \code{width}, or \code{height}
    are only given as numeric vectors.}
  \item{r}{The radius of the rounded corners.}
  \item{just}{The justification of the rectangle relative to its
location.}
  \item{name}{A name to identify the grob.}
  \item{gp}{Graphical parameters to apply to the grob.}
  \item{vp}{A viewport object or \code{NULL}.}
  \item{\dots}{Arguments to be passed to \code{roundrectGrob()}.}
}
\details{
  At present, this function can only be used to draw \emph{one}
  rounded rectangle.
}
\examples{
grid.roundrect(width=.5, height=.5, name="rr")
theta <- seq(0, 360, length=50)
for (i in 1:50)
    grid.circle(x=grobX("rr", theta[i]),
                y=grobY("rr", theta[i]),
                r=unit(1, "mm"),
                gp=gpar(fill="black"))
}
\keyword{dplot}
