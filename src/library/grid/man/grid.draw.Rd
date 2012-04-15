% File src/library/grid/man/grid.draw.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.draw}
\alias{grid.draw}
\title{Draw a grid grob}
\description{
  Produces graphical output from a graphical object.
}
\usage{
grid.draw(x, recording=TRUE)
}
\arguments{
  \item{x}{An object of class \code{"grob"} or NULL.}
  \item{recording}{A logical value to indicate whether the drawing
    operation should be recorded on the Grid display list.}
}
\details{
  This is a generic function with methods for grob and gTree objects.

  The grob and gTree methods automatically push any viewports in a
  \code{vp} slot and automatically apply any \code{gpar} settings
  in a \code{gp} slot.  In addition, the gTree method pushes and
  ups any viewports in a \code{childrenvp} slot and automatically
  calls \code{grid.draw} for any grobs in a \code{children} slot.

  The methods for grob and gTree call the generic hook functions
  \code{preDrawDetails}, \code{drawDetails}, and \code{postDrawDetails}
  to allow classes derived from grob or gTree to perform
  additional viewport pushing/popping and produce additional
  output beyond the default behaviour for grobs and gTrees.
}
\value{
  None.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grob}}.
}

\examples{
grid.newpage()
## Create a graphical object, but don't draw it
l <- linesGrob()
## Draw it
grid.draw(l)
}
\keyword{dplot}
