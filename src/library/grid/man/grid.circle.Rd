\name{grid.circle}
\alias{grid.circle}
\title{ Draw a Circle }
\description{
  This function draws a circle.  
}
\usage{
grid.circle(x=0.5, y=0.5, r=0.5, default.units="npc",
            gp=gpar(), draw=TRUE, vp=NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-locations.}
  \item{y}{A numeric vector or unit object specifying y-locations.}
  \item{r}{A numeric vector or unit object specifying radii.}
  \item{default.units}{A string indicating the default units to use
    if \code{x}, \code{y}, \code{width}, or \code{height}
    are only given as numeric vectors.}  
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\details{
  The radius may be given in any
  units;  if the units are \emph{relative} (e.g., \code{"npc"} or
  \code{"native"}) then the radius will be different depending on
  whether it is interpreted as a width or as a height.  In such cases,
  the smaller of these two values will be the result.  To see the
  effect, type \code{grid.circle()} and adjust the size of the window.

  The \code{"grob"} object contains an object of class \code{"circle"}.
}
\value{
  An object of class \code{"grob"}.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\keyword{dplot}
