\name{grid.lines}
\alias{grid.lines}
\title{Draw Lines in a Grid Viewport}
\description{
  This function draws a series of lines within a Grid viewport.
}
\usage{
grid.lines(x = unit(c(0, 1), "npc", units.per.obs),
       y = unit(c(0, 1), "npc", units.per.obs), 
       default.units = "npc", units.per.obs = FALSE,
       gp=gpar(), draw = TRUE, vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-values.}
  \item{y}{A numeric vector or unit object specifying y-values.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y} are only given as numeric vectors.}  
  \item{units.per.obs}{A logical value to indicate whether each
    individual (x, y) location has its own unit(s) specified.}
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\value{
  An object of class \code{"grob"}.
}
\details{
  The \code{"grob"} object contains an object of class \code{"lines"}.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\keyword{dplot}
