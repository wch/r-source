\name{grid.xaxis}
\alias{grid.xaxis}
\title{Draw an X-Axis on a Grid Viewport}
\description{
  This function draws an x-axis.
}
\usage{
grid.xaxis(at = NULL, label = TRUE, main = TRUE, gp = gpar(),
           draw = TRUE, vp = NULL)
}
\arguments{
  \item{at}{A numeric vector of x-value locations for the tick marks.}
  \item{label}{A logical value indicating whether to draw the labels
    on the tick marks.}
  \item{main}{A logical value indicating whether to draw the axis
    at the bottom (\code{TRUE}) or at the top (\code{FALSE}) of the
    viewport.}
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport obect (or \code{NULL}).}
}
\value{
  An object of class \code{"grob"}.
}
\details{
  The \code{"grob"} object contains an object of class \code{"xaxis"}.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}},
  \code{\link{grid.yaxis}}
}
\keyword{dplot}
