\name{grid.yaxis}
\alias{grid.yaxis}
\alias{yaxisGrob}
\title{Draw a Y-Axis}
\description{
  These functions create and draw a y-axis.
}
\usage{
grid.yaxis(at = NULL, label = TRUE, main = TRUE, name = NULL,
           gp = gpar(), draw = TRUE, vp = NULL)

yaxisGrob(at = NULL, label = TRUE, main = TRUE, name = NULL,
          gp = gpar(), vp = NULL)
}
\arguments{
  \item{at}{A numeric vector of y-value locations for the tick marks.}
  \item{label}{A logical value indicating whether to draw the labels
    on the tick marks.}
  \item{main}{A logical value indicating whether to draw the axis
    at the left (\code{TRUE}) or at the right (\code{FALSE}) of the
    viewport.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport obect (or \code{NULL}).}
}
\details{
  Both functions create a yaxis grob (a graphical object describing a
  yaxis), but only \code{grid.yaxis}
  draws the yaxis (and then only if \code{draw} is \code{TRUE}).
  
}
\value{
  A yaxis grob.  \code{grid.yaxis} returns the value invisibly.
}
\section{Children}{
  If the \code{at} slot of an xaxis grob is not \code{NULL} then
  the xaxis will have the following children:
  \describe{
    \item{major}{ representing the line at the base of the
    tick marks.}
    \item{ticks}{representing the tick marks.}
    \item{labels}{representing the tick labels.}
  }

  If the \code{at} slot is \code{NULL} then there are no children
  and ticks are drawn based on the current viewport scale.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}},
  \code{\link{grid.xaxis}}
}
\keyword{dplot}
