\name{grid.xaxis}
\alias{grid.xaxis}
\alias{xaxisGrob}
\title{Draw an X-Axis}
\description{
  These functions create and draw an x-axis.
}
\usage{
grid.xaxis(at = NULL, label = TRUE, main = TRUE, name = NULL,
           gp = gpar(), draw = TRUE, vp = NULL)

xaxisGrob(at = NULL, label = TRUE, main = TRUE, name = NULL,
          gp = gpar(), vp = NULL)
}
\arguments{
  \item{at}{A numeric vector of x-value locations for the tick marks.}
  \item{label}{A logical value indicating whether to draw the labels
    on the tick marks.}
  \item{main}{A logical value indicating whether to draw the axis
    at the bottom (\code{TRUE}) or at the top (\code{FALSE}) of the
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
  Both functions create an xaxis grob (a graphical object describing an
  xaxis), but only \code{grid.xaxis}
  draws the xaxis (and then only if \code{draw} is \code{TRUE}).
  
}
\value{
  An xaxis grob.  \code{grid.xaxis} returns the value invisibly.
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
  \code{\link{grid.yaxis}}
}
\keyword{dplot}
