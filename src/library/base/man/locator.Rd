\name{locator}
\title{Graphical Input}
\usage{
locator(n=512, type="n")
}
\alias{locator}
\description{
  \code{locator} reads the position of the graphics cursor when
  the (first) mouse button is pressed.
}
\details{
  Unless the process
  is terminated prematurely by the user (see below) at most
  \code{n} positions are determined.

  The input process can be terminated prematurely by
  pressing any mouse button other than the first.
}
\arguments{
  \item{n}{the maximum number of points to locate.}
  \item{type}{One of \code{"n"}, \code{"p"}, \code{"l"} or
    \code{"o"}. If \code{"p"} or \code{"o"} the points are
    plotted; if \code{"l"}  or \code{"o"} they are joined by lines.}
}
\details{
  The current graphics parameters apply just as if
  \code{plot.default} has been called with the same value
  of \code{type}. The plotting of the points and lines is subject to
  clipping, but locations outside the current clipping rectangle will
  be returned.
}
\value{
  A list containing \code{x} and \code{y} components which are
  the coordinates of the identified points.
}
\keyword{iplot}
