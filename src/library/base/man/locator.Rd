\name{locator}
\alias{locator}
\title{Graphical Input}
\description{
  Reads the position of the graphics cursor when the (first) mouse
  button is pressed.
}
\usage{
locator(n = 512, type = "n", \dots)
}
\arguments{
  \item{n}{the maximum number of points to locate.}
  \item{type}{One of \code{"n"}, \code{"p"}, \code{"l"} or
    \code{"o"}. If \code{"p"} or \code{"o"} the points are
    plotted; if \code{"l"}  or \code{"o"} they are joined by lines.}
  \item{\dots}{additional graphics parameters used if \code{type != "n"}
    for plotting the locations.}
}
\value{
  A list containing \code{x} and \code{y} components which are the
  coordinates of the identified points.
}
\details{
  Unless the process is terminated prematurely by the user (see below)
  at most \code{n} positions are determined.

  The input process can be terminated prematurely by pressing any mouse
  button other than the first.

  The current graphics parameters apply just as if \code{plot.default}
  has been called with the same value of \code{type}. The plotting of
  the points and lines is subject to clipping, but locations outside the
  current clipping rectangle will be returned.

  If the window is resized or hidden and then exposed before the input 
  process has terminated, any lines or points drawn by \code{locator}
  will disappear.  These will reappear once the input process has 
  terminated and the window is resized or hidden and exposed again.
  This is because the points and lines drawn by \code{locator} are not
  recorded in the device's display list until the input process has
  terminated.
}
\seealso{
  \code{\link{identify}}
}
\keyword{iplot}
