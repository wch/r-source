% File src/library/graphics/man/locator.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

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
  \item{n}{the maximum number of points to locate.  Valid values start
    at 1.}
  \item{type}{One of \code{"n"}, \code{"p"}, \code{"l"} or
    \code{"o"}. If \code{"p"} or \code{"o"} the points are
    plotted; if \code{"l"}  or \code{"o"} they are joined by lines.}
  \item{\dots}{additional graphics parameters used if \code{type != "n"}
    for plotting the locations.}
}
\value{
  A list containing \code{x} and \code{y} components which are the
  coordinates of the identified points in the user coordinate system,
  i.e., the one specified by \code{\link{par}("usr")}.
}
\details{
  \code{locator} is only supported on screen devices such as
  \code{X11}, \code{windows} and \code{quartz}.  On other devices the
  call will do nothing.

  Unless the process is terminated prematurely by the user (see below)
  at most \code{n} positions are determined.

#ifdef unix
  For the usual \code{\link{X11}} device the identification process is
  terminated by pressing any mouse button other than the first.
  For the \code{\link{quartz}} device the process is terminated by
  pressing the \code{ESC} key.
#endif
#ifdef windows
  The identification process can be terminated by clicking the second
  button and selecting \sQuote{Stop} from the menu, or from the
  \sQuote{Stop} menu on the graphics window.
#endif

  The current graphics parameters apply just as if \code{plot.default}
  has been called with the same value of \code{type}. The plotting of
  the points and lines is subject to clipping, but locations outside the
  current clipping rectangle will be returned.

  On most devices which support \code{locator}, successful selection of
  a point is indicated by a bell sound unless
  \code{\link{options}(locatorBell = FALSE)} has been set.

  If the window is resized or hidden and then exposed before the input
  process has terminated, any lines or points drawn by \code{locator}
  will disappear.  These will reappear once the input process has
  terminated and the window is resized or hidden and exposed again.
  This is because the points and lines drawn by \code{locator} are not
  recorded in the device's display list until the input process has
  terminated.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{identify}}.

  \code{\link{dev.capabilities}} to see if it is supported.
}
\keyword{iplot}
