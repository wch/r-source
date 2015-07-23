% File src/library/graphics/man/identify.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{identify}
\alias{identify}
\alias{identify.default}
\title{Identify Points in a Scatter Plot}
\usage{
identify(x, \dots)

\method{identify}{default}(x, y = NULL, labels = seq_along(x), pos = FALSE,
         n = length(x), plot = TRUE, atpen = FALSE, offset = 0.5,
         tolerance = 0.25, \dots)
}
\arguments{
  \item{x, y}{coordinates of points in a scatter plot.  Alternatively, any
    object which defines coordinates (a plotting structure, time
    series etc: see \code{\link{xy.coords}}) can be given as \code{x},
    and \code{y} left missing.}
  \item{labels}{an optional character vector giving labels for the
    points.  Will be coerced using \code{\link{as.character}}, and
    recycled if necessary to the length of \code{x}.  Excess labels will
    be discarded, with a warning.}
  \item{pos}{if \code{pos} is \code{TRUE}, a component is added to the
    return value which indicates where text was plotted relative to each
    identified point: see Value.}
  \item{n}{the maximum number of points to be identified.}
  \item{plot}{logical: if \code{plot} is \code{TRUE}, the labels are
    printed near the points and if \code{FALSE} they are omitted.}
  \item{atpen}{logical: if \code{TRUE} and \code{plot = TRUE}, the
    lower-left corners of the labels are plotted at the points clicked
    rather than relative to the points.}
  \item{offset}{the distance (in character widths) which separates the
    label from identified points.  Negative values are allowed.  Not
    used if \code{atpen = TRUE}.}
  \item{tolerance}{the maximal distance (in inches) for the pointer to be
    \sQuote{close enough} to a point.}
  \item{\dots}{further arguments passed to \code{\link{par}} such as
    \code{cex}, \code{col} and \code{font}.}
}
\description{
  \code{identify} reads the position of the graphics pointer when the
  (first) mouse button is pressed.  It then searches the coordinates
  given in \code{x} and \code{y} for the point closest to the pointer.
  If this point is close enough to the pointer, its index will be returned as
  part of the value of the call.
}
\details{
  \code{identify} is a generic function, and only the default method is
  described here.

  \code{identify} is only supported on screen devices such as
  \code{X11}, \code{windows} and \code{quartz}.  On other devices the
  call will do nothing.

  Clicking near (as defined by \code{tolerance}) a point adds it to the
  list of identified points.  Points can be identified only once, and if
  the point has already been identified or the click is not
  near any of the points a message is printed immediately on
  the \R console.

  If \code{plot} is \code{TRUE}, the point is labelled with the
  corresponding element of \code{labels}.  If \code{atpen} is false (the
  default) the labels are placed below, to the left, above or to the
  right of the identified point, depending on where the pointer was
  relative to the point.  If \code{atpen} is true, the
  labels are placed with the bottom left of the string's box at the
  pointer.

#ifdef unix
  For the usual \code{\link{X11}} device the identification process is
  terminated by pressing any mouse button other than the first.
  For the \code{\link{quartz}} device the process is terminated by
  pressing either the pop-up menu equivalent (usually second mouse
  button or \code{Ctrl}-click) or the \code{ESC} key.
#endif
#ifdef windows
  The identification process is terminated by clicking the second button
  and selecting \sQuote{Stop} from the menu, or from the \sQuote{Stop}
  menu on the graphics window.
#endif

  On most devices which support \code{identify}, successful selection of
  a point is indicated by a bell sound unless
  \code{\link{options}(locatorBell = FALSE)} has been set.

  If the window is resized or hidden and then exposed before the identification
  process has terminated, any labels drawn by \code{identify}
  will disappear.  These will reappear once the identification process has
  terminated and the window is resized or hidden and exposed again.
  This is because the labels drawn by \code{identify} are not
  recorded in the device's display list until the identification process has
  terminated.

  If you interrupt the \code{identify} call this leaves the graphics
  device in an undefined state, with points labelled but labels not
  recorded in the display list.  Copying a device in that state
#ifdef windows
  (e.g., saving the plot from the window's menu)
#endif
  will give unpredictable results.
}
\value{
  If \code{pos} is \code{FALSE}, an integer vector containing the
  indices of the identified points, in the order they were identified.

  If \code{pos} is \code{TRUE}, a list containing a component
  \code{ind}, indicating which points were identified and a component
  \code{pos}, indicating where the labels were placed relative to the
  identified points (1=below, 2=left, 3=above, 4=right and 0=no offset,
  used if \code{atpen = TRUE}).
}
\section{Technicalities}{
  The algorithm used for placing labels is the same as used by
  \code{text} if \code{pos} is specified there, the difference being
  that the position of the pointer relative the identified point
  determines \code{pos} in \code{identify}.

  For labels placed to the left of a point, the right-hand edge of the
  string's box is placed \code{offset} units to the left of the point,
  and analogously for points to the right.  The baseline of the text is
  placed below the point so as to approximately centre string vertically.
  For labels placed above or below a point, the string is centered
  horizontally on the point.  For labels placed above, the baseline of
  the text is placed \code{offset} units above the point, and
  for those placed below, the baseline is placed so that the top
  of the string's box is approximately \code{offset} units below the
  point.  If you want more precise placement (e.g., centering) use
  \code{plot = FALSE} and plot via \code{\link{text}} or
  \code{\link{points}}: see the examples.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{locator}}, \code{\link{text}}.

  \code{\link{dev.capabilities}} to see if it is supported.
}
\examples{
## A function to use identify to select points, and overplot the
## points with another symbol as they are selected
identifyPch <- function(x, y = NULL, n = length(x), pch = 19, ...)
{
    xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
    sel <- rep(FALSE, length(x)); res <- integer(0)
    while(sum(sel) < n) {
        ans <- identify(x[!sel], y[!sel], n = 1, plot = FALSE, ...)
        if(!length(ans)) break
        ans <- which(!sel)[ans]
        points(x[ans], y[ans], pch = pch)
        sel[ans] <- TRUE
        res <- c(res, ans)
    }
    res
}
}
\keyword{iplot}
